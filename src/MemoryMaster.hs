{-# HLINT ignore "Use if" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MemoryMaster (run, memoryMasterApp) where

import Butler
import Butler.Auth (PageDesc (PageDesc), PageTitle (..))
import Butler.Database (Database, dbSimpleCreate, withDatabase)
import Data.Aeson (Value (Number))
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Char8 as C8 (unpack)
import MemoryMaster.Engine
import Prelude

memoryMasterApp :: Database -> App
memoryMasterApp db =
  let name = "Memory Master"
      tags = mempty
      title = "Memory Master"
      description = "MemoryMaster"
      size = Nothing
      xfiles = mempty
      start = startMM db
      acceptFiles = Nothing
   in App {..}

run :: IO ()
run =
  void $
    runMain $
      spawnInitProcess ".butler-storage" $
        withDatabase "leaderboard" migrations runApp
  where
    migrations = dbSimpleCreate "scores" "id INTEGER PRIMARY KEY, name TEXT, date DATE, duration REAL, level TEXT"
    runApp db =
      let app = memoryMasterApp db
       in serveApps (publicDisplayApp (PageTitle app.title) (Just $ PageDesc app.description)) [app]

startMM :: Database -> AppContext -> ProcessIO ()
startMM db ctx = do
  svgs <- liftIO loadSVG
  (board, collectionName) <- liftIO $ mkBoard svgs
  let appState =
        AppState
          { collectionName,
            board,
            gameState = Play $ Progress NoCardTurned,
            cardsToRender = mempty
          }
      memAddr = "memory-master-" <> showT ctx.wid <> ".bin"
  (_, appStateM) <- newProcessMemory (from memAddr) (pure appState)
  forever $ do
    res <- atomically (readPipe ctx.pipe)
    case res of
      AppDisplay _ -> do
        let app = renderApp ctx.wid svgs appStateM db
        sendHtmlOnConnect app res
      AppTrigger ev -> do
        case ev.trigger of
          TriggerName "clickMenu" -> do
            logInfo "Got <clickMenu> game event" []
          TriggerName "clickRestart" -> do
            logInfo "Got <clickRestart> game event" []
            (newBoard, _) <- liftIO $ mkBoard svgs
            atomically $ do
              modifyMemoryVar appStateM $ \s ->
                s
                  { board = newBoard,
                    cardsToRender = mempty,
                    gameState = Play $ Progress NoCardTurned
                  }
            sendsHtml ctx.clients $ renderBoard ctx.wid svgs appStateM
          TriggerName "clickCard" -> do
            logInfo "Got <clickCard> game event" ["body" .= ev.body]
            case ev.body ^? key "index" . _Integer of
              Just _index -> do
                logInfo "Here" []
                let index = fromInteger $ toInteger _index
                newState <- atomically $ do
                  modifyMemoryVar appStateM flipSuccFail
                  modifyMemoryVar appStateM (handleCardClick index)
                  readMemoryVar appStateM
                mapM_ (sendsHtml ctx.clients . renderCard ctx.wid svgs appStateM) newState.cardsToRender
              Nothing -> pure ()
          _ -> pure ()
        pure ()
      _ -> pure ()
  where
    handleCardClick :: CardId -> AppState -> AppState
    handleCardClick cardId appState =
      let newAppState = case appState.gameState of
            Play Wait -> appState {gameState = Play (Progress NoCardTurned)}
            Play (Progress NoCardTurned) ->
              appState
                { gameState = Play (Progress $ OneCardTurned cardId),
                  board = setCardStatus cardId TurnedWaitPair appState.board,
                  cardsToRender = appState.cardsToRender <> [cardId]
                }
            Play (Progress (OneCardTurned turnedCardId)) -> do
              let isPairTurned =
                    getCardName appState.board turnedCardId == getCardName appState.board cardId
                      && turnedCardId /= cardId
                  (board, cardsToRender)
                    | isPairTurned =
                        ( setCardStatus cardId TurnedMatchSucc $
                            setCardStatus turnedCardId TurnedMatchSucc appState.board,
                          [cardId, turnedCardId]
                        )
                    | cardId == turnedCardId =
                        ( setCardStatus cardId Closed appState.board,
                          [cardId]
                        )
                    | otherwise =
                        ( setCardStatus cardId TurnedMatchFail $
                            setCardStatus turnedCardId TurnedMatchFail appState.board,
                          [cardId, turnedCardId]
                        )
               in appState
                    { gameState = Play (Progress NoCardTurned),
                      board,
                      cardsToRender = appState.cardsToRender <> cardsToRender
                    }
            _ -> appState
       in trace (show newAppState) newAppState
    flipSuccFail :: AppState -> AppState
    flipSuccFail appState =
      let Board cards = appState.board
          newState = zipWith (curry flipState) [0 ..] cards
       in appState
            { board = Board $ map fst newState,
              cardsToRender = mapMaybe snd newState
            }
      where
        flipState :: (CardId, Card) -> (Card, Maybe CardId)
        flipState (cardId, card@Card {..}) =
          let (newCardStatus, cardToRender) = case cardStatus of
                TurnedMatchFail -> (Closed, Just cardId)
                TurnedMatchSucc -> (Turned, Just cardId)
                TurnedWaitPair -> (TurnedWaitPair, Nothing)
                Closed -> (Closed, Nothing)
                Turned -> (Turned, Nothing)
           in (card {cardStatus = newCardStatus}, cardToRender)

withEvent :: Monad m => WinID -> Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
withEvent wid tId tAttrs elm = with elm ([id_ (withWID wid tId), wsSend' ""] <> tAttrs)
  where
    wsSend' = makeAttribute "ws-send"

renderApp :: WinID -> SVGCollections -> MemoryVar AppState -> Database -> HtmlT STM ()
renderApp wid cols appStateM _db = do
  appState <- lift $ readMemoryVar appStateM
  div_ [id_ (withWID wid "w"), class_ "container mx-auto"] $ do
    div_ [id_ (withWID wid "w"), class_ "flex flex-row justify-center"] $ do
      div_ [id_ "MSMain", class_ "min-w-fit max-w-fit border-2 rounded border-gray-400 bg-gray-100"] $ do
        div_ [id_ "AppMain", class_ "flex flex-row justify-center"] $ do
          div_ [class_ "grow min-w-min max-w-screen-2xl"] $ do
            div_ [class_ "flex-col justify-between h-full"] $ do
              div_ [class_ "flex justify-around m-1"] $ do
                button "clickMenu" "Menu"
                button "clickRestart" "Restart"
              case appState.gameState of
                Play _ -> do
                  div_ [class_ ""] $ do
                    renderBoard wid cols appStateM
                -- renderLeaderBoard
                _ -> p_ "Menu"
              div_ [class_ "bg-indigo-100"] $ do
                p_ "Footer"
  where
    button :: Text -> Text -> HtmlT STM ()
    button evText text =
      withEvent wid evText []
        $ div_
          [ class_
              "px-1 cursor-pointer border-2 rounded border-gray-200 whitespace-nowrap"
          ]
        $ toHtml text

renderBoard :: WinID -> SVGCollections -> MemoryVar AppState -> HtmlT STM ()
renderBoard wid cols appStateV = do
  div_ [id_ "Board", class_ "flex flex-row flex-wrap"] $ do
    div_ [class_ "basis-1/2 min-w-fit grow bg-green-400"] $ do
      div_ [class_ "m-2 grid grid-flow-row-dense gap-2 grid-cols-6 grid-rows-3 justify-items-center"] $ do
        mapM_ (renderCard wid cols appStateV) [0 .. 23]

renderCard :: WinID -> SVGCollections -> MemoryVar AppState -> CardId -> HtmlT STM ()
renderCard wid cols appStateV cardId = do
  appState <- lift $ readMemoryVar appStateV
  div_ [id_ cardDivId] $ do
    case getCardByCardId appState.board cardId of
      Card _ Closed -> div_ [class_ "cursor-pointer"] $ do
        withEvent wid "clickCard" [cardIdHX] $
          div_ [class_ boxes_style] ""
      Card _ Turned -> do
        div_ [class_ boxes_style] $ img_ [src_ $ getSVGInline appState]
      Card _ TurnedMatchSucc -> do
        div_ [class_ boxes_style] $
          div_ [class_ "bg-green-300"] $
            img_ [src_ $ getSVGInline appState]
      Card _ TurnedMatchFail -> do
        div_ [class_ boxes_style] $
          div_ [class_ "bg-red-300 cursor-pointer"] $
            withEvent wid "clickCard" [cardIdHX] $
              img_ [src_ $ getSVGInline appState]
      Card _ TurnedWaitPair -> do
        div_ [class_ boxes_style] $
          div_ [class_ "bg-gray-100"] $
            img_ [src_ $ getSVGInline appState]
  where
    getSVGInline :: AppState -> Text
    getSVGInline appState =
      let svgName = getCardName appState.board cardId
          svg = fromMaybe (error "no svg") $ getSVGByName cols appState.collectionName svgName
          svgB64 = B64.encode svg
       in "data:image/svg+xml;base64," <> from (C8.unpack svgB64)
    boxes_style = "w-14 md:w-24 lg:w-32 h-14 md:h-24 lg:h-32 shadow shadow-black bg-yellow-100 border-2 rounded border-pink-100"
    cardIdHX = encodeVal [("index", Number $ fromInteger $ toInteger cardId)]
    cardDivId = from $ "Card" <> show cardId

-- renderLeaderBoard :: HtmlT STM ()
-- renderLeaderBoard = do
--   div_ [class_ "basis-1/2 min-w-min grow bg-pink-400"] $ do
--     p_ "LeaderBoard"
--     p_ "Elsa"
--     p_ "Fabien"