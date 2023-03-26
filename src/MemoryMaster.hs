{-# HLINT ignore "Use if" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MemoryMaster (run, memoryMasterApp) where

import Butler
import Butler.App (Display (..))
import Butler.Auth (PageDesc (PageDesc), PageTitle (..))
import Butler.Database (Database, NamedParam ((:=)), dbExecute, dbQuery, dbSimpleCreate, withDatabase)
import Butler.Display.Session (Session (..), UserName, changeUsername)
import Data.Aeson (Value (Number))
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Char8 as C8 (unpack)
import Data.Time (defaultTimeLocale, diffUTCTime, formatTime)
import qualified Database.SQLite.Simple as DB
import MemoryMaster.Engine
import Text.Printf (printf)
import qualified XStatic.Remixicon as XStatic
import Prelude

memoryMasterApp :: Database -> App
memoryMasterApp db =
  let name = "Memory Master"
      tags = mempty
      title = "Memory Master - Concentration game - Matching card game - Shinkei-suijaku - Pexeso - Pelmanism"
      description = "Memory Master is a mini card game where the goal is to match pair in a minimum amount of time."
      size = Nothing
      xfiles = [] <> XStatic.remixicon
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
    migrations =
      dbSimpleCreate
        "scores"
        "id INTEGER PRIMARY KEY, name TEXT, date DATE, duration REAL, clicks INTEGER, collection TEXT"
    runApp db =
      let app = memoryMasterApp db
       in serveApps (publicDisplayApp (PageTitle app.title) (Just $ PageDesc app.description)) [app]

data Score = Score
  { scoreId :: Int,
    scoreName :: Text,
    scoreDate :: UTCTime,
    scoreDuration :: Float,
    scoreClicks :: Int,
    scoreCollection :: Text
  }
  deriving (Show)

instance DB.FromRow Score where
  fromRow =
    Score
      <$> DB.field
      <*> DB.field
      <*> DB.field
      <*> DB.field
      <*> DB.field
      <*> DB.field

addScore :: Database -> UserName -> UTCTime -> Float -> Int -> Text -> IO ()
addScore db name date duration clicksCount collection =
  dbExecute
    db
    "INSERT INTO scores (name, date, duration, clicks, collection) VALUES (:name,:date,:duration,:clicks,:collection)"
    [ ":name" := into @Text name,
      ":date" := date,
      ":duration" := duration,
      ":clicks" := clicksCount,
      ":collection" := collection
    ]

getTopScores :: Database -> Integer -> Text -> IO [Score]
getTopScores db limit collection =
  dbQuery
    db
    "SELECT * from scores WHERE collection = :collection ORDER BY clicks ASC, duration ASC LIMIT :limit"
    [":collection" := collection, ":limit" := show limit]

diffTimeToFloat :: UTCTime -> UTCTime -> Float
diffTimeToFloat a b = realToFrac $ diffUTCTime a b

startMM :: Database -> AppContext -> ProcessIO ()
startMM db ctx = do
  svgs <- liftIO loadSVG
  (board, collectionName) <- liftIO $ mkBoard svgs
  let appState =
        AppState
          { collectionName,
            board,
            gameState = Play Wait,
            cardsToRender = mempty
          }
      memAddr = "memory-master-" <> showT ctx.wid <> ".bin"
  (_, appStateM) <- newProcessMemory (from memAddr) (pure appState)
  spawnThread_ $ asyncTimerUpdateThread appStateM ctx.shared.clients
  forever $ do
    res <- atomically (readPipe ctx.pipe)
    case res of
      AppDisplay _ -> do
        app <- liftIO $ renderApp ctx.wid svgs appStateM db
        sendHtmlOnConnect app res
      AppTrigger ev -> do
        username <- readTVarIO (ev.client.session.username)
        case ev.trigger of
          TriggerName "clickAccount" -> do
            logInfo "Got <clickAccount> game event" []
            sendsHtml ctx.shared.clients $ renderSetAccountForm ctx.wid username
          TriggerName "setAccount" -> do
            logInfo "Got <setAccount> game event" ["body" .= ev]
            case ev.body ^? key "playerName" . _JSON of
              Just playerName -> do
                success <- changeUsername ctx.shared.display.sessions ev.client.session playerName
                if success || username == playerName
                  then do
                    logInfo "setAcount success" ["username" .= playerName]
                    sendsHtml ctx.shared.clients $ div_ [id_ "SettingsAccount"] ""
                  else do
                    logInfo "setAcount failed" ["username" .= playerName]
                    sendsHtml ctx.shared.clients $ div_ [id_ "SettingsAccountNotice"] $ do
                      p_ [class_ "text-red-500"] "Username already used"
              Nothing -> pure ()
          TriggerName "clickRestart" -> do
            logInfo "Got <clickRestart> game event" []
            (newBoard, _) <- liftIO $ mkBoard svgs
            atomically $ do
              modifyMemoryVar appStateM $ \s ->
                s
                  { board = newBoard,
                    cardsToRender = mempty,
                    gameState = Play Wait
                  }
            sendsHtml ctx.shared.clients $ renderBoard ctx.wid svgs appStateM
            sendsHtml ctx.shared.clients $ renderPlayStatus appStateM
            sendsHtml ctx.shared.clients $ renderTimer 0.0
          TriggerName "clickCard" -> do
            logInfo "Got <clickCard> game event" ["body" .= ev.body]
            case ev.body ^? key "index" . _Integer of
              Just _index -> do
                now <- liftIO getCurrentTime
                newState <- atomically $ do
                  modifyMemoryVar appStateM flipSuccFail
                  modifyMemoryVar appStateM (handleCardClick (fromInteger $ toInteger _index) now)
                  readMemoryVar appStateM
                mapM_
                  (sendsHtml ctx.shared.clients . renderCard ctx.wid svgs appStateM)
                  newState.cardsToRender
                sendsHtml ctx.shared.clients $ renderPlayStatus appStateM
                case newState.gameState of
                  Play (Win start playDuration clicksCount) -> do
                    liftIO $ addScore db username start playDuration clicksCount ""
                    leaderBoard <- liftIO $ renderLeaderBoard db
                    sendsHtml ctx.shared.clients leaderBoard
                  _ -> pure ()
              Nothing -> pure ()
          _ -> pure ()
        pure ()
      _ -> pure ()
  where
    asyncTimerUpdateThread :: MemoryVar AppState -> DisplayClients -> ProcessIO Void
    asyncTimerUpdateThread appStateM clients = forever $ do
      appState <- atomically $ readMemoryVar appStateM
      case appState.gameState of
        Play (Progress startTime _ _) -> do
          diffT <- liftIO $ diffTime startTime
          sendsHtml clients $ renderTimer diffT
        _ -> pure ()
      sleep 990
    handleCardClick :: CardId -> UTCTime -> AppState -> AppState
    handleCardClick cardId clickAtTime appState =
      let newAppState = case appState.gameState of
            Play Wait -> justFlip cardId clickAtTime 0 appState
            Play (Progress startTime clicksCount NoCardTurned) -> justFlip cardId startTime clicksCount appState
            Play (Progress startTime clicksCount (OneCardTurned turnedCardId)) -> do
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
                  gameState =
                    if isWinBoard board
                      then Play (Win startTime (diffTimeToFloat clickAtTime startTime) clicksCount)
                      else Play (Progress startTime (clicksCount + 1) NoCardTurned)
               in appState
                    { gameState,
                      board,
                      cardsToRender = appState.cardsToRender <> cardsToRender
                    }
            _ -> appState
       in newAppState
    justFlip :: CardId -> StartTime -> ClicksCount -> AppState -> AppState
    justFlip cardId startTime clicksCount appState =
      appState
        { gameState = Play (Progress startTime (clicksCount + 1) $ OneCardTurned cardId),
          board = setCardStatus cardId TurnedWaitPair appState.board,
          cardsToRender = appState.cardsToRender <> [cardId]
        }
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

withEvent :: Monad m => AppID -> Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
withEvent wid tId tAttrs elm = with elm ([id_ (withWID wid tId), wsSend' ""] <> tAttrs)
  where
    wsSend' = makeAttribute "ws-send"

version :: Text
version = "1.0.0"

renderApp :: AppID -> SVGCollections -> MemoryVar AppState -> Database -> IO (HtmlT STM ())
renderApp wid cols appStateM db = do
  appState <- atomically $ readMemoryVar appStateM
  leaderboard <- renderLeaderBoard db
  timerStatus <- renderTimerStatus appStateM
  pure $ div_ [id_ (withWID wid "w"), class_ "container mx-auto"] $ do
    div_ [id_ (withWID wid "w"), class_ "flex flex-row justify-center"] $ do
      div_ [id_ "MSMain", class_ "min-w-fit max-w-fit border-2 rounded border-indigo-200 bg-slate-50"] $ do
        div_ [id_ "AppMain", class_ "flex flex-row justify-center"] $ do
          div_ [class_ "grow min-w-min max-w-screen-2xl"] $ do
            div_ [class_ "flex-col justify-between h-full"] $ do
              div_ [class_ "flex justify-between m-1 pl-2 pr-2 font-semibold bg-indigo-200"] $ do
                renderPlayStatus appStateM
                timerStatus
                div_ [class_ "flex gap-3 justify-end"] $ do
                  buttonRestart
                  buttonAccount
              div_ [id_ "SettingsAccount"] ""
              case appState.gameState of
                Play _ -> do
                  div_ [class_ ""] $ do
                    renderBoard wid cols appStateM
                    leaderboard
                _ -> p_ "Menu"
              div_ [class_ "flex flex-row gap-2 flex-row-reverse pr-2 bg-indigo-50"] $ do
                div_ [] (toHtml version)
                a_
                  [ class_ "text-blue-600",
                    href_ "https://github.com/web-apps-lab/MemoryMaster"
                  ]
                  "MemoryMaster"
  where
    buttonRestart :: HtmlT STM ()
    buttonRestart =
      withEvent wid "clickRestart" [] $ i_ [class_ "cursor-pointer ri-restart-line", title_ "Restart"] mempty
    buttonAccount :: HtmlT STM ()
    buttonAccount =
      withEvent wid "clickAccount" [] $ i_ [class_ "cursor-pointer ri-user-fill", title_ "Account"] mempty

renderBoard :: AppID -> SVGCollections -> MemoryVar AppState -> HtmlT STM ()
renderBoard wid cols appStateV = do
  div_ [id_ "Board", class_ "flex flex-row flex-wrap"] $ do
    div_ [class_ "basis-1/2 min-w-fit grow"] $ do
      div_ [class_ "m-2 grid grid-flow-row-dense gap-2 grid-cols-6 grid-rows-3 justify-items-center"] $ do
        mapM_ (renderCard wid cols appStateV) [0 .. 23]

renderSetAccountForm :: AppID -> UserName -> HtmlT STM ()
renderSetAccountForm wid username =
  div_ [id_ "SettingsAccount", class_ "pr-2 pl-2"] $ do
    div_ [id_ "SettingsAccountNotice"] ""
    withEvent wid "setAccount" [] $ do
      form_ [class_ "w-full"] $ do
        label_ [class_ "font-semibold"] "Set your name"
        input_
          [ type_ "text",
            name_ "playerName",
            value_ (into @Text username),
            placeholder_ "Guest",
            size_ "15",
            maxlength_ "15",
            class_ "h-8 ml-1 text-center border border-slate-300 rounded-md focus:border-slate-400"
          ]
        button_
          [ type_ "submit",
            class_ "ml-1 border-2 border-indigo-200 rounded"
          ]
          "Submit"

renderPlayStatus :: MemoryVar AppState -> HtmlT STM ()
renderPlayStatus appStateM = do
  appState <- lift $ readMemoryVar appStateM
  case appState.gameState of
    Play (Win {}) -> render "You Win !"
    Play (Progress _ clickCounts _) -> render $ toHtml $ "Playing (" <> show clickCounts <> " flips)"
    Play Wait -> render "Waiting for first card flip"
    Menu -> render $ div_ "In Menu"
  where
    render :: HtmlT STM () -> HtmlT STM ()
    render = div_ [id_ "PlayStatus"]

renderTimerStatus :: MemoryVar AppState -> IO (HtmlT STM ())
renderTimerStatus appStateM = do
  appState <- atomically $ readMemoryVar appStateM
  case appState.gameState of
    Play (Win _ playDuration _) -> pure $ renderTimer playDuration
    Play (Progress startTime _ _) -> do
      diffT <- diffTime startTime
      pure $ renderTimer diffT
    Play Wait -> pure $ renderTimer 0.0
    Menu -> pure $ pure ()

renderTimer :: Float -> HtmlT STM ()
renderTimer duration = do
  div_ [id_ "Timer", class_ "flex flex-row justify-between gap-2 w-24"] $ do
    if duration > 0
      then do
        div_ [class_ ""] "since "
        div_ [class_ ""] $ toHtml $ toDurationString duration <> " s"
      else div_ ""

diffTime :: UTCTime -> IO Float
diffTime startTime = do
  atTime <- liftIO getCurrentTime
  pure $ diffTimeToFloat atTime startTime

toDurationString :: Float -> String
toDurationString = printf "%1.f"

renderCard :: AppID -> SVGCollections -> MemoryVar AppState -> CardId -> HtmlT STM ()
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
          div_ [class_ "bg-indigo-100"] $
            img_ [src_ $ getSVGInline appState]
  where
    getSVGInline :: AppState -> Text
    getSVGInline appState =
      let svgName = getCardName appState.board cardId
          svg = fromMaybe (error "no svg") $ getSVGByName cols appState.collectionName svgName
          svgB64 = B64.encode svg
       in "data:image/svg+xml;base64," <> from (C8.unpack svgB64)
    boxes_style = "w-14 md:w-24 lg:w-32 h-14 md:h-24 lg:h-32 shadow shadow-black bg-yellow-50 border-2 rounded border-pink-100"
    cardIdHX = encodeVal [("index", Number $ fromInteger $ toInteger cardId)]
    cardDivId = from $ "Card" <> show cardId

renderLeaderBoard :: Database -> IO (HtmlT STM ())
renderLeaderBoard db = do
  scores <- getTopScores db 10 ""
  pure $
    div_ [id_ "LeaderBoard", class_ "m-1"] $
      case length scores of
        0 -> p_ "The leaderboard is empty. Be the first to appear here !"
        _ -> ol_ [] $ do
          header
          mapM_ displayScoreLine (zip [0 ..] scores)
  where
    displayScoreLine :: (Int, Score) -> HtmlT STM ()
    displayScoreLine (index, Score {..}) = do
      li_ []
        $ div_
          [ class_ $
              "grid grid-cols-6 gap-1 pl-1 pr-1 text-left "
                <> if even index then "bg-white" else ""
          ]
        $ do
          div_ [class_ "col-span-2"] $
            toHtml $
              formatTime defaultTimeLocale "%F" scoreDate
          div_ [class_ "col-span-2"] $ toHtml scoreName
          div_ [class_ "col-span-1"] $ toHtml (show scoreClicks)
          div_ [class_ "col-span-1 text-right"] $ toHtml $ toDurationString scoreDuration
    header :: HtmlT STM ()
    header = do
      li_ [] $ div_ [class_ "grid grid-cols-6 gap-1 font-semibold text-left bg-indigo-200 pr-1 pl-1"] $ do
        div_ [class_ "col-span-2"] "Date"
        div_ [class_ "col-span-2"] "Name"
        div_ [class_ "col-span-1"] "Flips"
        div_ [class_ "col-span-1 text-right"] "Time"
