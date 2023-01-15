{-# HLINT ignore "Use if" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MemoryMaster (run, memoryMasterApp) where

import Butler
import Butler.Auth (PageDesc (PageDesc), PageTitle (..))
import Butler.Database (Database, dbSimpleCreate, withDatabase)
import Prelude

memoryMasterApp :: Database -> App
memoryMasterApp db =
  let name = "Memory Master"
      tags = mempty
      title = "Memory Master"
      description = "MemoryMaster"
      size = Nothing
      xfiles = mempty
      start = startHH db
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

startHH :: Database -> AppContext -> ProcessIO ()
startHH db ctx = do
  forever $ do
    res <- atomically (readPipe ctx.pipe)
    case res of
      AppDisplay _ -> do
        app <- liftIO $ renderApp ctx.wid db
        sendHtmlOnConnect app res
      AppTrigger _ev -> pure ()
      _ -> pure ()

renderApp :: WinID -> Database -> IO (HtmlT STM ())
renderApp wid _db = do
  pure $ div_ [id_ (withWID wid "w"), class_ "container mx-auto"] $ do
    div_ [id_ (withWID wid "w"), class_ "flex flex-row justify-center"] $ do
      div_ [id_ "MSMain", class_ "min-w-fit max-w-fit border-2 rounded border-gray-400 bg-gray-100"] $ do
        div_ [class_ "flex flex-col"] $ do
          p_ "MemoryMaster"
