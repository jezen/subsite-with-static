{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import ClassyPrelude hiding (Handler)
import Subsite
import Yesod
import Yesod.Core.Types

{- HLINT ignore "Use newtype instead of data" -}
data App = App
  { getSubsite :: Subsite
  }

mkMessage "App" "messages" "en"

mkYesod "App" [parseRoutes|
/ HomeR GET
/sub SubsiteR Subsite getSubsite
|]

getHomeR :: Handler Html
getHomeR = do
  now <- liftIO getCurrentTime
  defaultLayout
    [whamlet|<p>The time now is: #{tshow now}|]

instance Yesod App where

makeFoundation :: IO App
makeFoundation = App <$> newSubsite

main :: IO ()
main = makeFoundation >>= warp 3000

makeApplication :: App -> IO Application
makeApplication foundation = do
  appPlain <- toWaiAppPlain foundation
  return $ defaultMiddlewaresNoLogging appPlain

getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  foundation <- makeFoundation
  app1       <- makeApplication foundation
  return (3000, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _foundation = pure ()
