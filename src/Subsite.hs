{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Subsite
  ( module Subsite.Data
  , module Subsite
  ) where

import ClassyPrelude
import Network.Wai qualified as W
import Network.Wai.Internal (requestMethod)
import StaticFiles
import Subsite.Data
import Yesod
import Yesod.Core.Types

getSubHomeR :: Yesod master => SubHandlerFor Subsite master Html
getSubHomeR = do
  toParent <- getRouteToParent
  liftHandler $ defaultLayout $ do
    addStylesheet $ toParent $ StaticR css_normalize_css
    [whamlet|Welcome to the subsite!|]

instance Yesod master => YesodSubDispatch Subsite master where
  yesodSubDispatch env req =
    case W.pathInfo req of
      [] -> case requestMethod req of
        "GET" -> subHelper getSubHomeR env (Just SubHomeR) req
        _     -> subHelper (void badMethod) env (Just SubHomeR) req

      ("static":rest) ->
        yesodSubDispatch
          YesodSubRunnerEnv
            { ysreGetSub = getStatic . ysreGetSub env
            , ysreToParentRoute = ysreToParentRoute env . StaticR
            , ysreParentRunner = ysreParentRunner env
            , ysreParentEnv = ysreParentEnv env
            }
          req { W.pathInfo = rest }

      _ -> subHelper (void notFound) env Nothing req

subHelper
  :: ToTypedContent content
  => SubHandlerFor child master content
  -> YesodSubRunnerEnv child master
  -> Maybe (Route child)
  -> W.Application
subHelper (SubHandlerFor f) YesodSubRunnerEnv {..} mroute =
  ysreParentRunner handler ysreParentEnv (ysreToParentRoute <$> mroute)
  where
    handler = fmap toTypedContent $ HandlerFor $ \hd ->
      let rhe = handlerEnv hd
          rhe' = rhe
            { rheRoute = mroute
            , rheChild = ysreGetSub $ yreSite ysreParentEnv
            , rheRouteToMaster = ysreToParentRoute
            }
       in f hd { handlerEnv = rhe' }
