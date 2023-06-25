{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Subsite (
  module Subsite.Data,
  module Subsite,
) where

import ClassyPrelude
import StaticFiles
import Subsite.Data
import Yesod

import Network.Wai
import Yesod.Core.Types

getSubHomeR :: Yesod master => SubHandlerFor Subsite master Html
getSubHomeR = do
  toParent <- getRouteToParent
  liftHandler $ defaultLayout $ do
    addStylesheet $ toParent $ StaticR css_normalize_css
    [whamlet|Welcome to the subsite!|]

instance Yesod master => YesodSubDispatch Subsite master where
  -- yesodSubDispatch = $(mkYesodSubDispatch resourcesSubsite)
  yesodSubDispatch =
    let
      helper_a3P0 =
        inner_a3OZ
       where
        inner_a3OZ env6731_a3OO req6731_a3OP =
          helper6731_a3OQ (pathInfo req6731_a3OP)
         where
          helper6731_a3OQ [] =
            case requestMethod req6731_a3OP of
              "GET" ->
                (((subHelper getSubHomeR) env6731_a3OO) (Just SubHomeR))
                  req6731_a3OP
              _ ->
                (((subHelper (void badMethod)) env6731_a3OO) (Just SubHomeR))
                  req6731_a3OP
          helper6731_a3OQ ((:) "static" restPath_a3OR) =
            ( ( ( ( ( \_ getSub_a3OU toParent_a3OV env_a3OW ->
                        yesodSubDispatch
                          YesodSubRunnerEnv
                            { ysreParentRunner = ysreParentRunner env_a3OW
                            , ysreGetSub = (getSub_a3OU . ysreGetSub env_a3OW)
                            , ysreToParentRoute = (ysreToParentRoute env_a3OW . toParent_a3OV)
                            , ysreParentEnv = ysreParentEnv env_a3OW
                            }
                    )
                      subHelper
                  )
                    (\sub_a3OX -> getStatic sub_a3OX)
                )
                  (\sroute_a3OY -> StaticR sroute_a3OY)
              )
                env6731_a3OO
            )
              ( ((\p_a3OS r_a3OT -> r_a3OT{pathInfo = p_a3OS}) restPath_a3OR)
                  req6731_a3OP
              )
          helper6731_a3OQ _ =
            (((subHelper (void notFound)) env6731_a3OO) Nothing) req6731_a3OP
     in
      helper_a3P0

subHelper ::
  ToTypedContent content =>
  SubHandlerFor child master content ->
  YesodSubRunnerEnv child master ->
  Maybe (Route child) ->
  Application
subHelper (SubHandlerFor f) YesodSubRunnerEnv{..} mroute =
  ysreParentRunner handler ysreParentEnv (fmap ysreToParentRoute mroute)
 where
  handler = fmap toTypedContent $ HandlerFor $ \hd ->
    let rhe = handlerEnv hd
        rhe' =
          rhe
            { rheRoute = mroute
            , rheChild = ysreGetSub $ yreSite ysreParentEnv
            , rheRouteToMaster = ysreToParentRoute
            }
     in f hd{handlerEnv = rhe'}