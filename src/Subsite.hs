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

getSubHomeR :: Yesod master => SubHandlerFor Subsite master Html
getSubHomeR = do
  toParent <- getRouteToParent
  liftHandler $ defaultLayout $ do
    addStylesheet $ toParent $ StaticR css_normalize_css
    [whamlet|Welcome to the subsite!|]

instance Yesod master => YesodSubDispatch Subsite master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesSubsite)
