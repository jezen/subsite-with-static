{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Subsite
  ( module Subsite.Data
  , module Subsite
  ) where

import ClassyPrelude
import Subsite.Data
import Yesod

getSubHomeR :: Yesod master => SubHandlerFor Subsite master Html
getSubHomeR = liftHandler $ defaultLayout $ do
  -- TODO: Swap this remote stylesheet for the local one
  addStylesheetRemote "https://necolas.github.io/normalize.css/8.0.1/normalize.css"
  -- TODO: Make this work
  --addStylesheet $ StaticR css_normalize_css
  [whamlet|Welcome to the subsite!|]

instance Yesod master => YesodSubDispatch Subsite master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesSubsite)
