{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Subsite.Data where

import ClassyPrelude
import Yesod
import Yesod.Static

{- HLINT ignore "Use newtype instead of data" -}
data Subsite = Subsite
  { getStatic :: Static
  }

mkYesodSubData
  "Subsite"
  [parseRoutes|
/ SubHomeR GET

/static StaticR Static getStatic
|]

newSubsite :: MonadIO m => m Subsite
newSubsite = Subsite <$> liftIO (static "static")
