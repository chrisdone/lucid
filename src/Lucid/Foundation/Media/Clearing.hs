{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Media.Clearing where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


clearing_thumbs_ :: Term arg result => arg -> result
clearing_thumbs_ =
  termWith "ul" [class_ "clearing-thumbs"]

