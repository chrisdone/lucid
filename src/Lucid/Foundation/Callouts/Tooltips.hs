{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Callouts.Tooltips where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


data_tooltip_ :: Attribute
data_tooltip_ = data_ "attribute" ""

has_tip_ :: T.Text
has_tip_ = " has-tip "



