{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Forms.Abide where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


data_abide_ :: Attribute
data_abide_ = data_ "abide" ""

