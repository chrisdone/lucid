{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Content.Equalizer where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


data_equalizer_ :: Attribute
data_equalizer_ = data_ "equalizer" ""

data_equalizer_watch_ :: Attribute
data_equalizer_watch_ = data_ "equalizer-watch" ""



