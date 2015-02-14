{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Media.Orbit where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid

data_orbit_ :: Attribute
data_orbit_ = data_ "orbit" ""


data_orbit_slide_ :: T.Text -> Attribute
data_orbit_slide_ = data_ "orbit-slide"

data_orbit_link_ :: T.Text -> Attribute
data_orbit_link_ = data_ "orbit-slide"

