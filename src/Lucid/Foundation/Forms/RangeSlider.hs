{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Forms.RangeSlider where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


range_slider_ :: T.Text
range_slider_ = " range-slider "

data_slider_ :: Attribute
data_slider_ = data_ "slider" ""

role_slider_ :: Attribute
role_slider_ = makeAttribute "role" "slider"

range_slider_handle_ :: T.Text
range_slider_handle_ = " range-slider-handle "

range_slider_active_segment_ :: T.Text
range_slider_active_segment_ = " range-slider-active-segment "

vertical_range_ :: T.Text
vertical_range_ = " vertical-range "

makeVertical_ :: Attribute
makeVertical_ = data_ "options" "vertical: true;"
