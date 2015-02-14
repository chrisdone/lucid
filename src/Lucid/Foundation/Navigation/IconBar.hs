{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Navigation.IconBar where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


item_ :: T.Text
item_ = " item "

icon_bar_ :: T.Text
icon_bar_ = " icon-bar "

vertical_ :: T.Text
vertical_ = " vertical "

vertical_size_ :: T.Text -> T.Text
vertical_size_ size = " " <> size <> "-vertical "

one_up_ :: T.Text
one_up_ = " one-up "

two_up_ :: T.Text
two_up_ = " two-up "

three_up_ :: T.Text
three_up_ = " three-up "

four_up_ :: T.Text
four_up_ = " four-up "

five_up_ :: T.Text
five_up_ = " five-up "

six_up_ :: T.Text
six_up_ = " six-up "

seven_up_ :: T.Text
seven_up_ = " seven-up "

eight_up_ :: T.Text
eight_up_ = " eight-up "
