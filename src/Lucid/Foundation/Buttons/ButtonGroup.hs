{-# LANGUAGE OverloadedStrings #-}

module Lucid.Foundation.Buttons.ButtonGroup where

import qualified Data.Text as T
import Data.Monoid

button_group_ :: T.Text
button_group_ = " button-group "

even_ :: Int -> T.Text
even_ n = " even-" <> (T.pack $ show n) <> " "

stack_ :: T.Text
stack_ = " stack "

stack_for_ :: T.Text -> T.Text
stack_for_ size = " stack-for-" <> size <> " "

button_bar_ :: T.Text
button_bar_ = " button-bar "
