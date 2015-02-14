{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Navigation.OffCanvas where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


off_canvas_wrap_ :: Monad m => HtmlT m () -> HtmlT m ()
off_canvas_wrap_ content =
  termWith "div" [class_ "off-canvas-wrap"] $
    termWith "main" [class_ "inner-wrap"] content

data_offcanvas_ :: Attribute
data_offcanvas_ = data_ "offcanvas" ""


left_off_canvas_menu_ :: Term arg result => arg -> result
left_off_canvas_menu_ =
  termWith "nav" [class_ "left-off-canvas-menu"]

right_off_canvas_menu_ :: Term arg result => arg -> result
right_off_canvas_menu_ =
  termWith "nav" [class_ "right-off-canvas-menu"]


left_off_canvas_toggle_ :: T.Text
left_off_canvas_toggle_ = " left-off-canvas-toggle "

right_off_canvas_toggle_ :: T.Text
right_off_canvas_toggle_ = " right-off-canvas-toggle "

exit_off_canvas_ :: T.Text
exit_off_canvas_ = " exit-off-canvas "


left_small_ :: Term arg result => arg -> result
left_small_ =
  termWith "div" [class_ "left-small"]

right_small_ :: Term arg result => arg -> result
right_small_ =
  termWith "div" [class_ "right-small"]

off_canvas_list_ :: Term arg result => arg -> result
off_canvas_list_ =
  termWith "ul" [class_ "off-canvas-list"]


