{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | We can't lens into DOM on the server-side, yet, so we've made some auxilliary functions
-- for creating @row_@ @<div>@ tags, and some class names for creating columns.
-- .
-- Use them like this:
-- .
-- >  div_ [class_ $ mconcat [ row_
-- >                         , small_collapsed_
-- >                         , large_uncollapsed_ ]] $
-- >     div_ [class_ $ mconcat [ columns_
-- >                            , small_ 6
-- >                            , medium_ 4
-- >                            , large_ 3
-- >                            , small_centered_
-- >                            , large_uncentered_
-- >                            , small_push_ 6
-- >                            , medium_pull_ 4
-- >                            , large_reset_order_ ]] $
-- >        -- rest of DOM...

module Lucid.Foundation.Structure.Grid where

import Lucid.Base
import Lucid.Html5

import qualified Data.Text as T
import Data.Monoid

row_ :: T.Text
row_ = " row "

small_collapsed_ :: T.Text
small_collapsed_ = " small-collapsed "

medium_collapsed_ :: T.Text
medium_collapsed_ = " medium-collapsed "

large_collapsed_ :: T.Text
large_collapsed_ = " large-collapsed "

medium_uncollapsed_ :: T.Text
medium_uncollapsed_ = " medium-uncollapsed "

large_uncollapsed_ :: T.Text
large_uncollapsed_ = " large-uncollapsed "


columns_ :: T.Text
columns_ = " columns "

small_ :: Int -> T.Text
small_ n = " small-" <> (T.pack $ show n) <> " "

medium_ :: Int -> T.Text
medium_ n = " medium-" <> (T.pack $ show n) <> " "

large_ :: Int -> T.Text
large_ n = " large-" <> (T.pack $ show n) <> " "

small_offset_ :: Int -> T.Text
small_offset_ n = " small-offset-" <> (T.pack $ show n) <> " "

medium_offset_ :: Int -> T.Text
medium_offset_ n = " medium-offset-" <> (T.pack $ show n) <> " "

large_offset_ :: Int -> T.Text
large_offset_ n = " large-offset-" <> (T.pack $ show n) <> " "

small_centered_ :: T.Text
small_centered_ = " small-centered "

medium_centered_ :: T.Text
medium_centered_ = " medium-centered "

large_centered_ :: T.Text
large_centered_ = " large-centered "

small_uncentered_ :: T.Text
small_uncentered_ = " small-uncentered "

medium_uncentered_ :: T.Text
medium_uncentered_ = " medium-uncentered "

large_uncentered_ :: T.Text
large_uncentered_ = " large-uncentered "
small_push_ :: Int -> T.Text
small_push_ n = " small-push-" <> (T.pack $ show n) <> " "

medium_push_ :: Int -> T.Text
medium_push_ n = " medium-push-" <> (T.pack $ show n) <> " "

large_push_ :: Int -> T.Text
large_push_ n = " large-push-" <> (T.pack $ show n) <> " "

small_pull_ :: Int -> T.Text
small_pull_ n = " small-pull-" <> (T.pack $ show n) <> " "

medium_pull_ :: Int -> T.Text
medium_pull_ n = " medium-pull-" <> (T.pack $ show n) <> " "

large_pull_ :: Int -> T.Text
large_pull_ n = " large-pull-" <> (T.pack $ show n) <> " "

small_reset_order_ :: T.Text
small_reset_order_ = " small-reset-order "

medium_reset_order_ :: T.Text
medium_reset_order_ = " medium-reset-order "

large_reset_order_ :: T.Text
large_reset_order_ = " large-reset-order "
