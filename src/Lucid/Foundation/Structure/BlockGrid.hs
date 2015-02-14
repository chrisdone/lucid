{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Structure.BlockGrid where

import qualified Data.Text as T
import Data.Monoid


small_block_grid_ :: Int -> T.Text
small_block_grid_ n = " small-block-grid-" <> (T.pack $ show n) <> " "

medium_block_grid_ :: Int -> T.Text
medium_block_grid_ n = " medium-block-grid-" <> (T.pack $ show n) <> " "

large_block_grid_ :: Int -> T.Text
large_block_grid_ n = " large-block-grid-" <> (T.pack $ show n) <> " "
