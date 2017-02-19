{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Text.MeCab as M

import Test.Hspec
import qualified Data.Text as T

main :: IO ()
main =
    M.new [] >>= \handle ->
    hspec $ describe "Text.MeCab" $
    do it "processes large input" $
           do let s = T.replicate 50000 "ねえ、私はアレキサンダーです。"
              ns <- M.parseToNodes handle s
              length ns `shouldBe` 350002
       it "correctly extracts nodes" $
           do ns <-
                  map M.nodeSurface . filter ((\x -> x == M.NOR || x == M.UNK) . M.nodeStat)
                  <$> M.parseToNodes handle (T.pack "ねえ、私はアレキサンダーです。")
              ns `shouldBe` ["ねえ", "、", "私", "は", "アレキサンダー", "です", "。"]
