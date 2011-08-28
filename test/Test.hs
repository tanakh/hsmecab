{-# Language OverloadedStrings #-}

import Text.MeCab

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  putStrLn =<< version
  
  m <- new ["mecab", "-l1"]
  
  T.putStrLn =<< parse m "にわにはにわにわとりがいる"
  T.putStrLn =<< parseNBest m 2 "にわにはにわにわとりがいる"

  parseNBestInit m "にわにはにわにわとりがいる"
  replicateM_ 5 $ do
    s <- next m
    when (isJust s) $
      T.putStrLn $ fromJust s
  
  print =<< parseToNode m "にわにはにわにわとりがいる"
