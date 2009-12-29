import NLP.MeCab

import Control.Monad
import Data.Maybe

import Prelude hiding (error, null)

main = do
  putStrLn =<< version
  
  m <- new ["mecab", "-l1"]
  
  putStrLn =<< sparseToStr m "にわにはにわにわとりがいる"
  -- putStrLn =<< nBestSparseToStr m 2 "にわにはにわにわとりがいる"
  
  nBestInit m "にわにはにわにわとりがいる"
  flip mapM_ [1..5] $ \_ -> do
    s <- nBestNextToStr m
    when (isJust s) $
      putStrLn $ fromJust s
  
  destroy m
  
  return ()
