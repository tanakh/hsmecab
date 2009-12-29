{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module NLP.MeCab(
  new, new2,
  destroy,
  version,
  error, null,
  sparseToStr,
  nBestSparseToStr,
  nBestInit,
  nBestNextToStr,
  ) where

import Control.Exception
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import qualified Codec.Binary.UTF8.String as U8

import Prelude hiding (error, null)

type MeCab = Ptr MeCab_t
data MeCab_t

new :: [String] -> IO MeCab
new args =
  bracket
    (mapM newCString args)
    (mapM free) $ \argsp ->
    withArrayLen argsp $ \argc argv -> do
      p <- mecab_new (fromIntegral argc) argv
      when (p==nullPtr) $ fail =<< error null
      return p

new2 :: String -> IO MeCab
new2 arg =
  withCString arg $ \pstr ->
  mecab_new2 pstr

destroy :: MeCab -> IO ()
destroy = mecab_destroy

version :: IO String
version =
  mecab_version >>= peekCString

error :: MeCab -> IO String
error m =
  mecab_strerror m >>= peekCString

null :: MeCab
null = nullPtr

sparseToStr :: MeCab -> String -> IO String
sparseToStr m str =
  withCStringLen (U8.encodeString str) $ \(pstr, len) -> do
    p <- mecab_sparse_tostr2 m pstr (fromIntegral len)
    when (p==nullPtr) $ fail =<< error m
    peekCString p

nBestSparseToStr :: MeCab -> Int -> String -> IO String
nBestSparseToStr m n str =
  withCStringLen (U8.encodeString str) $ \(pstr, len) -> do
    p <- mecab_nbest_sparse_tostr2 m (fromIntegral n) pstr (fromIntegral len)
    when (p==nullPtr) $ fail =<< error m
    peekCString p

nBestInit :: MeCab -> String -> IO ()
nBestInit m str =
  withCStringLen (U8.encodeString str) $ \(pstr, len) -> do
    ret <- mecab_nbest_init2 m pstr (fromIntegral len)
    when (ret/=1) $ fail =<< error m

nBestNextToStr :: MeCab -> IO (Maybe String)
nBestNextToStr m = do
  r <- mecab_nbest_next_tostr m
  if r==nullPtr
    then return Nothing
    else liftM Just $peekCString r

--

foreign import ccall mecab_new ::
  CInt -> Ptr CString -> IO MeCab

foreign import ccall mecab_new2 ::
  CString -> IO MeCab

foreign import ccall mecab_version ::
  IO CString

foreign import ccall mecab_strerror ::
  MeCab -> IO CString

-- foreign import ccall mecab_sparse_tostr ::
--   MeCab -> CString -> IO CString

foreign import ccall mecab_sparse_tostr2 ::
  MeCab -> CString -> CSize -> IO CString

foreign import ccall mecab_nbest_sparse_tostr2 :: 
  MeCab -> CSize -> CString -> CSize -> IO CString

foreign import ccall mecab_nbest_init2 ::
  MeCab -> CString -> CSize -> IO CInt

foreign import ccall mecab_nbest_next_tostr ::
  MeCab -> IO CString

foreign import ccall mecab_destroy ::
  MeCab -> IO ()
