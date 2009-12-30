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
  
  Node(..), Stat(..),
  sparseToNode,
  nBestNextToNode,
  
  getPartial,
  setPartial,
  getTheta,
  setTheta,
  getLatticeLevel,
  setLatticeLevel,
  getAllMorphs,
  setAllMorphs,
  ) where

import Control.Exception
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import qualified Codec.Binary.UTF8.String as U8

import Prelude hiding (error, null, id)

#include <mecab.h>

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

data Stat =
  NOR | UNK | BOS | EOS
  deriving (Eq, Read, Show)

data Node = Node {
  surface :: String,
  feature :: String,
  spaces :: Int,
  id :: Int,
  rcAttr :: Int,
  lcAttr :: Int,
  posid :: Int,
  charType :: Int,
  stat :: Stat,
  isBest :: Bool,
  alpha :: Double,
  beta :: Double,
  prob :: Double,
  wcost :: Int,
  cost :: Int
  } deriving (Eq, Read, Show)
            
sparseToNode :: MeCab -> String -> IO [Node]
sparseToNode m str = do
  withCStringLen (U8.encodeString str) $ \(pstr, len) -> do
    p <- mecab_sparse_tonode2 m pstr (fromIntegral len)
    peekNodes p

nBestNextToNode :: MeCab -> IO  [Node]
nBestNextToNode m = do
  p <- mecab_nbest_next_tonode m 
  peekNodes p

peekNodes :: Ptr () -> IO [Node]
peekNodes ptr
  | ptr == nullPtr = return []
  | otherwise = do
    len <- (#peek mecab_node_t, length) ptr
    rlen <- (#peek mecab_node_t, rlength) ptr
    ps <- (#peek mecab_node_t, surface) ptr
    sf <- peekCStringLen (ps, fromIntegral len)
    pf <- (#peek mecab_node_t, feature) ptr
    fe <- peekCString pf
    ii <- (#peek mecab_node_t, id) ptr
    ra <- (#peek mecab_node_t, rcAttr) ptr
    la <- (#peek mecab_node_t, lcAttr) ptr
    po <- (#peek mecab_node_t, posid) ptr
    ct <- (#peek mecab_node_t, char_type) ptr
    st <- (#peek mecab_node_t, stat) ptr
    ib <- (#peek mecab_node_t, isbest) ptr
    al <- (#peek mecab_node_t, alpha) ptr
    be <- (#peek mecab_node_t, beta) ptr
    pr <- (#peek mecab_node_t, prob) ptr
    wc <- (#peek mecab_node_t, wcost) ptr
    co <- (#peek mecab_node_t, cost) ptr
    next <- (#peek mecab_node_t, next) ptr
    rest <- peekNodes next
    let cur = Node {
          surface = sf,
          feature = fe,
          spaces = fromIntegral (rlen-len :: CUShort),
          id = fromIntegral (ii :: CUInt),
          rcAttr = fromIntegral (ra :: CUShort),
          lcAttr = fromIntegral (la :: CUShort),
          posid = fromIntegral (po :: CUShort),
          charType = fromIntegral (ct :: CUChar),
          stat = toStat (st :: CUChar),
          isBest = (ib :: CUChar) ==1,
          alpha = realToFrac (al :: CFloat),
          beta = realToFrac (be :: CFloat),
          prob = realToFrac (pr :: CFloat),
          wcost = fromIntegral (wc :: CShort),
          cost = fromIntegral (co :: CLong) }
    return $ cur : rest

  where
    toStat (#const MECAB_NOR_NODE) = NOR
    toStat (#const MECAB_UNK_NODE) = UNK
    toStat (#const MECAB_BOS_NODE) = BOS
    toStat (#const MECAB_EOS_NODE) = EOS
    toStat _ = UNK

getPartial :: MeCab -> IO Bool
getPartial m =
  liftM (==1) $ mecab_get_partial m

setPartial :: MeCab -> Bool -> IO ()
setPartial m b =
  mecab_set_partial m (if b then 1 else 0)

getTheta :: MeCab -> IO Double
getTheta m =
  liftM realToFrac $ mecab_get_theta m

setTheta :: MeCab -> Double -> IO ()
setTheta m f =
  mecab_set_theta m (realToFrac f)

getLatticeLevel :: MeCab -> IO Int
getLatticeLevel m =
  liftM fromIntegral $ mecab_get_lattice_level m

setLatticeLevel :: MeCab -> Int -> IO ()
setLatticeLevel m ll =
  mecab_set_lattice_level m (fromIntegral ll)

getAllMorphs :: MeCab -> IO Int
getAllMorphs m =
  liftM fromIntegral $ mecab_get_all_morphs m

setAllMorphs :: MeCab -> Int -> IO ()
setAllMorphs m am =
  mecab_set_all_morphs m (fromIntegral am)

--

foreign import ccall mecab_new ::
  CInt -> Ptr CString -> IO MeCab

foreign import ccall mecab_new2 ::
  CString -> IO MeCab

foreign import ccall mecab_version ::
  IO CString

foreign import ccall mecab_strerror ::
  MeCab -> IO CString

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

foreign import ccall mecab_sparse_tonode2 ::
  MeCab -> CString -> CSize -> IO (Ptr ())

foreign import ccall mecab_nbest_next_tonode ::
  MeCab -> IO (Ptr ())

foreign import ccall mecab_get_partial ::
  MeCab -> IO CInt

foreign import ccall mecab_set_partial ::
  MeCab -> CInt -> IO ()

foreign import ccall mecab_get_theta ::
  MeCab -> IO CFloat

foreign import ccall mecab_set_theta ::
  MeCab -> CFloat -> IO ()

foreign import ccall mecab_get_lattice_level ::
  MeCab -> IO CInt

foreign import ccall mecab_set_lattice_level ::
  MeCab -> CInt -> IO ()

foreign import ccall mecab_get_all_morphs ::
  MeCab -> IO CInt

foreign import ccall mecab_set_all_morphs ::
  MeCab -> CInt -> IO ()
