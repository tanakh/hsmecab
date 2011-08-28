{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Text.MeCab(
  Text.MeCab.new,
  Text.MeCab.new2,
  
  version,
  
  parse,
  parseNBest,
  
  parseNBestInit,
  next,
  nextNode,
  
  Node(..), Stat(..),
  parseToNode,
  
  getPartial,
  setPartial,
  getTheta,
  setTheta,
  getLatticeLevel,
  setLatticeLevel,
  getAllMorphs,
  setAllMorphs,
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Foreign
import Foreign.C

import Prelude hiding (id)

#include <mecab.h>

newtype MeCab =
  MeCab { unMeCab :: ForeignPtr MeCab }
  deriving (Eq, Ord)

mkMeCab :: Ptr MeCab -> IO MeCab
mkMeCab p =
  MeCab <$> newForeignPtr p_mecab_destroy p

data MeCabError =
  MeCabError String
  deriving (Eq, Ord, Show, Typeable)

instance Exception MeCabError

new :: [String] -> IO MeCab
new args =
  withCStrings args $ \argc argv -> do
    p <- mecab_new (fromIntegral argc) argv
    when (p == nullPtr) $
      throwIO =<< (MeCabError <$> strerror nullPtr)
    mkMeCab p

new2 :: String -> IO MeCab
new2 arg =
  withCString arg $ \pstr ->
  mkMeCab =<< mecab_new2 pstr

withCStrings :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStrings ss f =
  withCStrings' ss $ \ps ->
    withArrayLen ps f

withCStrings' :: [String] -> ([CString] -> IO a) -> IO a
withCStrings' strs f = go [] strs where
  go ps [] = f $ reverse ps
  go ps (s:ss) =
    withCString s $ \p -> go (p:ps) ss

version :: IO String
version =
  peekCString =<< mecab_version

strerror :: Ptr MeCab -> IO String
strerror p =
  peekCString =<< mecab_strerror p

--

parse :: MeCab -> T.Text -> IO T.Text
parse m txt = withForeignPtr (unMeCab m) $ \pm ->
  B.useAsCStringLen (T.encodeUtf8 txt) $ \(pstr, len) -> do
    p <- mecab_sparse_tostr2 pm pstr (fromIntegral len)
    when (p == nullPtr) $ throwIO =<< (MeCabError <$> strerror pm)
    packCString p

parseNBest :: MeCab -> Int -> T.Text -> IO T.Text
parseNBest m n txt = withForeignPtr (unMeCab m) $ \pm ->
  B.useAsCStringLen (T.encodeUtf8 txt) $ \(pstr, len) -> do
    p <- mecab_nbest_sparse_tostr2 pm (fromIntegral n) pstr (fromIntegral len)
    when (p == nullPtr) $ throwIO =<< (MeCabError <$> strerror pm)
    packCString p

parseNBestInit :: MeCab -> T.Text -> IO ()
parseNBestInit m txt = withForeignPtr (unMeCab m) $ \pm ->
  B.useAsCStringLen (T.encodeUtf8 txt) $ \(pstr, len) -> do
    ret <- mecab_nbest_init2 pm pstr (fromIntegral len)
    when (ret /= 1) $ throwIO =<< (MeCabError <$> strerror pm)

next :: MeCab -> IO (Maybe T.Text)
next m = withForeignPtr (unMeCab m) $ \pm -> do
  r <- mecab_nbest_next_tostr pm
  if r == nullPtr
    then return Nothing
    else Just <$> packCString r

packCString :: CString -> IO T.Text
packCString p = T.decodeUtf8 <$> B.packCString p

--

data Stat =
  NOR | UNK | BOS | EOS
  deriving (Eq, Read, Show)

data Node =
  Node
  { surface :: T.Text
  , feature :: T.Text
  , rlength :: CUShort
  , id :: CUInt
  , rcAttr :: CUShort
  , lcAttr :: CUShort
  , posid :: CUShort
  , charType :: CUChar
  , stat :: Stat
  , isBest :: Bool
  , alpha :: CFloat
  , beta :: CFloat
  , prob :: CFloat
  , wcost :: CShort
  , cost :: CLong
  } deriving (Eq, Read, Show)

peekNodes :: Ptr Node -> IO [Node]
peekNodes ptr
  | ptr == nullPtr =
    return []
  | otherwise =
      (:) <$> peekNode ptr
          <*> (peekNodes =<< (#peek mecab_node_t, next) ptr)

peekNode :: Ptr Node -> IO Node
peekNode ptr = do
  sfc <- do
    p <- (#peek mecab_node_t, surface) ptr
    len <- (#peek mecab_node_t, length) ptr
    T.decodeUtf8 <$> B.packCStringLen (p, fromIntegral (len :: CUShort))
  Node
    <$> return sfc
    <*> (packCString =<< (#peek mecab_node_t, feature) ptr)
    <*> (#peek mecab_node_t, rlength) ptr
    <*> (#peek mecab_node_t, id) ptr
    <*> (#peek mecab_node_t, rcAttr) ptr
    <*> (#peek mecab_node_t, lcAttr) ptr
    <*> (#peek mecab_node_t, posid) ptr
    <*> (#peek mecab_node_t, char_type) ptr
    <*> (toStat <$> (#peek mecab_node_t, stat) ptr)
    <*> ((==(1::CUChar)) <$> (#peek mecab_node_t, isbest) ptr)
    <*> (#peek mecab_node_t, alpha) ptr
    <*> (#peek mecab_node_t, beta) ptr
    <*> (#peek mecab_node_t, prob) ptr
    <*> (#peek mecab_node_t, wcost) ptr
    <*> (#peek mecab_node_t, cost) ptr
  where
    toStat :: CUChar -> Stat
    toStat (#const MECAB_NOR_NODE) = NOR
    toStat (#const MECAB_UNK_NODE) = UNK
    toStat (#const MECAB_BOS_NODE) = BOS
    toStat (#const MECAB_EOS_NODE) = EOS
    toStat _ = UNK

parseToNode :: MeCab -> T.Text -> IO [Node]
parseToNode m txt = withForeignPtr (unMeCab m) $ \pm ->
  B.useAsCStringLen (T.encodeUtf8 txt) $ \(pstr, len) -> do
    p <- mecab_sparse_tonode2 pm pstr (fromIntegral len)
    when (p == nullPtr) $ throwIO =<< (MeCabError <$> strerror pm)
    peekNodes p
 
nextNode :: MeCab -> IO  [Node]
nextNode m = withForeignPtr (unMeCab m) $ \pm -> do
  p <- mecab_nbest_next_tonode pm
  when (p == nullPtr) $ throwIO =<< (MeCabError <$> strerror pm)
  peekNodes p

--

getPartial :: MeCab -> IO Bool
getPartial m = withForeignPtr (unMeCab m) $ \pm ->
  (==1) <$> mecab_get_partial pm

setPartial :: MeCab -> Bool -> IO ()
setPartial m b = withForeignPtr (unMeCab m) $ \pm ->
  mecab_set_partial pm (if b then 1 else 0)

getTheta :: MeCab -> IO Double
getTheta m = withForeignPtr (unMeCab m) $ \pm ->
  liftM realToFrac $ mecab_get_theta pm

setTheta :: MeCab -> Double -> IO ()
setTheta m f = withForeignPtr (unMeCab m) $ \pm ->
  mecab_set_theta pm (realToFrac f)

getLatticeLevel :: MeCab -> IO Int
getLatticeLevel m = withForeignPtr (unMeCab m) $ \pm ->
  fromIntegral <$> mecab_get_lattice_level pm

setLatticeLevel :: MeCab -> Int -> IO ()
setLatticeLevel m ll = withForeignPtr (unMeCab m) $ \pm ->
  mecab_set_lattice_level pm (fromIntegral ll)

getAllMorphs :: MeCab -> IO Int
getAllMorphs m = withForeignPtr (unMeCab m) $ \pm ->
  fromIntegral <$> mecab_get_all_morphs pm

setAllMorphs :: MeCab -> Int -> IO ()
setAllMorphs m am = withForeignPtr (unMeCab m) $ \pm ->
  mecab_set_all_morphs pm (fromIntegral am)

--

foreign import ccall mecab_new
  :: CInt -> Ptr CString -> IO (Ptr MeCab)

foreign import ccall mecab_new2
  :: CString -> IO (Ptr MeCab)

foreign import ccall "&mecab_destroy"
  p_mecab_destroy :: FunPtr (Ptr MeCab -> IO ())

foreign import ccall mecab_version
  :: IO CString

foreign import ccall mecab_strerror
  :: Ptr MeCab -> IO CString

foreign import ccall mecab_sparse_tostr2
  :: Ptr MeCab -> CString -> CSize -> IO CString

foreign import ccall mecab_nbest_sparse_tostr2
  :: Ptr MeCab -> CSize -> CString -> CSize -> IO CString

foreign import ccall mecab_nbest_init2
  :: Ptr MeCab -> CString -> CSize -> IO CInt

foreign import ccall mecab_nbest_next_tostr
  :: Ptr MeCab -> IO CString

foreign import ccall mecab_sparse_tonode2
  :: Ptr MeCab -> CString -> CSize -> IO (Ptr Node)

foreign import ccall mecab_nbest_next_tonode
  :: Ptr MeCab -> IO (Ptr Node)

foreign import ccall mecab_get_partial
  :: Ptr MeCab -> IO CInt

foreign import ccall mecab_set_partial
  :: Ptr MeCab -> CInt -> IO ()

foreign import ccall mecab_get_theta
  :: Ptr MeCab -> IO CFloat

foreign import ccall mecab_set_theta
  :: Ptr MeCab -> CFloat -> IO ()

foreign import ccall mecab_get_lattice_level
  :: Ptr MeCab -> IO CInt

foreign import ccall mecab_set_lattice_level
  :: Ptr MeCab -> CInt -> IO ()

foreign import ccall mecab_get_all_morphs
  :: Ptr MeCab -> IO CInt

foreign import ccall mecab_set_all_morphs
  :: Ptr MeCab -> CInt -> IO ()
