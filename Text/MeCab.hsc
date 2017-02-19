{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.MeCab(
  -- * MeCab type
  MeCab,

  -- * Error Type
  MeCabError(..),

  -- * Node type
  Node(..), Stat(..),

  -- * String-like class
  MeCabString(..),

  -- * Initializing MeCab
  Text.MeCab.new,
  Text.MeCab.new2,

  -- * Parsing
  parse,
  parseToNodes,

  -- * N-best parsing
  parseNBest,
  parseNBestInit,
  nBestNext,
  nBestNextNodes,

  -- * Setting properties
  getPartial,
  setPartial,
  getTheta,
  setTheta,
  getLatticeLevel,
  setLatticeLevel,
  getAllMorphs,
  setAllMorphs,

  -- * Geting MeCab version
  version,
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Typeable
import Foreign
import Foreign.C
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Prelude

#include <mecab.h>

newtype MeCab =
  MeCab { unMeCab :: ForeignPtr MeCab }
  deriving (Eq, Ord)

mkMeCab :: Ptr MeCab -> IO MeCab
mkMeCab p =
  MeCab <$> newForeignPtr p_mecab_destroy p

data Stat =
    -- | Normal node defined in the dictionary
    NOR
    -- | Unknown node not defined in the dictionary
  | UNK
    -- | Virtual node representing a beginning of the sentence
  | BOS
    -- | Virtual node representing a end of the N-best enumeration
  | EOS
    -- | Virtual node representing a end of the N-best enumeration
  | EON
  deriving (Eq, Read, Show)

-- The node type
data Node s = Node
  { -- | Surface string
    nodeSurface :: s
    -- | Feature string
  , nodeFeature :: s
    -- | Uength of the surface form including white space before the morph
  , nodeRlength :: Int
    -- | Unique node id
  , nodeId :: Int
    -- | Right attribute id
  , nodeRcAttr :: Int
    -- | Left attribute id
  , nodeLcAttr :: Int
    -- | Unique part of speech id
  , nodePosid :: Int
    -- | Character type
  , nodeCharType :: Int
    -- | Status of this model
  , nodeStat :: Stat
    -- | Is this node best?
  , nodeIsBest :: Bool
    -- | Forward accumulative log summation
  , nodeAlpha :: Double
    -- | backward accumulative log summation
  , nodeBeta :: Double
    -- | marginal probability
  , nodeProb :: Double
    -- | Word cost
  , nodeWcost :: Int
    -- | Best accumulative cost from bos node to this node
  , nodeCost :: Int
  } deriving (Eq, Read, Show)

peekNodes :: MeCabString s => Ptr (Node s) -> IO [Node s]
peekNodes ptr
  | ptr == nullPtr =
    return []
  | otherwise =
      (:) <$> peekNode ptr
          <*> (peekNodes =<< (#peek mecab_node_t, next) ptr)

peekNode :: MeCabString s => Ptr (Node s) -> IO (Node s)
peekNode ptr = do
  sfc <- do
    p <- (#peek mecab_node_t, surface) ptr
    len <- (#peek mecab_node_t, length) ptr
    fromBS <$> B.packCStringLen (p, fromIntegral (len :: CUShort))
  Node
    <$> return sfc
    <*> (packCString =<< (#peek mecab_node_t, feature) ptr)
    <*> ((fromIntegral :: CUShort -> Int) <$> (#peek mecab_node_t, rlength) ptr)
    <*> ((fromIntegral :: CUInt   -> Int) <$> (#peek mecab_node_t, id) ptr)
    <*> ((fromIntegral :: CUShort -> Int) <$> (#peek mecab_node_t, rcAttr) ptr)
    <*> ((fromIntegral :: CUShort -> Int) <$> (#peek mecab_node_t, lcAttr) ptr)
    <*> ((fromIntegral :: CUShort -> Int) <$> (#peek mecab_node_t, posid) ptr)
    <*> ((fromIntegral :: CUChar  -> Int) <$> (#peek mecab_node_t, char_type) ptr)
    <*> (toStat <$> (#peek mecab_node_t, stat) ptr)
    <*> ((==(1::CUChar)) <$> (#peek mecab_node_t, isbest) ptr)
    <*> ((realToFrac :: CFloat -> Double) <$> (#peek mecab_node_t, alpha) ptr)
    <*> ((realToFrac :: CFloat -> Double) <$> (#peek mecab_node_t, beta) ptr)
    <*> ((realToFrac :: CFloat -> Double) <$> (#peek mecab_node_t, prob) ptr)
    <*> ((fromIntegral :: CShort  -> Int) <$> (#peek mecab_node_t, wcost) ptr)
    <*> ((fromIntegral :: CLong   -> Int) <$> (#peek mecab_node_t, cost) ptr)
  where
    toStat :: CUChar -> Stat
    toStat (#const MECAB_NOR_NODE) = NOR
    toStat (#const MECAB_UNK_NODE) = UNK
    toStat (#const MECAB_BOS_NODE) = BOS
    toStat (#const MECAB_EOS_NODE) = EOS
    toStat (#const MECAB_EON_NODE) = EON
    toStat _ = UNK

data MeCabError =
  MeCabError String
  deriving (Eq, Ord, Show, Typeable)

instance Exception MeCabError

-- | Initializing MeCab by passing command-line args
new :: [String] -> IO MeCab
new args =
  withCStrings args $ \argc argv -> do
    p <- mecab_new (fromIntegral argc) argv
    when (p == nullPtr) $
      throwIO =<< (MeCabError <$> strerror nullPtr)
    mkMeCab p

-- | Initializing MeCab by passing concatenated command-line args
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

-- | Get MeCab version
version :: IO String
version =
  peekCString =<< mecab_version

strerror :: Ptr MeCab -> IO String
strerror p =
  peekCString =<< mecab_strerror p

-- String Types

class MeCabString s where
  toBS :: s -> B.ByteString
  fromBS :: B.ByteString -> s

instance MeCabString String where
  toBS = toBS . T.pack
  fromBS = T.unpack . fromBS

instance MeCabString B.ByteString where
  toBS = id
  fromBS = id

instance MeCabString T.Text where
  toBS = T.encodeUtf8
  fromBS = T.decodeUtf8

-- Parsing

-- | Parse given string and obtain results as a string format
parse :: MeCabString s => MeCab -> s -> IO s
parse m txt = withForeignPtr (unMeCab m) $ \pm ->
  B.useAsCStringLen (toBS txt) $ \(pstr, len) -> do
    p <- mecab_sparse_tostr2 pm pstr (fromIntegral len)
    when (p == nullPtr) $ throwIO =<< (MeCabError <$> strerror pm)
    packCString p

-- | Parse given string and obtain results as a nodes
parseToNodes :: MeCabString s => MeCab -> s -> IO [Node s]
parseToNodes m txt = withForeignPtr (unMeCab m) $ \pm ->
  B.useAsCStringLen (toBS txt) $ \(pstr, len) -> do
    p <- mecab_sparse_tonode2 pm pstr (fromIntegral len)
    when (p == nullPtr) $ throwIO =<< (MeCabError <$> strerror pm)
    peekNodes p

-- N-best parsing

-- | Parse given string and obtain whole N-best results as a string format
parseNBest :: MeCabString s => MeCab -> Int -> s -> IO s
parseNBest m n txt = withForeignPtr (unMeCab m) $ \pm ->
  B.useAsCStringLen (toBS txt) $ \(pstr, len) -> do
    p <- mecab_nbest_sparse_tostr2 pm (fromIntegral n) pstr (fromIntegral len)
    when (p == nullPtr) $ throwIO =<< (MeCabError <$> strerror pm)
    packCString p

-- | Parse given string and prepare obtaining N-best results
parseNBestInit :: MeCabString s => MeCab -> s -> IO ()
parseNBestInit m txt = withForeignPtr (unMeCab m) $ \pm ->
  B.useAsCStringLen (toBS txt) $ \(pstr, len) -> do
    ret <- mecab_nbest_init2 pm pstr (fromIntegral len)
    when (ret /= 1) $ throwIO =<< (MeCabError <$> strerror pm)

-- | Obtain next result as a string format
nBestNext :: MeCabString s => MeCab -> IO (Maybe s)
nBestNext m = withForeignPtr (unMeCab m) $ \pm -> do
  r <- mecab_nbest_next_tostr pm
  if r == nullPtr
    then return Nothing
    else Just <$> packCString r

-- | Obtain next result as a nodes
nBestNextNodes :: MeCabString s => MeCab -> IO (Maybe [Node s])
nBestNextNodes m = withForeignPtr (unMeCab m) $ \pm -> do
  p <- mecab_nbest_next_tonode pm
  if p == nullPtr
    then return Nothing
    else Just <$> peekNodes p

packCString :: MeCabString s => CString -> IO s
packCString p = fromBS <$> B.packCString p

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
  :: Ptr MeCab -> CString -> CSize -> IO (Ptr (Node a))

foreign import ccall mecab_nbest_next_tonode
  :: Ptr MeCab -> IO (Ptr (Node a))

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
