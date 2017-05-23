module Control.Concurrent.STM.TMapMVar.Hash where

import Data.Maybe (catMaybes)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (void)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, modifyTVar', readTVar)
import Control.Concurrent.STM.TMVar (TMVar, readTMVar, tryReadTMVar, newEmptyTMVar, takeTMVar, tryTakeTMVar, swapTMVar, putTMVar)


putForceTMVar :: TMVar a -> a -> STM ()
putForceTMVar t x = do
  q <- tryReadTMVar t
  case q of
    Nothing -> putTMVar t x
    Just _  -> () <$ swapTMVar t x


newtype TMapMVar k a = TMapMVar
  { getTMapMVar :: TVar (HashMap k (TMVar a))
  }


newTMapMVar :: STM (TMapMVar k a)
newTMapMVar = TMapMVar <$> newTVar HashMap.empty

keys :: TMapMVar k a -> STM [k]
keys (TMapMVar m) = HashMap.keys <$> readTVar m

peekElems :: TMapMVar k a -> STM [a]
peekElems (TMapMVar m) = do
  m' <- readTVar m
  let ts = HashMap.elems m'
  catMaybes <$> traverse tryReadTMVar ts


-- | Blocks if it's full
insert :: (Eq k, Hashable k) => TMapMVar k a -> k -> a -> IO ()
insert t k a = do
  x <- getTMVar t k
  atomically $ putTMVar x a

-- | Doesn't Block
insertForce :: (Eq k, Hashable k) => TMapMVar k a -> k -> a -> IO ()
insertForce t k a = do
  x <- getTMVar t k
  atomically $ putForceTMVar x a


-- | Blocks, and deletes upon looking it up
lookup :: (Eq k, Hashable k) => TMapMVar k a -> k -> IO a
lookup t k = do
  x <- getTMVar t k
  atomically $ takeTMVar x

tryLookup :: (Eq k, Hashable k) => TMapMVar k a -> k -> IO (Maybe a)
tryLookup t k = do
  x <- getTMVar t k
  atomically $ tryTakeTMVar x

-- | Blocks, but doesn't delete when looking it up
observe :: (Eq k, Hashable k) => TMapMVar k a -> k -> IO a
observe t k = do
  x <- getTMVar t k
  atomically $ readTMVar x

tryObserve :: (Eq k, Hashable k) => TMapMVar k a -> k -> IO (Maybe a)
tryObserve t k = do
  x <- getTMVar t k
  atomically $ tryReadTMVar x

delete :: (Eq k, Hashable k) => TMapMVar k a -> k -> IO ()
delete t k =
  void $ tryLookup t k



-- * Utils

getTMVar :: (Eq k, Hashable k) => TMapMVar k a -> k -> IO (TMVar a)
getTMVar (TMapMVar m) k = atomically $ do
  m' <- readTVar m
  case HashMap.lookup k m' of
    Nothing -> do
      t <- newEmptyTMVar
      modifyTVar' m (HashMap.insert k t)
      pure t
    Just t -> pure t
