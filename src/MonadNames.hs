{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MonadNames where

import Control.Monad.Trans.Class (lift, MonadTrans(..))
import Control.Monad.Identity
import Control.Monad.State

import Utility

class Monad m => MonadNames s1 s2 m | m -> s1, m -> s2 where
    nextVariable :: m s1
    nextLabel :: m s2
    popVariable :: m s1
    popLabel :: m s2

newtype NamesT s1 s2 m a = NamesT (StateT ([s1], [s2]) m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

newtype Names s1 s2 a = Names (NamesT s1 s2 Identity a)
  deriving (Functor, Applicative, Monad, MonadNames s1 s2, MonadFix)

instance Monad m => MonadNames s1 s2 (NamesT s1 s2 m) where
    nextVariable = NamesT $ do
        ((v:_), _) <- get
        return v
    nextLabel = NamesT $ do
        (_, (l:_)) <- get
        return l
    popVariable = NamesT $ do
        ((v:vs), ls) <- get
        put (vs, ls)
        return v
    popLabel = NamesT $ do
        (vs, (l:ls)) <- get
        put (vs, ls)
        return l


evalNamesT :: Monad m => NamesT s1 s2 m a -> [s1] -> [s2] -> m a
evalNamesT (NamesT s) xs ys = evalStateT s (xs, ys)

evalNames :: Names s1 s2 a -> [s1] -> [s2] -> a
evalNames (Names s) = runIdentity ... evalNamesT s

runNamesT :: Monad m => NamesT s1 s2 m a -> [s1] -> [s2] -> m (a, ([s1], [s2]))
runNamesT (NamesT s) xs ys = runStateT s (xs, ys)

runNames :: Names s1 s2 a -> [s1] -> [s2] -> (a, ([s1], [s2]))
runNames (Names s) = runIdentity ... runNamesT s