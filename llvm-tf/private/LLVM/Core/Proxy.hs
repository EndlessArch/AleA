module LLVM.Core.Proxy where

import Control.Applicative (Applicative, pure, (<*>), )

data Proxy a = Proxy

instance Functor Proxy where
   fmap _f Proxy = Proxy

instance Applicative Proxy where
   pure _ = Proxy
   Proxy <*> Proxy = Proxy


fromValue :: a -> Proxy a
fromValue _ = Proxy

element :: Proxy (f a) -> Proxy a
element Proxy = Proxy
