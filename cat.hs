{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

newtype Identity a = Identity {runIdentity :: a} deriving (Functor, Show)

type NatTrans f g = forall a. f a -> g a

makelist :: NatTrans Identity []
makelist (Identity x) = [x]
