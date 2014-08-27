{-# LANGUAGE DeriveFunctor #-}
module Mgw.Util.Force (

    ForceMe, ForceMe_, getValue, forceLater, forceLater_, forceLaterAndReturn
  , zeroForce, zeroForce_

) where

data ForceMe a = ForceMe a deriving Functor

getValue :: ForceMe a -> a
getValue (ForceMe x) = x

type ForceMe_ = ForceMe ()

zeroForce_ :: ForceMe_
zeroForce_ = ForceMe ()

zeroForce :: a -> ForceMe a
zeroForce a = ForceMe a

forceLater_ :: a -> ForceMe_
forceLater_ a = a `seq` ForceMe ()

forceLater :: a -> b -> ForceMe b
forceLater a b = a `seq` ForceMe b

forceLaterAndReturn :: a -> ForceMe a
forceLaterAndReturn a = a `seq` ForceMe a
