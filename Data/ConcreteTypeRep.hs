{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

{- |
This module defines 'Binary' and 'Hashable' instances for 'TypeRep'. These are defined on a newtype of 'TypeRep', namely 'ConcreteTypeRep', for two purposes:

  * to avoid making orphan instances

  * the 'Hashable' instance for 'ConcreteTypeRep' may not be pure enough for some people's tastes.

As usual with 'Typeable', this module will typically be used with some variant of @Data.Dynamic@. Two possible uses of this module are:

  * making hashmaps: @HashMap 'ConcreteTypeRep' Dynamic@

  * serializing @Dynamic@s.

-}

module Data.ConcreteTypeRep (
  ConcreteTypeRep,
  cTypeOf,
  toTypeRep,
  fromTypeRep,
 ) where

import Data.Typeable
import Data.Hashable
import Data.Binary

import System.IO.Unsafe

import Control.Applicative((<$>))

-- | Abstract type providing the functionality of 'TypeRep', but additionally supporting hashing and serialization. 
--
-- The 'Eq' instance is just the 'Eq' instance for 'TypeRep', so an analogous guarantee holds: @'cTypeOf' a == 'cTypeOf' b@ if and only if @a@ and @b@ have the same type.
-- The hashing and serialization functions preserve this equality.
newtype ConcreteTypeRep = CTR { unCTR :: TypeRep } deriving(Eq, Typeable)

-- | \"Concrete\" version of 'typeOf'.
cTypeOf :: Typeable a => a -> ConcreteTypeRep
cTypeOf = fromTypeRep . typeOf

-- | Converts to the underlying 'TypeRep'
toTypeRep :: ConcreteTypeRep -> TypeRep
toTypeRep = unCTR

-- | Converts from the underlying 'TypeRep'
fromTypeRep :: TypeRep -> ConcreteTypeRep
fromTypeRep = CTR

-- show as a normal TypeRep
instance Show ConcreteTypeRep where
  showsPrec i = showsPrec i . unCTR

-- | The 'Hashable' instance is defined by running 'unsafePerformIO' on @'typeRepKey' :: 'TypeRep' -> IO Int@. 
--
-- This instance actually provides a stronger guarantee than required: it is guaranteed that @t1 == t2@ if and only if @'hash' t1 == 'hash' t2@.
--
-- As the documentation for 'typeRepKey' notes, \"... the actual value of the key may vary from run to run of the program. You should only rely on the equality property, not any actual key value. The relative ordering of keys has no meaning either.\"
instance Hashable ConcreteTypeRep where
  hash = unsafePerformIO . typeRepKey . toTypeRep

------------- serialization: this uses Gökhan San's construction, from
---- http://www.mail-archive.com/haskell-cafe@haskell.org/msg41134.html
newtype SerialRep = SR (String, [SerialRep]) deriving(Binary)

toSerial :: ConcreteTypeRep -> SerialRep
toSerial (CTR t) = 
  case splitTyConApp t of
    (con, args) -> SR (tyConString con, map (toSerial . CTR) args)

fromSerial :: SerialRep -> ConcreteTypeRep
fromSerial (SR (con, args)) = CTR $ mkTyConApp (mkTyCon con) (map (unCTR . fromSerial) args)

instance Binary ConcreteTypeRep where
  put = put . toSerial
  get = fromSerial <$> get
