import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Data.Binary(encode, decode)
import Data.Hashable(hash)
import Control.Applicative

import Data.Typeable
import Data.ConcreteTypeRep
import Data.Word

main = defaultMain tests

tests = [
  testGroup "Serialisation" [
     testProperty "roundtrip" prop_roundtrip
   ]
 ]

prop_roundtrip ty = (toTypeRep . decode . encode . fromTypeRep $ ty) == ty

-- instances of arbitrary
genTyCon :: Int -- ^ Number of arguments; must be in [0,1,2]
         -> Gen TyCon
genTyCon 0 = elements [tyConOf (__::Int), tyConOf (__::Word), tyConOf (__::Double), tyConOf (__::Bool)]
genTyCon 1 = elements [tyConOf (__::Maybe Int), tyConOf (__::IO Int), tyConOf (__::[Int])]
genTyCon 2 = elements [tyConOf (__::Either Int Int), tyConOf (__::Int -> Int)]

tyConOf ty = fst $ splitTyConApp (typeOf ty)
__ = undefined

instance Arbitrary TypeRep where
  arbitrary = do
    nargs <- elements [0,1,2]
    mkTyConApp <$> (genTyCon nargs) <*> (vectorOf nargs arbitrary)
