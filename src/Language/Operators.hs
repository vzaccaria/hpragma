{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Language.Operators where  

import           Control.Arrow
import           Data.Proxy
import           GHC.TypeLits
import           Language.Data
import           Prelude       (IO (..), Int, Integer, Show, String, putStrLn,
                                show, undefined, ($), (++))
data M (a :: Nat) (b :: Nat) = M ()

data Const = LIT [[Integer]] | PERM [Integer] | I deriving (Show)

infixl 8 <*>
infixl 4 <**>

class Symantics r where
  dec    :: forall n. (KnownNat n) => Proxy n -> r (M n 1) -> r (M ((*) 256 n) 1)
  l      :: (KnownNat a, KnownNat b) => Proxy a -> Proxy b -> Const -> r (M a b)
  t      :: (KnownNat a, KnownNat b) => r (M a b) -> r (M b a)
  (<*>)  :: (KnownNat a, KnownNat b, KnownNat c) => r (M a b) -> r (M b c) -> r (M a c)
  (<**>) :: (KnownNat a, KnownNat b, KnownNat c, KnownNat d) => r (M a b) -> r (M c d) -> r (M ((*) a c) ((*) b d))

type IMAT r n = ((Symantics r) => r (M n n))
type GMAT r a b = ((Symantics r) => r (M a b))



