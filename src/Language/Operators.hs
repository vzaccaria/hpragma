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
import           Prelude       (IO (..), Int, Show, String, putStrLn, show, ($),
                                (++), undefined)

data M (a :: Nat) (b :: Nat) = M ()

data Const = MC_4 | SBOX | PSR | PMC | I | AR deriving (Show)

infixl 8 <*>
infixl 4 <**>

class Symantics r where
  dec    :: forall n. (KnownNat n) => Proxy n -> r (M n 1) -> r (M ((*) 256 n) 1)
  l      :: (KnownNat a, KnownNat b) => Proxy a -> Proxy b -> Const -> r (M a b)
  (<*>)  :: (KnownNat a, KnownNat b, KnownNat c) => r (M a b) -> r (M b c) -> r (M a c)
  (<**>) :: (KnownNat a, KnownNat b, KnownNat c, KnownNat d) => r (M a b) -> r (M c d) -> r (M ((*) a c) ((*) b d))

type IMAT r n = ((Symantics r) => r (M n n))
type GMAT r a b = ((Symantics r) => r (M a b))

liftMx m a = (m <*> a)

n16        = Proxy :: Proxy 16
n1         = Proxy :: Proxy 1
n2         = Proxy :: Proxy 2
n4         = Proxy :: Proxy 4
n256       = Proxy :: Proxy 256

ss         = l n1 n2 AR     :: GMAT r 1 2
sr         = l n16 n16 PSR  :: GMAT r 16 16
pmc        = l n16 n16 PMC  :: GMAT r 16 16
mc         = l n4 n4 MC_4   :: GMAT r 4 4
i4         = l n4 n4 I      :: IMAT r 4
i16        = l n16 n16 I    :: IMAT r 16
s          = l n1 n256 SBOX :: GMAT r 1 256

b2         = liftMx $ (i4 <**> mc) <*> pmc <*> sr
b1         = liftMx (i16 <**> s)
p1         = liftMx $ (i16 <**> ss)

aes        = p1 >>> dec n16 >>> b1 >>> b2


