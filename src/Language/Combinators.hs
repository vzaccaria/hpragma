{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Language.Combinators where  

import           Control.Arrow
import           Data.Proxy
import           GHC.TypeLits
import           Language.Data
import           Language.Operators
import           Prelude       (IO (..), Int, Integer, Show, String, putStrLn,
                                show, undefined, ($), (++))

liftMx m a = (m <*> a)

n16        = Proxy :: Proxy 16
n1         = Proxy :: Proxy 1
n2         = Proxy :: Proxy 2
n4         = Proxy :: Proxy 4
n256       = Proxy :: Proxy 256

ak         = l n1 n2   $ LIT [ [ 1, 1 ] ]                                             :: GMAT r 1 2
sr         = l n16 n16 $ LIT shiftRowMatrix                                           :: GMAT r 16 16
pmc        = l n16 n16 $ PERM $ stride 4 16                                           :: GMAT r 16 16
mc         = l n4 n4   $ LIT [[2, 3, 1, 1], [1, 2, 3, 1], [1, 1, 2, 3], [3, 1, 1, 2]] :: GMAT r 4 4
i4         = l n4 n4 I                                                                :: IMAT r 4
i16        = l n16 n16 I                                                              :: IMAT r 16
sb         = l n1 n256 $ LIT [ _sboxData ]                                            :: GMAT r 1 256

b2         = liftMx $ (i4 <**> mc) <*> pmc <*> sr
b1         = liftMx (i16 <**> sb)
p1         = liftMx $ (i16 <**> ak)

aes        = p1 >>> dec n16 >>> b1 >>> b2


