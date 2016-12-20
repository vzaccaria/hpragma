{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Backend.Sage where

import           Control.Arrow
import           Data.Proxy
import           GHC.TypeLits
import           Language.Operators
import           Language.Combinators
import           Prelude            (IO (..), Int, Show, String, iterate, map,
                                     mod, putStrLn, quot, show, undefined, ($),
                                     (*), (+), (++), (-), (.))

import Backend.SageHelpers

import System.Process

data family Sa a
data instance Sa (M a b)= S { unS :: String } 

instance Symantics Sa where
  l n _ I        = S $ (toGF2 . iden $ natVal n)
  l _ _ (LIT v)  = S $ toGF2 $ fromHaskellList v
  l _ _ (PERM v) = S $ toGF2 $ permutation v
  t x            = S $ tran (unS x)

  a <*> b    = S $ "(" ++ unS a ++ " * " ++ unS b ++ ")"
  a <**> b   = S $ unS a ++ ".tensor_product(" ++ unS b ++ ")"



apply m x = (liftMx m) x
emit x  = putStrLn (unS x)
eval x  = sage (print $ fromGF2 $ unS $ t x)

