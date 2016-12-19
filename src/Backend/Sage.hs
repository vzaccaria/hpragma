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
import           Prelude            (IO (..), Int, Show, String, iterate, map,
                                     mod, putStrLn, quot, show, undefined, ($),
                                     (*), (+), (++), (-), (.))

import Backend.SageData
import Backend.SageHelpers

import System.Process

data family Sa a
data instance Sa (M a b)= S { unS :: String }

instance Symantics Sa where
  l n _ I    = S $ (toGF2 . iden $ natVal n)
  l _ _ MC_4 = S $ toGF2 $ fromHaskellList [[2, 3, 1, 1], [1, 2, 3, 1], [1, 1, 2, 3], [3, 1, 1, 2]]
  l _ _ AR   = S $ toGF2 $ fromHaskellList [[1, 1]]
  l _ _ PSR  = S $ toGF2 $ fromHaskellList shiftRowMatrix
  l _ _ PMC  = S $ toGF2 $ stridePermutation 4 16
  l _ _ SBOX = S $ toGF2 $ fromHaskellList _sboxData

  a <*> b    = S $ unS a ++ " * " ++ unS b
  a <**> b   = S $ unS a ++ ".tensor_product(" ++ unS b ++ ")"

tbox = (i4 <**> mc) <*> pmc <*> sr 

printtbox = sage (print $ unS tbox)
