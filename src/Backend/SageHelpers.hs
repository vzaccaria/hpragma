{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Backend.SageHelpers where

import           Control.Arrow
import           Data.List
import           Data.Proxy
import           GHC.TypeLits
import           Language.Operators
import           Prelude            (IO (..), Int, Integer, Show, String,
                                     iterate, map, mod, putStrLn, quot, show,
                                     undefined, ($), (*), (+), (++), (-))

import           Backend.SageData
import           Data.List.Split
import           System.Process

ex :: String
ex = "3243f6a8885a308d313198a2e0370734"

key :: String
key = "2b7e151628aed2a6abf7158809cf4f3c"

fromHexString v = let
  r = intercalate "," $ map (\x -> "Integer(0x" ++ x ++ ")") $ chunksOf 2 v
  in
    "Matrix([" ++ r ++ "])"



-- fromHexInteger
-- fromHexInteger n =

iden n                = "identity_matrix(" ++ show n ++ ")"
fromHaskellList mx    = "Matrix(" ++ (show mx) ++ ")"
stridePermutation s n = "Permutation(" ++ (show $ __stride s n) ++ ").to_matrix()"
fromGF2 mx            = "map_threaded(lambda i:ZZ(i._int_repr())," ++ mx ++ ")"

toGF2 intMatrix = let
  poli = "GF(256,'x', modulus=x^8 + x^4 + x^3 + x + 1)"
  convertedToGF = toGF intMatrix
  toGF m = "map_threaded(lambda i: " ++ poli ++ ".fetch_int(i)," ++ m ++ ")"
  in
    convertedToGF

tran v = "transpose(" ++ v ++ ")"

prepareText t = stridePermutation 4 16 ++ " * " ++ tran t

print s = "print " ++ s ++ ";"

sage :: String -> IO ()
sage v = callCommand ("sage -c '" ++ v ++ "'")

