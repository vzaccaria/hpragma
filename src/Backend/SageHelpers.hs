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
import           Data.List.Split
import           Data.Proxy
import           GHC.TypeLits
import           Language.Combinators
import           Language.Operators
import           Numeric              (readHex)
import           Prelude              (IO (..), Int, Integer, Show, String, fst,
                                       head, iterate, map, mod, putStrLn, quot,
                                       return, show, undefined, ($), (*), (+),
                                       (++), (-), (.))
import           System.Process
import           Utils.External

ex = "3243f6a8885a308d313198a2e0370734"

key = "2b7e151628aed2a6abf7158809cf4f3c"



iden n = "identity_matrix(" ++ show n ++ ")"

fromHaskellList mx = "Matrix(" ++ (show mx) ++ ")"

permutation l = "Permutation(" ++ (show $ l) ++ ").to_matrix()"

fromGF2 mx = "map_threaded(lambda i:ZZ(i._int_repr())," ++ mx ++ ")"


toGF2 intMatrix =
  let poli = "GF(256,'x', modulus=x^8 + x^4 + x^3 + x + 1)"
      convertedToGF = toGF intMatrix
      toGF m =
        "map_threaded(lambda i: " ++ poli ++ ".fetch_int(i)," ++ m ++ ")"
  in convertedToGF

tran v = "transpose(" ++ v ++ ")"

print s = "print " ++ s ++ ".str();"

sage :: String -> IO String
sage v =
  do
  (output, _) <- getProcessOutput ("sage -c '" ++ v ++ "'");
  return output;
