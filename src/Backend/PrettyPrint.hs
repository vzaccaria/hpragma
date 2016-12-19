{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Backend.PrettyPrint where

import           Control.Arrow
import           Data.Proxy
import           GHC.TypeLits
import           Language.Operators
import           Prelude            (IO (..), Int, Show, String, putStrLn, show,
                                     undefined, ($), (++))


data family Pr a
data instance Pr (M a b)= P { unP :: String }

instance Symantics Pr where
  dec v x  = P $ "dec_{" ++ (show (natVal v)) ++ "}(" ++ (unP x) ++ ")"
  a <*> b  = P $ (unP a) ++ " × " ++ (unP b)
  a <**> b = P $ "(" ++ (unP a) ++ " ⊗ " ++ (unP b) ++ ")"

px = putStrLn $ unP (aes $ P "input")

main :: IO ()
main = putStrLn "hello world"

