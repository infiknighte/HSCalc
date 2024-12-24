module Main (main) where

import HSCalc (hsCalc)

main :: IO ()
main = print $ hsCalc "1 + 1" == Right 2