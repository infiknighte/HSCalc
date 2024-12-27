module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import HSCalc (hsCalc, prettyString)
import System.IO qualified as Sys.IO (hFlush, stdout)

main :: IO ()
main = repl

repl :: IO ()
repl = repl' 0
  where
    repl' :: Double -> IO ()
    repl' ans = do
      putStr "> "
      Sys.IO.hFlush Sys.IO.stdout
      input <- getLine
      let input' = replaceAll "@" (show ans) (dropWhileEnd isSpace $ dropWhile isSpace input)
       in case input' of
            ":q" -> return ()
            ":ans" -> do
              print ans
              repl' ans
            _ -> case hsCalc input' of
              Left err -> do
                putStrLn $ prettyString err
                repl' 0
              Right result -> do
                print result
                repl' result

replaceAll :: String -> String -> String -> String
replaceAll _ _ [] = []
replaceAll from to str = replace' str
  where
    replace' s
      | from `isPrefixOf` s = to ++ replace' (drop (length from) s)
      | otherwise = case s of
          [] -> []
          c : cs -> c : replace' cs
