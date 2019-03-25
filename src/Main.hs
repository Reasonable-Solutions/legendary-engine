{-# Language ScopedTypeVariables #-}
module Main where

main :: IO ()
main = let
         (x :: IO String) = getLine
       in do
         (line :: String) <- x :: IO String
         (putStrLn :: String -> IO ()) (line :: String)
