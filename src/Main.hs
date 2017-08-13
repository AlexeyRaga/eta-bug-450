{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)

main :: IO ()
main =
  mapM_ assertText (replicate 20 "I love you")

assertText :: Text -> IO ()
assertText msg = do
  let res = decodeUtf8 $ encodeUtf8 msg
  if res /= msg
    then do
      putStrLn $ "Expected: " <> show msg
      putStrLn $ "Actual:   " <> show res
      putStrLn "-------------------------------------------------------------"
    else return ()

