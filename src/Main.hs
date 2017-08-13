{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Avro
import           Data.Avro.Schema
import qualified Data.Avro.Types    as AT
import           Data.List.NonEmpty
import           Data.Monoid        ((<>))
import           Data.Text

data Message = Message
  { messageTo   :: Maybe Text
  , messageText :: Text
  } deriving (Show, Eq)

messageSchema :: Schema
messageSchema =
  Record "Message" Nothing [] Nothing Nothing
    [ fld "to"        (mkUnion $ Null :| [String])  Nothing
    , fld "text"      String                        Nothing
    ]
    where
    fld nm ty def = Field nm [] Nothing Nothing ty def

instance ToAvro Message where
  schema = pure messageSchema
  toAvro msg = record messageSchema
            [ "to"         .= messageTo msg
            , "text"       .= messageText msg
            ]

instance FromAvro Message where
  fromAvro (AT.Record _ r) =
    Message <$> r .: "to"
            <*> r .: "text"
  fromAvro r = badValue r "Message"


main :: IO ()
main =
  let msg = Message (Just "foo") "I love you"
  in mapM_ (assert msg) [1..20]


assert :: Message -> a -> IO ()
assert msg _ = do
  let res = roundtrip msg
  case res of
    Left err -> do
      putStrLn $ "Expected: " <> show msg
      putStrLn $ "Actual:   " <> show err
      error err
    Right msg' | msg /= msg' -> do
      putStrLn $ "Expected: " <> show msg
      putStrLn $ "Actual:   " <> show msg'
      putStrLn "-------------------------------------------------------------"
    _ -> return ()

roundtrip :: Message -> Either String Message
roundtrip msg = resultToEither $ decode (schemaOf msg) (encode msg)

resultToEither :: Result a -> Either String a
resultToEither (Success a) = Right a
resultToEither (Error e)   = Left e
