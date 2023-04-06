module Main where

import Data.ByteString.Builder (stringUtf8)
import Network.Wai (Application, responseBuilder)
import Network.Wai.Handler.Warp (runEnv)
import Network.HTTP.Types.Status (status200)

app :: Application
app req reply = do
  reply $ responseBuilder status200 [] $ stringUtf8 "hello"

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  runEnv 1974 app
