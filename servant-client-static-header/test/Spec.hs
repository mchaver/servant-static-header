{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Servant.Server
import Servant.StaticHeader.Client
import Servant.StaticHeader.Server
import Test.Hspec

-- type Creatubbles = "v2" :> "creations" :> StaticHeader "User-Agent" "second" :> Capture "id" Text :> Get '[JSON] Text

testAPI :: Proxy TestAPI
testAPI = Proxy

app :: Application
app = serve testAPI testServer

type TestAPI = "single"   :> StaticHeader "User-Agent" "Hspec" :> Get '[JSON] Text
          :<|> "multiple" :> StaticHeader "User-Agent" "Hspec" :> StaticHeader "TestData" "abc" :> Get '[JSON] [Text]

getStaticHeader :: ClientM Text
getMultipleStaticHeaders :: ClientM [Text]
getStaticHeader :<|> getMultipleStaticHeaders = client testAPI

{-
type TT = "x" :> Get '[JSON] Text

ttAPI :: Proxy TT
ttAPI = Proxy

getTT :: ClientM Text
getTT = client ttAPI
-}

-- test server
testServer :: Server TestAPI
testServer = getStaticHeaderH :<|> getMultipleStaticHeadersH
  where
    getStaticHeaderH h = liftIO $ getStaticHeader h
    getMultipleStaticHeadersH h1 h2 = liftIO $ getMultipleStaticHeaders h1 h2
    
    getStaticHeader :: Maybe Text -> IO Text
    getStaticHeader (Just h) = return h
    getStaticHeader _ = undefined

    getMultipleStaticHeaders :: Maybe Text -> Maybe Text -> IO [Text]
    getMultipleStaticHeaders (Just h1) (Just h2) = return [h1,h2]
    getMultipleStaticHeaders _ _ = undefined

spec :: Spec
spec = do
  runIO $ forkIO $ runSettings (setPort 3000 $ defaultSettings) app
  manager <- runIO $ newManager defaultManagerSettings
  describe "StaticHeader" $ do
    it "gets the static header it passes" $ do
      res <- runClientM getStaticHeader (ClientEnv manager (BaseUrl Http "localhost" 3000 ""))
      case res of 
        Left err -> do
          putStrLn $ "Error: " ++ show err
          True `shouldBe` False
        Right h -> h `shouldBe` "Hspec"
    it "gets the multiple static headers it passes" $ do
      res <- runClientM getMultipleStaticHeaders (ClientEnv manager (BaseUrl Http "localhost" 3000 ""))
      case res of 
        Left err -> do
          putStrLn $ "Error: " ++ show err
          True `shouldBe` False
        Right h -> h `shouldBe` ["Hspec","abc"]
        

main :: IO ()
main = hspec spec
