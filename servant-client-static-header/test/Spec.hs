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

type TestAPI = "test" :> StaticHeader "User-Agent" "Hspec" :> Get '[JSON] Text

getStaticHeader :: ClientM Text
getStaticHeader = client testAPI

{-
type TT = "x" :> Get '[JSON] Text

ttAPI :: Proxy TT
ttAPI = Proxy

getTT :: ClientM Text
getTT = client ttAPI
-}

-- test server
testServer :: Server TestAPI
testServer = getStaticHeaderH
  where
    getStaticHeaderH h = liftIO $ getStaticHeader h
    
    getStaticHeader :: Maybe Text -> IO Text
    getStaticHeader (Just h) = return h
    getStaticHeader _ = undefined

spec :: Spec
spec = do
  runIO $ forkIO $ runSettings (setPort 3000 $ defaultSettings) app
  manager <- runIO $ newManager defaultManagerSettings
  describe "" $ do
    it "" $ do
      res <- runClientM getStaticHeader (ClientEnv manager (BaseUrl Http "localhost" 3000 ""))
      case res of 
        Left err -> do
          putStrLn $ "Error: " ++ show err
          True `shouldBe` False
        Right h -> h `shouldBe` "Hspec"
        

main :: IO ()
main = hspec spec
