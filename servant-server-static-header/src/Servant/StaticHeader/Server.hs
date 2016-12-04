{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.StaticHeader.Server where

import Data.Proxy
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.TypeLits
import Servant
import Servant.StaticHeader
import Servant.Server.Internal.RoutingApplication
import Network.Wai (requestHeaders)

  
instance (KnownSymbol fieldName, KnownSymbol fieldValue, HasServer api context) => HasServer (StaticHeader fieldName fieldValue :> api) context where
  type ServerT (StaticHeader fieldName fieldValue :> api) m = Maybe Text -> ServerT api m
  
  route Proxy context subserver = 
    route (Proxy :: Proxy api) context (passToServer subserver mheader)
    where
      fieldNameStr  = fromString $ symbolVal (Proxy :: Proxy fieldName)
      fieldValueStr = fromString $ symbolVal (Proxy :: Proxy fieldValue)
      -- return fieldValue if fieldValueStr found in request headers and if 
      -- the preset fieldValueStr is equivalent to the header from the 
      -- client request.
      mheader req =  
        let mresult = lookup fieldNameStr (requestHeaders req)
        in case mresult of
          Nothing -> Nothing
          Just result -> if (decodeUtf8 result) == fieldValueStr then (Just fieldValueStr) else Nothing
