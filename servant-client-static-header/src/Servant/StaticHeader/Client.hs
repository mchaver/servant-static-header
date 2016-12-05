{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.StaticHeader.Client (
   module Servant.StaticHeader
 , module Servant.StaticHeader.Client
) where

import Control.Concurrent (forkIO)
import Data.Proxy
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.TypeLits
import Servant
import Servant.Client
import qualified Servant.Common.Req as Req
import Servant.StaticHeader

instance (KnownSymbol fieldName, KnownSymbol fieldValue, HasClient api) => HasClient (StaticHeader fieldName fieldValue :> api) where
  type Client (StaticHeader fieldName fieldValue :> api) = Client api
  clientWithRoute Proxy req = 
    clientWithRoute (Proxy :: Proxy api) (Req.addHeader fieldNameStr fieldValueStr req)
    where
       fieldNameStr = fromString $ symbolVal (Proxy :: Proxy fieldName)
       fieldValueStr = decodeUtf8 $ fromString $ symbolVal (Proxy :: Proxy fieldValue)
