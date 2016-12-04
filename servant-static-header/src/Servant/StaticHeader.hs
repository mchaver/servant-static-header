{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Servant.StaticHeader where
  
import           GHC.TypeLits

data StaticHeader (fieldName :: Symbol) (fieldValue :: Symbol)
