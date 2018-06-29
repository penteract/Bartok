{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Serialize where

import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Aeson (ToJSON,toEncoding,genericToEncoding,defaultOptions,toJSON,encode,fromJSON,FromJSON,decode)

import DataTypes
import Views

deriving instance Generic GameView
deriving instance Generic CardView
deriving instance Generic Rank
deriving instance Generic Suit

deriving instance Generic Action
deriving instance Generic Event

instance ToJSON Rank where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Suit where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON CardView where -- TODO: Angus; go to unicode chars instead
  toEncoding = genericToEncoding defaultOptions
instance ToJSON GameView where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Action where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Event where
  toEncoding = genericToEncoding defaultOptions

  
instance FromJSON Suit
instance FromJSON Rank
instance FromJSON Action
instance FromJSON Event

serialize :: GameView -> L.ByteString
serialize = encode . toJSON

unserialize :: L.ByteString -> Maybe Event
unserialize = decode
