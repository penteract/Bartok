{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Serialize where

import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Aeson (ToJSON,toEncoding,genericToEncoding,defaultOptions,toJSON,encode)

import DataTypes
import Views

deriving instance Generic GameView
deriving instance Generic CardView
deriving instance Generic Rank
deriving instance Generic Suit

instance ToJSON Rank where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Suit where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON CardView where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON GameView where
  toEncoding = genericToEncoding defaultOptions

serialize :: GameView -> L.ByteString
serialize = encode . toJSON

unserialize :: L.ByteString -> Event
unserialize = undefined
