{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Serialize where

import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T (unpack)
import Data.Aeson (FromJSON,ToJSON,defaultOptions,decode,encode,fromJSON,genericToEncoding,toEncoding,toJSON)

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
instance ToJSON CardView where
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

data ActionReq = ReqPlay PlayerIndex Int String | ReqDraw PlayerIndex Int String | ReqJoin Name deriving (Show,Eq,Generic)
instance ToJSON ActionReq where toEncoding = genericToEncoding defaultOptions
instance FromJSON ActionReq

unserialize :: L.ByteString -> Maybe ActionReq
unserialize = decode
-- unserialize s = do
--   name <- s ^? key "name" . _String . to T.unpack
--   action <- s ^? key "action" . _String
--   let messages = fromMaybe "" (s ^? key "messages" . _String . to T.unpack)
--   case action of
--     "draw" -> s ^? key "number" . _Integral >>= (\n -> return $ Action name (Draw n) messages)
--     "join" -> return $ PlayerJoin name
--     "play" -> do suit <- join $ s ^? key "card" . key "suit" . _String . to (readMaybe . showText)
--                  rank <- join $ s ^? key "card" . key "rank" . _String . to (readMaybe . showText)
--                  return $ Action name (Play (rank,suit)) messages
--     _ -> Nothing
