{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Bartok.Serialize
  ( serialize,
    unserialize,
    readNewRule,
    ActionReq (..),
    Token,
    ClientPacket (..),
    NewRuleReq (..),
    getName,
    getTok,
    getCount,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    decode,
    encode,
    toJSON,
  )
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Game.Bartok.DataTypes (GameView, Name)

data ClientPacket = NewData Int GameView | NoNewData | Redirect String
  deriving (Show, Generic, ToJSON)

serialize :: ClientPacket -> L.ByteString
serialize = encode . toJSON

type Token = String

type CardIndex = Int

-- TODO: Consider adding some validation to player join requests (eg name can't contain silly characters)

-- | Requests contain a Name, a validation token, a counter indicating how much they haven't seen
--   and some data about the move being made
data ActionReq
  = ReqPlay Name Token Int CardIndex String
  | ReqDraw Name Token Int Int String
  | ReqJoin Name Token Int
  | ReqLeave Name Token Int
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data NewRuleReq = NewRuleReq
  { imports :: [String],
    ruleType :: String,
    code :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

readNewRule :: L.ByteString -> Maybe NewRuleReq
readNewRule = decode

getName :: ActionReq -> Name
getName (ReqPlay p _ _ _ _) = p
getName (ReqDraw p _ _ _ _) = p
getName (ReqJoin p _ _) = p
getName (ReqLeave p _ _) = p

getTok :: ActionReq -> Token
getTok (ReqPlay _ t _ _ _) = t
getTok (ReqDraw _ t _ _ _) = t
getTok (ReqJoin _ t _) = t
getTok (ReqLeave _ t _) = t

getCount :: ActionReq -> Int
getCount (ReqPlay _ _ n _ _) = n
getCount (ReqDraw _ _ n _ _) = n
getCount (ReqJoin _ _ n) = n
getCount (ReqLeave _ _ n) = n

unserialize :: L.ByteString -> Maybe ActionReq
unserialize = decode
