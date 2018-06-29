{-# LANGUAGE OverloadedStrings #-}
module Serialize where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L


import DataTypes
import Views

serialize :: GameView -> L.ByteString -- TODO(toby): change when Views is a thing
serialize gs = ""
