module Tests where

import Lib
import Sample

testGame = GS { _deck = [(x,y) | x <- enumFrom minBound, y <- enumFrom minBound ]
              , _pile = []
              , _messages = []
              , _lastMoveLegal = True
              , _randg = mkStdGen 0
              , _varMap = Map.empty
              , _players = [("Angus",[]),("Toby",[]),("Anne",[])]
              , _prevGS = Nothing
              }
