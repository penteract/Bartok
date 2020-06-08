{-# LANGUAGE LambdaCase #-}

module Game.Bartok.Rules
  ( r8,
    rq,
    rlast,
    rMao,
    getState,
    rMao'',
    ifSaid,
    r7',
    defaultRules,
    defaultRulesNamed,
  )
where

import Control.Applicative (liftA2)
import Control.Lens (at, (%~), (^.))
import Data.Bifunctor (second)
import Data.Maybe (isJust)
import Game.Bartok.DataTypes
  ( Action (..),
    Event (..),
    Game,
    GameState,
    Rank (Eight, Queen, Seven),
    Rule,
    Step,
    hands,
    lastMoveLegal,
    modifyVar,
    players,
    rank,
    readVar,
    setVar,
    winner,
    _winner,
  )
import Game.Bartok.RuleHelpers
  ( banPhrase,
    doAfter,
    doNothing,
    findInMs,
    illegal,
    isTurn,
    mustSay,
    nextTurn,
    onAction,
    onLegalCard,
    onPlay,
    penalty,
    removeAllN,
    split,
    when',
    with,
    with',
  )
import Utils (applyWhen)

r8 :: Rule
r8 =
  onLegalCard
    ( \card _ ->
        applyWhen (rank card == Eight) nextTurn
    )

reverseDirection :: Step
reverseDirection = players %~ uncurry (++) . second reverse . splitAt 1

-- | Reverse direction on q
rq :: Rule
rq =
  onPlay
    ( \c act e gs ->
        let altgs' = act e (reverseDirection gs)
            gs' = act e gs
         in if rank c == Queen && altgs' ^. lastMoveLegal && gs' ^. lastMoveLegal then altgs' else gs'
    )

rlast :: Rule
rlast =
  onLegalCard
    ( \_ e gs' ->
        case e of
          Action p _ m ->
            if maybe False ((== 1) . length) (gs' ^. hands . at p) && not ("last card" `findInMs` m)
              then penalty 1 "failure to declare \"last card\"" p gs'
              else gs'
          _ -> gs'
    )
    . banPhrase 1 "False \"last card\" pronouncement" (\(p, _, m) gs -> maybe False ((/= 1) . length) (gs ^. hands . at p) && ("last card" `findInMs` m))

-- TODO: make it order sensitive (Mao should only be sayable last)
-- rMao :: Rule
-- rMao act e gs = (onAction (\(p,a,m) act' e' gs'->
--                     if said m "mao" && (act' e' gs')^.winner /= Just p
--                         then illegal 3 ("Lying, cheating, stealing, deceiving, taking the name of the Chairman in vain.") e' gs'
--                         else act' e' gs' )
--               . onLegalCard
--                     (\card e'@(Action p _ m) gs'->
--                         if gs'^.winner == Just p && not (said m "mao")
--                             then illegal 1 ("Failure to declare Mao!") e gs
--                             else gs' )) act e gs

rMao :: Rule
rMao =
  onAction
    ( \(p, _, m) act e gs ->
        let next = act e gs
            won = next ^. winner == Just p
            saidmao = "mao" `findInMs` m
            saidmaolast = "mao" `findInMs` last ("" : split m)
         in case (saidmao, won && saidmaolast, won) of
              (True, False, _) ->
                penalty
                  4
                  "Lying, cheating, deceiving, taking the name of the Chairman in vain."
                  p
                  next
              (False, _, True) ->
                act
                  e
                  (penalty 1 "Failure to declare Mao!" p gs)
              _ -> next
    )

ifSaid :: String -> Game -> Game
ifSaid s g =
  \case
    e@(Action _ _ m) | s `findInMs` m -> g e
    _ -> id

getState :: Event -> GameState -> GameState
getState = flip const

rMao'' :: Rule
rMao'' =
  onAction
    ( \(p, _, _) ->
        with
          (,)
          ( const $
              doAfter
                ( with'
                    (\_ gs -> _winner gs == Just p)
                    ( when' id (mustSay "mao")
                        ^^.^^ when'
                          not
                          ( ifSaid
                              "mao"
                              (illegal 3 ("Lying, cheating, stealing, deceiving, taking the name of the Chairman in vain."))
                          )
                    )
                )
          )
    )
  where
    (^^.^^) ::
      (Applicative f1, Applicative f2) =>
      f1 (f2 (b -> c)) ->
      f1 (f2 (a -> b)) ->
      f1 (f2 (a -> c))
    (^^.^^) = liftA2 $ liftA2 $ (.)

r7' :: Rule
r7' =
  onAction
    ( \(p, a, m) act e gs ->
        let --f b = if b then 1 else 0 (this is fromEnum)
            count7 = readVar "sevens" gs
            veries = concat $ replicate count7 " very"
            veries' = concat $ replicate (count7 -1) " very"
            bidm = ("Have a" ++ veries ++ " nice day")
            thankm = "Thank you" ++ veries' ++ if count7 > 1 then " much" else ""
            i = fst $ removeAllN "(Have a( very)* nice day)|(Thank you( very)*( very much)?)" m
            bePolite :: Maybe String -> Step
            bePolite c =
              let pens = i - fromEnum (isJust c)
               in (if pens > 0 then penalty pens "Excessive politeness" p else doNothing)
                    . ( case c of
                          Just s -> mustSay s e
                          Nothing -> doNothing
                      )
            gs' = act e gs
         in case a of
              (Draw n)
                | count7 > 0,
                  isTurn p gs ->
                  if n == 2 * count7
                    then bePolite (Just thankm) . setVar "sevens" 0 $ gs'
                    else bePolite (Just thankm) $ illegal 1 ("Failure to draw " ++ show (2 * count7) ++ " cards.") e gs
              (Play c)
                | gs' ^. lastMoveLegal,
                  rank c == Seven ->
                  bePolite (Just bidm) . modifyVar "sevens" (+ 1) $ gs'
              (Play _)
                | gs' ^. lastMoveLegal,
                  count7 > 0 -> -- rank c /= Seven
                  bePolite (Just thankm) $ illegal 1 ("Failure to draw " ++ show (2 * count7) ++ " cards.") e gs
              _ -> bePolite Nothing gs'
    )

defaultRules :: [Rule]
defaultRules = [r7', rlast, r8, rq, rMao]

defaultRulesNamed :: [(String, Rule)]
defaultRulesNamed = [("r7'", r7'), ("rLastCard", rlast), ("r8", r8), ("rq", rq), ("rMao", rMao)]
