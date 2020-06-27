Questions
==========

How to mix theory and examples?
Both RuleHelpers and TLib have been written in such a way that some example rules are straightforward.
For example, from TSample:
```
r8 = when (isLegal ~&~ cardIs ((==Eight) . rank)) (doAfter nextTurn)
```

However, without a good understanding of what is happening underneath, it is easy to make mistakes about what should happen - the above rule would behave differently if it used 'doBefore' rather than 'doAfter'.

At minimum, players should know the type of rule `(Event -> GameState -> GameState) -> Event -> GameState -> GameState`,
and what that means.
