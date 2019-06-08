module Whitelist(allowed) where

allowed :: String -> Bool
allowed = (`elem`[
  "doc/doc-index-I.html",
  "doc/mini_BaseGame.html",
  "doc/bartok.txt",
  "doc/BaseGame.html",
  "doc/DataTypes.html",
  "doc/doc-index-126.html",
  "doc/doc-index-37.html",
  "doc/doc-index-46.html",
  "doc/doc-index-47.html",
  "doc/doc-index-58.html",
  "doc/doc-index-94.html",
  "doc/doc-index-95.html",
  "doc/doc-index-A.html",
  "doc/doc-index-All.html",
  "doc/doc-index-B.html",
  "doc/doc-index-C.html",
  "doc/doc-index-D.html",
  "doc/doc-index-E.html",
  "doc/doc-index-F.html",
  "doc/doc-index-G.html",
  "doc/doc-index-H.html",
  "doc/doc-index-J.html",
  "doc/doc-index-K.html",
  "doc/doc-index-L.html",
  "doc/doc-index-M.html",
  "doc/doc-index-N.html",
  "doc/doc-index-O.html",
  "doc/doc-index-P.html",
  "doc/doc-index-Q.html",
  "doc/doc-index-R.html",
  "doc/doc-index-S.html",
  "doc/doc-index-T.html",
  "doc/doc-index-U.html",
  "doc/doc-index-V.html",
  "doc/doc-index-W.html",
  "doc/doc-index.html",
  "doc/doc-index.json",
  "doc/haddock-bundle.min.js",
  "doc/haddock-util.js",
  "doc/hslogo-16.png",
  "doc/index.html",
  "doc/meta.json",
  "doc/mini_DataTypes.html",
  "doc/mini_RuleHelpers.html",
  "doc/mini_Rules.html",
  "doc/mini_Sample.html",
  "doc/mini_TLib.html",
  "doc/mini_TSample.html",
  "doc/mini_Views.html",
  "doc/minus.gif",
  "doc/ocean.css",
  "doc/plus.gif",
  "doc/quick-jump.css",
  "doc/quick-jump.min.js",
  "doc/RuleHelpers.html",
  "doc/Rules.html",
  "doc/Sample.html",
  "doc/src/BaseGame.html",
  "doc/src/DataTypes.html",
  "doc/src/highlight.js",
  "doc/src/RuleHelpers.html",
  "doc/src/Rules.html",
  "doc/src/Sample.html",
  "doc/src/style.css",
  "doc/src/TLib.html",
  "doc/src/TSample.html",
  "doc/src/Views.html",
  "doc/synopsis.png",
  "doc/TLib.html",
  "doc/TSample.html",
  "doc/Views.html",
  "static/ace.js",
  "static/dejavupc.woff",
  "static/home.html",
  "static/jquery.min.js",
  "static/mode-haskell.js",
  "static/newRule.html",
  "static/play.html",
  "static/star.js",
  "static/styles.css",
  "static/Testing.html",
  "static/theme-monokai.js",
  "static/wait.html"])
