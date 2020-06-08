module Game.Bartok.Whitelist (allowed) where

allowed :: String -> Bool
allowed =
  ( `elem`
      [ "doc/bartok.txt",
        "doc/doc-index-126.html",
        "doc/doc-index-37.html",
        "doc/doc-index-46.html",
        "doc/doc-index-47.html",
        "doc/doc-index-58.html",
        "doc/doc-index-60.html",
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
        "doc/doc-index-I.html",
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
        "doc/Game-Bartok-BaseGame.html",
        "doc/Game-Bartok-DataTypes.html",
        "doc/Game-Bartok-RuleHelpers.html",
        "doc/Game-Bartok-Rules.html",
        "doc/Game-Bartok-Sample.html",
        "doc/Game-Bartok-TLib.html",
        "doc/Game-Bartok-TSample.html",
        "doc/Game-Bartok-Tutorial.html",
        "doc/Game-Bartok-Views.html",
        "doc/haddock-bundle.min.js",
        "doc/hslogo-16.png",
        "doc/index.html",
        "doc/meta.json",
        "doc/minus.gif",
        "doc/ocean.css",
        "doc/plus.gif",
        "doc/quick-jump.css",
        "doc/quick-jump.min.js",
        "doc/src/Game.Bartok.BaseGame.html",
        "doc/src/Game.Bartok.DataTypes.html",
        "doc/src/Game.Bartok.RuleHelpers.html",
        "doc/src/Game.Bartok.Rules.html",
        "doc/src/Game.Bartok.Sample.html",
        "doc/src/Game.Bartok.TLib.html",
        "doc/src/Game.Bartok.TSample.html",
        "doc/src/Game.Bartok.Tutorial.html",
        "doc/src/Game.Bartok.Views.html",
        "doc/src/highlight.js",
        "doc/src/style.css",
        "doc/synopsis.png",
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
        "static/wait.html"
      ]
  )
