Bartok
======

Bartok is a card game in which players may modify rules when they win.

You play a game of Bartok (initially similar to Uno) and when your hand is empty, you may submit code that modifies the rules.

This can be played at http://3dox.uk and you can test rules by starting games where the name of the game begins with the string "TST".

For an introduction to writing rules, see [Tutorial](Lib/Game/Bartok/Tutorial.hs).
[TLib](Lib/Game/Bartok/TLib.hs) is a framework for writing rules. [TSample](Lib/Game/Bartok/TSample.hs) gives examples of rules written with this framework.

You can also find documentation [here](http://3dox.uk/doc/index.html).

Building and running
---------------------

Assuming you have [Stack](https://docs.haskellstack.org/en/stable/README/), the following commands should get it running:
```
git clone https://github.com/penteract/Bartok.git
cd Bartok
./serve
```

(`serve` just does `stack build && stack exec server`)
