Threes.scala
============

This is an implementation of the fantastically addictive iOS game
[Threes](http://asherv.com/threes/) in Scala, including a snazzy ANSI console
UI and a basic framework for building and evaluating AI solvers.

It aims to correctly emulate the game, based on
[this](http://forums.toucharcade.com/showpost.php?p=3140044&postcount=522)
thread on TouchArcade.

### Usage

Make sure `sbt` is available on your path, `cd` into the directory you cloned
this repo in, and run one of the following:

```
$ sbt "run human"      # play human game
$ sbt "run ai"         # evaluate an AI's performance
$ sbt "run animate-ai" # watch a sample run of an AI
```
