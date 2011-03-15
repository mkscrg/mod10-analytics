## mod10-analytics

Last updated by Mike S. Craig, March 2011

This is a small Haskell project to explore various aspects of the Mod10 card
game. It is "fully functional" at this point, but there is a lot of potential
for expansion, as discussed below. The whole project is unlicensed, i.e. it's
in the public domain.

### The Game of Mod10

Mod10 is a simple solitaire card game, in which the player lays down cards in
piles and picks up cards from those piles under certain conditions. There's a
full explanation of the game's rules, with helpful pictures, in a [blog
post](http://mkscrg.github.com/2011/02/02/mod10-a-game-youve-never-played/) of
mine. Give it a try with a real deck, or if you're running Mac OS X, you can
find a playable version of the game [here](https://github.com/mkscrg/Mod10).
Both a universal binary and the source code are available, compatible with Snow
Leopard or later.

### Simulating and Analyzing

Bluntly put, Mod10 is not a game that one wins very often. It makes sense,
then, to try to learn something about the game that would afford a greater
chance of winning. Therein lies the goal of this project: automatically
simulate the game and process the results in search of such insights. Those two
tasks are solved by the `simulate` and `analyze` executables. The commenting in
the source files is thorough, so I'll stick to the high-level details here.

`simulate`, by default, just plays a single game of Mod10 and outputs the
result to `stdout`: `Win N`, `Loss N`, or `Timeout`, where `N` is the number of
turns in the game and `Timeout` occurs if the game runs over 2000 turns. The
time-out contingency is necessary because the game is not guaranteed to
terminate.  Occasionally, Mod10 enters a loop configuration, where the same
cards are dealt to the same stacks, often in the same order, over and over
again. Command-line options to `simulate` provide for multiple games, a
specified output path, simple game filtering (explained below), and verbose
output where each turn in the game is printed in a human-readable format. Run
`./simulate --help` for a full specification of its options. The output from
`./simulate --ngames=10` looks like this:

    Loss 136
    Loss 163
    Loss 184
    Loss 100
    Win 244
    Loss 139
    Loss 157
    Loss 154
    Loss 217
    Loss 115

`analyze`, by default, takes input from `stdin` of the form that is output by
`simulate`, and prints a number of statistics about the games played.
Command-line options to `analyze` provide for specified input and output paths,
as well as plotting the number of wins/losses versus the number of rounds in
each game, using the `gnuplot` bindings in the
[Graphics.Gnuplot](http://hackage.haskell.org/package/gnuplot-0.4.1.1) library.
Run `./analyze --help` for a full specification of its options. The text output
from `./simulate -n 100000 | ./analyze --plots` looks like this:

    # games played: 100000
    winning ...
      (# games, frac. of non-timeouts): (2358,2.3676600530163065e-2)
          (min # rounds, max # rounds): (28,1441)
       (mean # rounds, stDev # rounds): (328.48473282442745,175.18319031764773)
    losing ...
      (# games, frac. of non-timeouts): (97234,0.9763233994698369)
          (min # rounds, max # rounds): (37,1186)
       (mean # rounds, stDev # rounds): (137.79134870518544,73.61708991630982)
    timeout ...
             (# games, frac. of total): (408,4.08e-3)

The .png plot produced by that run is
[here](https://github.com/mkscrg/mod10-analytics/raw/improvements/plot.png).

### In Search of Better Odds

We've got some basic answers, now:

+ About 0.4% of games enter a loop configuration. (We conclude, from the plot,
  that only a very small number non-looping games run as long as 2000 turns.)
+ Of the non-looping games, about 2.4% are winners, while the remaining 97.6%
  are losers.
+ The average losing game is shorter than the average winning game, and the
  winners are more-widely distributed. So games that run longer than about 100
  turns are more likely to be winners.

This is interesting information--especially the last point--but we'd really
like to know if we can improve our odds without breaking the rules of the game.
Fortunately, these simulation and analysis tools are a good platform for
further exploration. It's easy to build in additional functionality by writing
alternate functions for key pieces of the process, and Haskell's type system
helps by ensuring everything fits together nicely.

One "intuitive" way to improve our chances is to simply throw out games that
seem to begin badly. (In my experience, this is often done by human players.)
This is what `simulate`'s `--filter` flag does: if at least one stack isn't
eliminated by the end of the seventh round (i.e. after the first row of
playable cards has been dealt), call the game a `Timeout` and start a new game.
Let's look at the output of `./simulate --filter -n 100000 | ./analyze`

    # games played: 100000
    winning ...
      (# games, frac. of non-timeouts): (2051,3.749680061428206e-2)
          (min # rounds, max # rounds): (19,1153)
       (mean # rounds, stDev # rounds): (308.95855680156023,172.91285479915663)
    losing ...
      (# games, frac. of non-timeouts): (52647,0.9625031993857179)
          (min # rounds, max # rounds): (40,1288)
       (mean # rounds, stDev # rounds): (151.56607214086273,81.58507051256947)
    timeout ...
             (# games, frac. of total): (45302,0.45302)

By throwing out almost half of the games we start, we multiply our chances of
winning by a factor of almost 1.6, to about 3.7% overall. But what if we throw
out even more of them? Perhaps we only want to play games where we've
eliminated at least 2 stacks by the end of turn seven. (See the `threshold`
assignment at the bottom of `simulate.hs`.) The output is now even more
interesting:

    # games played: 100000
    winning ...
      (# games, frac. of non-timeouts): (1358,8.056956392761792e-2)
          (min # rounds, max # rounds): (37,1042)
       (mean # rounds, stDev # rounds): (302.0117820324006,176.56064780089)
    losing ...
      (# games, frac. of non-timeouts): (15497,0.9194304360723821)
          (min # rounds, max # rounds): (49,1222)
       (mean # rounds, stDev # rounds): (172.72962508872686,93.7022599593909)
    timeout ...
             (# games, frac. of total): (83145,0.83145)

By throwing out almost 85% of the games we start, we multiply our chances of
winning by a factor of 2.4, to about 8.1% overall. This is a great insight for
anyone patient enough to actually play this game!

### Further Development

The basic platform for simulation and analysis is finished, barring any
insights regarding more efficient ways to play the game or process the data.
However, different strategies for choosing which games to play and which to
throw out may provide better odds. Also, the gameplay itself is not entirely
devoid of player choice, so the strategy could probably be improved there, as
well. Further work on this project will consist of exploring these options.
