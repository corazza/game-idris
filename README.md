# game-idris-2

Game made in Idris--new iteration/rewrite

The previous codebase had an ad-hoc architecture that would've been hard to adapt to a networked mode, therefore the major systems had to be rewritten.

There were two errors total in the rewrite: I forgot to flip SDL, and forgot to read the mass value from Box2D.
