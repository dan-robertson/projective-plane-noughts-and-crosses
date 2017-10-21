# Projective plane noughts and crosses

Imagine a 4x4 version of the game of noughts and crosses (tic-tac-toe)
but instead of being played on a normal plane, the game is played on
the real projective plane instead. It turns out this isn't
particularly well defined so one must make some arbitrary choices.
However it turns out that there is a winning strategy for the player
who goes first (crosses), and that there is no strategy by which the
second player (noughts) may force a draw.

# Running

	CL-USER> (load "board")
	Tried 120 = 5! combinations
	Tried 720 = 6! combinations
	Tried 5040 = 7! combinations
	Tried 40320 = 8! combinations
	Tried 362880 = 9! combinations
	Tried 3628800 = 10! combinations
	Tried 120 = 5! combinations
	Tried 720 = 6! combinations
	Tried 5040 = 7! combinations
	Tried 40320 = 8! combinations
	Tried 362880 = 9! combinations
	Tried 3628800 = 10! combinations
	T
	CL-USER>

This will determine first if there is a winning strategy for player 1
(there is) and then it will determine what that strategy is and write
it to the file `strat.txt`. A sample of this file is:

	Crosses should play (0 0).
     If naughts plays like:
     xo..
     ....
     ....
     ....
     Crosses should play (0 1).
      If naughts plays like:
      xoo.
      x...
      ....
      ....
      Crosses should play (3 0).
       If naughts plays like:
       xoox
       xo..
       ....
       ....
       Crosses should play (0 2).
        If naughts plays like:
        xoox
        xoo.
        x...
        ....
        Crosses should play (3 1).
         If naughts plays like:
         xoox
         xoox
         xo..
         ....
         Crosses should play (0 3).
         If naughts plays like:
         xoox
         xoox
         x.o.
         ....
         Crosses should play (0 3).
         If naughts plays like:
         xoox
         xoox
         x..o
         ....

There is also a file `draw.lisp` which attempts to draw the winning
strategy onto one big image. The image shows a board with a big red X
where player one should play and in every blank square where player
two may play is a smaller board following the same rules. It turns out
that this image is very large and likely to upset whatever program you
try to view it in. There is also some code to draw this image into
tiles at a fixed size. The program makes no effort to do any culling
so drawing each tile takes a long time. Teh function `tiles` will draw
tiles at different zoom levels while `tiles2` attempts to draw tiles
at only one zoom level with the hope that they will later be processed
into more suitably sized tiles at every zoom factor.
