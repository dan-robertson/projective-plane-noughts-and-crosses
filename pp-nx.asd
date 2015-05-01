(in-package :asdf-user)

(defsystem "projective-plane-naughts-and-crosses"
    :description "A program to search for a winning, losing or tying strategy in 4x4 projective-plane naughts-and-crosses"
    :author "Dan Robertson"
    :components
    ((:file "board.lisp")))
