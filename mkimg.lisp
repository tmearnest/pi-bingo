(load "pi-bingo.lisp")
(pi-bingo:load-game)
(save-lisp-and-die "bingo.img" :toplevel #'pi-bingo:main-loop :executable t)
