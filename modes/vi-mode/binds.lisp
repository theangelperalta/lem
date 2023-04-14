(defpackage :lem-vi-mode/binds
  (:use :cl
        :lem
        :lem/universal-argument
        :lem/abbrev
        :lem-vi-mode/core
        :lem-vi-mode/commands
        :lem-vi-mode/ex
        :lem-vi-mode/state/visual
        :lem-vi-mode/state/insert))
(in-package :lem-vi-mode/binds)

;; Commands

(define-key *command-keymap* "s" 'vi-substitute)
;; Command/Action keys = ['x'];
(define-key *command-keymap* "x" 'vi-delete-next-char)
;; Command/Action keys = ['X'];
(define-key *command-keymap* "X" 'vi-delete-previous-char)
;; Actions keys = ['g', 'J'];
(define-key *command-keymap* "g J" 'vi-join)
;; Actions keys = ['J'];
(define-key *command-keymap* "J" 'vi-join-line)

;; Put/Command keys = ['p'];
(define-key *command-keymap* "p" 'vi-paste-after)
;; Put/Command keys = ['P']; PutBeforeCommand
(define-key *command-keymap* "P" 'vi-paste-before)
;; Command keys = ['r']; ActionReplaceCharacter
(define-key *command-keymap* "r" 'vi-replace-char)
;; Command keys = ['u']; CommandUndo
(define-key *command-keymap* "u" 'vi-undo)
(define-key *command-keymap* "C-r" 'vi-redo)
(define-key *command-keymap* "C-f" 'next-page)
(define-key *command-keymap* "C-b" 'previous-page)

;; Commands keys = ['v'];
(define-key *command-keymap* "v" 'vi-visual-char)
;; Commands keys = ['V'];
(define-key *command-keymap* "V" 'vi-visual-line)
(define-key *command-keymap* "C-v" 'vi-visual-block)
;; Commands keys = [['i'], ['<Insert>']];
(define-key *command-keymap* "i" 'vi-insert)
;; Commands keys = ['I'];
(define-key *command-keymap* "I" 'vi-insert-line)
;; Commands keys = ['a'];
(define-key *command-keymap* "a" 'vi-append)
;; Commands keys = ['A'];
(define-key *command-keymap* "A" 'vi-append-line)
;; Commands keys = ['o'];
(define-key *command-keymap* "o" 'vi-open-below)
;; Commands keys = ['O'];
(define-key *command-keymap* "O" 'vi-open-adove)
(define-key *command-keymap* "C-o" 'vi-jump-back)
(define-key *command-keymap* "C-i" 'vi-jump-next)
;; Commands keys = [':'];
(define-key *command-keymap* ":" 'vi-ex)
(define-key *command-keymap* 'delete-previous-char 'vi-backward-char)
(define-key *command-keymap* 'self-insert 'undefined-key)

(define-key *insert-keymap* "Escape" 'vi-end-insert)
(define-key *insert-keymap* "C-p" 'abbrev)
;; Commands keys = ['<C-w>'];
(define-key *insert-keymap* "C-w" 'vi-kill-last-word)
;; Command keys = ['/'];
(define-key *command-keymap* "/" 'vi-search-forward)
;; Command keys = ['?'];
(define-key *command-keymap* "?" 'vi-search-backward)

(define-key *command-keymap* "C-p" 'yank-pop)
(define-key *command-keymap* "C-n" 'yank-pop-next)

(define-key *command-keymap* "C-g" 'vi-keyboard-quit)
(define-key *inactive-keymap* "Escape" 'vi-keyboard-quit)

;; Operator

;; Operators keys = ['d'];
(define-key *command-keymap* "d" 'vi-delete)
;; Operators keys = ['D'];
(define-key *command-keymap* "D" 'vi-delete-line)
;; Operators keys = ['c']; change
(define-key *command-keymap* "c" 'vi-clear)
;; Operators keys = ['C']; ChangeToLineEnd
(define-key *command-keymap* "C" 'vi-clear-line)
;; Operator keys = ['y'];
(define-key *command-keymap* "y" 'vi-yank)
;; Operator keys = ['='];
(define-key *command-keymap* "=" 'vi-indent)

;; Motion
;; Motion keys = [['0'], ['<Home>'], ['<D-left>']];
(define-key *command-keymap* "0" 'vi-move-to-beginning-of-line/universal-argument-0)
(define-key *command-keymap* "1" 'universal-argument-1)
(define-key *command-keymap* "2" 'universal-argument-2)
(define-key *command-keymap* "3" 'universal-argument-3)
(define-key *command-keymap* "4" 'universal-argument-4)
(define-key *command-keymap* "5" 'universal-argument-5)
(define-key *command-keymap* "6" 'universal-argument-6)
(define-key *command-keymap* "7" 'universal-argument-7)
(define-key *command-keymap* "8" 'universal-argument-8)
(define-key *command-keymap* "9" 'universal-argument-9)
;; Motion keys = [['l'], ['<right>'], [' ']];
(define-key *command-keymap* "l" 'vi-forward-char)
;; Motion keys = [['h'], ['<left>'], ['<BS>'], ['<C-BS>'], ['<S-BS>']];
(define-key *command-keymap* "h" 'vi-backward-char)
;; Motion keys = [['j'], ['<down>'], ['<C-j>'], ['<C-n>']];
(define-key *command-keymap* "j" 'vi-next-line)
;; Motion keys = [['k'], ['<up>'], ['<C-p>']];
(define-key *command-keymap* "k" 'vi-previous-line)
;; Motions keys = [['g', 'j'], ['g', '<down>']]
(define-key *command-keymap* "g j" 'vi-next-display-line)
;; Motions keys = [['g', 'k'], ['g', '<up>']]
(define-key *command-keymap* "g k" 'vi-previous-display-line)
;; Motions keys = ['w'];
(define-key *command-keymap* "w" 'vi-forward-word-begin)
;; Motions keys = [['b'], ['<C-left>']];
(define-key *command-keymap* "b" 'vi-backward-word-begin)
;; Motions keys = [['W'], ['<C-right>']];
(define-key *command-keymap* "W" 'vi-forward-word-begin-broad)
;; Motions keys = ['B'];
(define-key *command-keymap* "B" 'vi-backward-word-begin-broad)
;; Motion keys = ['e'];
(define-key *command-keymap* "e" 'vi-forward-word-end)
;; Motion keys = ['E'];
(define-key *command-keymap* "E" 'vi-forward-word-end-broad)
;; Motion keys = [['$'], ['<End>'], ['<D-right>']];
(define-key *command-keymap* "$" 'vi-move-to-end-of-line)
;; Motion keys = ['_'];
(define-key *command-keymap* "g _" 'vi-move-to-last-nonblank)
;; Motion keys = ['H'];
(define-key *command-keymap* "H" 'vi-move-to-window-top)
;; Motion keys = ['M'];
(define-key *command-keymap* "M" 'vi-move-to-window-middle)
;; Motion keys = ['L'];
(define-key *command-keymap* "L" 'vi-move-to-window-bottom)
;; Motion keys = ['^'];
(define-key *command-keymap* "^" 'vi-back-to-indentation)
;; Motions keys = ['{'];
(define-key *command-keymap* "{" 'backward-paragraph)
;; Motions keys = ['}'];
(define-key *command-keymap* "}" 'forward-paragraph)

;; Motions keys = ['%'];
(define-key *command-keymap* "%" 'vi-move-to-matching-paren)
;; Motions keys = ['n'];
(define-key *command-keymap* "n" 'vi-search-next)
;; Motions keys = ['N'];
(define-key *command-keymap* "N" 'vi-search-previous)
;; Motion/Command? keys = ['*']
(define-key *command-keymap* "*" 'vi-search-forward-symbol-at-point)
;; Motions keys = [['g', 'g'], ['<C-Home>']];
(define-key *command-keymap* "g g" 'vi-goto-first-line)
;; Motions keys = ['G'];
(define-key *command-keymap* "G" 'vi-goto-line)
(define-key *command-keymap* "Return" 'vi-return)
;; Motions keys = ['f', '<character>'];
(define-key *command-keymap* "f" 'vi-find-char)
;; Motions keys = ['F', '<character>'];
(define-key *command-keymap* "F" 'vi-find-char-backward)
;; Motions keys = ['t', '<character>'];
(define-key *command-keymap* "t" 'vi-find-char-before)
;; Motions keys = ['T', '<character>'];
(define-key *command-keymap* "T" 'vi-find-char-backward-after)
(define-key *command-keymap* "z z" 'recenter)
(define-key *command-keymap* "Z Z" 'vi-write-quit)
(define-key *command-keymap* "C-w s" 'split-active-window-vertically)
(define-key *command-keymap* "C-w C-s" 'split-active-window-vertically)
(define-key *command-keymap* "C-w w" 'other-window)
(define-key *command-keymap* "C-w C-w" 'other-window)
(define-key *command-keymap* "C-w q" 'vi-quit)
(define-key *command-keymap* "C-w h" 'window-move-left)
(define-key *command-keymap* "C-w C-h" 'undefined-key)
(define-key *command-keymap* "C-w l" 'window-move-right)
(define-key *command-keymap* "C-w C-l" 'undefined-key)
(define-key *command-keymap* "C-w k" 'window-move-up)
(define-key *command-keymap* "C-w C-k" 'undefined-key)
(define-key *command-keymap* "C-w j" 'window-move-down)
(define-key *command-keymap* "C-w C-j" 'undefined-key)

;; TextObjects