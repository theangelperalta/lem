(defpackage :lem-vi-mode/state/insert
  (:use :cl
        :lem
        :lem-vi-mode/core)
  (:export :*insert-keymap*
  	   :insert))
(in-package :lem-vi-mode/state/insert)

(defvar *insert-keymap* (make-keymap :name '*insert-keymap* :parent *global-keymap*))

(define-vi-state insert (:keymap *insert-keymap* :cursor-color "IndianRed"))

(defmethod e-hook ((state insert) &rest args)
  (message " -- INSERT --"))