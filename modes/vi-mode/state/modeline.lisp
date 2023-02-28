(defpackage :lem-vi-mode.state.modeline
  (:use :cl
        :lem
        :lem-vi-mode.core)
  (:export :initialize-vi-modeline
	   :finalize-vi-modeline
  	   :modeline))
(in-package :lem-vi-mode.state.modeline)

(define-vi-state modeline (:keymap *inactive-keymap*))

(defvar *modeline-element*)

(define-attribute state-attribute
  (t :reverse-p t))

(defstruct (vi-modeline-element (:conc-name element-))
  name)

(defmethod convert-modeline-element ((element vi-modeline-element) window)
  (values (element-name element) 'state-attribute))

(defun initialize-vi-modeline ()
  (setf *modeline-element* (make-vi-modeline-element))
  (modeline-add-status-list *modeline-element*))

(defun finalize-vi-modeline ()
  (modeline-remove-status-list *modeline-element*))

(defun change-element-name (name)
  (setf (element-name *modeline-element*) name))