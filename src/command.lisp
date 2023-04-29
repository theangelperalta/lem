(in-package :lem)

(define-condition executing-command (signal-handler)
  ((command :initarg :command
            :initform (alexandria:required-argument :command)
            :reader executing-command-command)))

(define-condition before-executing-command (executing-command) ()
  (:report (lambda (c s)
             (format s "before executing the ~S command" (executing-command-command c)))))

(define-condition after-executing-command (executing-command) ()
  (:report (lambda (c s)
             (format s "after executing the ~S command" (executing-command-command c)))))

(defvar *this-command*)

(defun this-command ()
  *this-command*)

(defgeneric execute (mode command argument))

(defclass primary-command ()
  ((name :initarg :name
         :reader command-name)
   (source-location :initarg :source-location
                    :reader command-source-location)))

(defun register-command-class (symbol class-name)
  (setf (get symbol 'normal-class) class-name))

(defun get-command-class (symbol)
  (get symbol 'normal-class))

(defun get-command (symbol)
  (alexandria:when-let (class (get-command-class symbol))
    (make-instance class)))

(defun ensure-command (command)
  (check-type command (or primary-command symbol))
  (if (typep command 'primary-command)
      command
      (get-command command)))

(defun call-command (this-command universal-argument)
  (let ((*this-command* (ensure-command this-command)))
    (unless *this-command*
      (editor-error "~A: command not found" this-command))
    (signal-subconditions 'before-executing-command :command *this-command*)
    (prog1 (execute (get-active-modes-class-instance (current-buffer))
                    *this-command*
                    universal-argument)
      (buffer-undo-boundary)
      (signal-subconditions 'after-executing-command :command *this-command*))))
