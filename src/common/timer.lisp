(defpackage :lem/common/timer
  (:use :cl :alexandria)
  (:export :timer-manager
           :send-timer-notification
           :timer-error
           :timer
           :timer-name
           :timer-expired-p
           :make-timer
           :make-idle-timer
           :start-timer
           :stop-timer
           :with-idle-timers
           :update-idle-timers
           :get-next-timer-timing-ms
           :with-timer-manager))
(in-package :lem/common/timer)

(defvar *timer-manager*)

(defgeneric send-timer-notification (timer-manager continue))

(defclass timer-manager ()
  ())

(define-condition timer-error (error)
  ((timer :initarg :timer)
   (condition :initarg :condition))
  (:report (lambda (c s)
             (with-slots (timer condition) c
               (format s "Error running timer ~S: ~A" (timer-name timer) condition)))))

(defmethod get-microsecond-time ((timer-manager timer-manager))
  (values
   (floor (/ (get-internal-real-time)
             (load-time-value (/ internal-time-units-per-second 1000))))))

(defclass timer-internal ()
  ((ms
    :initarg :ms
    :accessor timer-internal-ms
    :type (integer 1 *))
   (repeat-p
    :initarg :repeat-p
    :reader timer-internal-repeat-p
    :type boolean)
   (expired-p
    :initform nil
    :reader timer-internal-expired-p
    :writer set-timer-internal-expired-p
    :type boolean)
   (last-time
    :initform nil
    :initarg :last-time
    :accessor timer-internal-last-time
    :type (or null (integer 1 *)))

   (mutex
    :initarg :mutex
    :reader timer-internal-mutex
    :type sb-thread:mutex)
   (stop-mailbox
    :initarg :stop-mailbox
    :accessor timer-internal-stop-mailbox
    :type sb-concurrency:mailbox)
   (thread
    :initarg :thread
    :accessor timer-internal-thread
    :type sb-thread:thread)))

(defgeneric timer-expired-p (timer))
(defgeneric expire-timer (timer))
(defgeneric inspire-timer (timer))

(defclass <timer> ()
  ((name
    :initarg :name
    :reader timer-name
    :type (or null string))
   (function
    :initarg :function
    :reader timer-function
    :type (or symbol function))
   (handle-function
    :initarg :handle-function
    :reader timer-handle-function
    :type (or null function))
   (internal
    :accessor timer-internal
    :type timer-internal)))

(defmethod print-object ((object <timer>) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (prin1 (timer-name object) stream)))

(defun guess-function-name (function)
  (etypecase function
    (function (sb-impl::%fun-name function))
    (symbol (symbol-name function))))

(defun make-timer-instance (timer-class function name handle-function)
  (make-instance timer-class
                 :name (or name
                           (guess-function-name function))
                 :function (ensure-function function)
                 :handle-function (when handle-function
                                    (ensure-function handle-function))))

(defun call-timer-function (timer)
  (handler-case
      (if (timer-handle-function timer)
          (handler-bind ((error (timer-handle-function timer)))
            (funcall (timer-function timer)))
          (funcall (timer-function timer)))
    (error (condition)
      (error 'timer-error :timer timer :condition condition))))

(defun call-with-timer-manager (timer-manager function)
  (let ((*timer-manager* timer-manager)
        (bt:*default-special-bindings* (acons '*timer-manager*
                                              timer-manager
                                              bt:*default-special-bindings*)))
    (funcall function)))

(defmacro with-timer-manager (timer-manager &body body)
  `(call-with-timer-manager ,timer-manager (lambda () ,@body)))


;;; timer
(defclass timer (<timer>)
  ())

(defmethod timer-expired-p ((timer timer))
  (sb-thread:with-mutex ((timer-internal-mutex (timer-internal timer)))
    (timer-internal-expired-p (timer-internal timer))))

(defmethod expire-timer ((timer timer))
  (sb-thread:with-mutex ((timer-internal-mutex (timer-internal timer)))
    (set-timer-internal-expired-p t (timer-internal timer))))

(defmethod inspire-timer ((timer timer))
  (sb-thread:with-mutex ((timer-internal-mutex (timer-internal timer)))
    (set-timer-internal-expired-p nil (timer-internal timer))))

(defun make-timer (function &key name handle-function)
  (make-timer-instance 'timer function name handle-function))

(defmethod start-timer ((timer timer) ms &optional repeat-p)
  (setf (timer-internal timer)
        (make-instance 'timer-internal
                       :ms ms
                       :repeat-p repeat-p
                       :last-time (get-microsecond-time *timer-manager*)
                       :mutex (sb-thread:make-mutex :name "timer internal mutex")))
  (start-timer-thread timer ms repeat-p)
  timer)

(defmethod stop-timer ((timer timer))
  (stop-timer-thread timer))

(defun start-timer-thread (timer ms repeat-p)
  (let ((stop-mailbox (sb-concurrency:make-mailbox))
        (timer-manager *timer-manager*)
        (seconds (float (/ ms 1000))))
    (setf (timer-internal-stop-mailbox (timer-internal timer))
          stop-mailbox)
    (setf (timer-internal-thread (timer-internal timer))
          (bt:make-thread
           (lambda ()
             (loop
               (let ((recv-stop-msg
                       (nth-value 1
                                  (sb-concurrency:receive-message stop-mailbox
                                                                  :timeout seconds))))
                 (when recv-stop-msg
                   (expire-timer timer)
                   (return)))
               (send-timer-notification timer-manager
                                        (lambda () (call-timer-function timer)))
               (unless repeat-p
                 (return))))
           :name (format nil "Timer ~A" (timer-name timer))))))

(defun stop-timer-thread (timer)
  (sb-concurrency:send-message (timer-internal-stop-mailbox (timer-internal timer)) t))


;;; idle-timer

(defvar *idle-timer-list* nil)
(defvar *processed-idle-timer-list* nil)

(defclass idle-timer (<timer>)
  ())

(defmethod timer-repeat-p ((timer idle-timer))
  (timer-internal-repeat-p (timer-internal timer)))

(defmethod timer-last-time ((timer idle-timer))
  (timer-internal-last-time (timer-internal timer)))

(defmethod set-timer-last-time (value (timer idle-timer))
  (setf (timer-internal-last-time (timer-internal timer)) value))

(defmethod timer-has-last-time ((timer idle-timer))
  (not (null (timer-last-time timer))))

(defmethod timer-next-time ((timer idle-timer))
  (+ (timer-last-time timer) (timer-internal-ms (timer-internal timer))))

(defmethod timer-expired-p ((timer idle-timer))
  (timer-internal-expired-p (timer-internal timer)))

(defmethod expire-timer ((timer idle-timer))
  (set-timer-internal-expired-p t (timer-internal timer)))

(defmethod inspire-timer ((timer idle-timer))
  (set-timer-internal-expired-p nil (timer-internal timer)))

(defun make-idle-timer (function &key name handle-function)
  (make-timer-instance 'idle-timer function name handle-function))

(defmethod start-timer ((timer idle-timer) ms &optional repeat-p)
  (setf (timer-internal timer)
        (make-instance 'timer-internal
                       :ms ms
                       :repeat-p repeat-p))
  (push timer *idle-timer-list*)
  timer)

(defmethod stop-timer ((timer idle-timer))
  (stop-idle-timer timer))

(defun stop-idle-timer (timer)
  (expire-timer timer)
  (setf *idle-timer-list* (delete timer *idle-timer-list*)))

(defun start-idle-timers ()
  (flet ((update-last-time-in-idle-timers ()
           (loop :with last-time := (get-microsecond-time *timer-manager*)
                 :for timer :in *idle-timer-list*
                 :do (set-timer-last-time last-time timer)))
         (inspire-idle-timers ()
           (mapc #'inspire-timer *idle-timer-list*)))
    (update-last-time-in-idle-timers)
    (inspire-idle-timers)))

(defun stop-idle-timers ()
  (setf *idle-timer-list* (nconc *processed-idle-timer-list* *idle-timer-list*))
  (setf *processed-idle-timer-list* '()))

(defun call-with-idle-timers (function)
  (start-idle-timers)
  (prog1 (funcall function)
    (stop-idle-timers)))

(defmacro with-idle-timers (() &body body)
  `(call-with-idle-timers (lambda () ,@body)))

(defun update-idle-timers ()
  (let* ((tick-time (get-microsecond-time *timer-manager*))
         (updating-timers (remove-if-not (lambda (timer)
                                           (< (timer-next-time timer) tick-time))
                                         (remove-if-not #'timer-has-last-time
                                                        *idle-timer-list*)))
         (deleting-timers (remove-if #'timer-repeat-p
                                     updating-timers))
         (updating-idle-timers (remove-if-not #'timer-repeat-p
                                              updating-timers)))
    (mapc #'expire-timer deleting-timers)

    (setf *idle-timer-list* (set-difference *idle-timer-list* deleting-timers))
    (setf *idle-timer-list* (set-difference *idle-timer-list* updating-idle-timers))
    (setf *processed-idle-timer-list* (nconc updating-idle-timers *processed-idle-timer-list*))

    (dolist (timer updating-timers)
      (unless (timer-repeat-p timer)
        (set-timer-last-time tick-time timer)))
    (mapc #'call-timer-function updating-timers)
    (not (null updating-timers))))

(defun get-next-timer-timing-ms ()
  (let ((timers *idle-timer-list*))
    ;; Remove timers without a last-time
    (setf timers (remove-if-not #'timer-has-last-time timers))
    (if (null timers)
        nil
        (- (loop :for timer :in timers
                 :minimize (timer-next-time timer))
           (get-microsecond-time *timer-manager*)))))