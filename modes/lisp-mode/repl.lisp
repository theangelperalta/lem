(in-package :lem-lisp-mode)

(define-major-mode lisp-repl-mode lisp-mode
    (:name "lisp-repl"
     :keymap *lisp-repl-mode-keymap*
     :syntax-table lem-lisp-syntax:*syntax-table*)
  (cond
    ((eq (repl-buffer) (current-buffer))
     (repl-reset-input)
     (lem/listener-mode:start-listener-mode (merge-pathnames "history/lisp-repl" (lem-home)))
     (setf *write-string-function* 'write-string-to-repl)
     (setf (variable-value 'completion-spec) 'repl-completion))
    (t
     (editor-error "No connection for repl. Did you mean 'start-lisp-repl' command?"))))

(defun read-string-thread-stack ()
  (buffer-value (repl-buffer) 'read-string-thread-stack))

(defun (setf read-string-thread-stack) (val)
  (setf (buffer-value (repl-buffer) 'read-string-thread-stack) val))

(defun read-string-tag-stack ()
  (buffer-value (repl-buffer) 'read-string-tag-stack))

(defun (setf read-string-tag-stack) (val)
  (setf (buffer-value (repl-buffer) 'read-string-tag-stack) val))

(define-key *lisp-repl-mode-keymap* "C-c C-c" 'lisp-repl-interrupt)
(define-key *lisp-repl-mode-keymap* "," 'lisp-repl-shortcut)

(define-command lisp-repl-interrupt () ()
  (send-message-string *connection*
                       (format nil "(:emacs-interrupt ~(~S~))"
                               (or (car (read-string-thread-stack))
                                   :repl-thread))))

(defvar *lisp-repl-shortcuts* '())

(defun prompt-for-shortcuts ()
  (let* ((*lisp-repl-shortcuts* *lisp-repl-shortcuts*)
         (names (mapcar #'car *lisp-repl-shortcuts*)))
    (cdr (assoc (prompt-for-string
                 "Command:"
                 :completion-function (lambda (x) (completion-strings x names))
                 :test-function (lambda (name) (member name names :test #'string=))
                 :history-symbol 'mh-lisp-repl-shortcuts)
                *lisp-repl-shortcuts* :test #'equal))))

(define-command lisp-repl-shortcut (n) ("p")
  (with-point ((point (current-point)))
    (if (point>= (lem/listener-mode:input-start-point (current-buffer)) point)
        (let ((fun (prompt-for-shortcuts)))
          (when fun
            (funcall fun n)))
        (let ((c (insertion-key-p (last-read-key-sequence))))
          (insert-character point c n)))))

(defmacro define-repl-shortcut (name lambda-list &body body)
  (if (symbolp lambda-list)
      `(progn
         (setf *lisp-repl-shortcuts*
               (remove ,(string-downcase name) *lisp-repl-shortcuts* :key 'first :test 'equal))
         (push (cons ,(string-downcase name) ',lambda-list) *lisp-repl-shortcuts*)
         ',name)
      `(progn
         (setf *lisp-repl-shortcuts*
               (remove ,(string-downcase name) *lisp-repl-shortcuts* :key 'first :test 'equal))
         (push (cons ,(string-downcase name) ',name) *lisp-repl-shortcuts*)
         (defun ,name ,lambda-list ,@body))))

(defun repl-buffer ()
  (get-buffer "*lisp-repl*"))

(defun ensure-repl-buffer-exist ()
  (let ((buffer (repl-buffer)))
    (unless buffer
      (with-current-window (current-window)
        (start-lisp-repl))
      (setf buffer (repl-buffer)))
    buffer))

(defun repl-set-prompt (point)
  (insert-string point
                 (format nil "~A> " (connection-prompt-string *connection*)))
  point)

(defun repl-paren-correspond-p (point)
  (unless (eq (repl-buffer) (point-buffer point))
    (return-from repl-paren-correspond-p))
  (with-point ((start (lem/listener-mode:input-start-point (repl-buffer))))
    (let ((state (parse-partial-sexp start point)))
      (and (not (pps-state-string-or-comment-p state))
           (>= 0 (pps-state-paren-depth state))))))

(defun repl-reset-input ()
  (let ((buffer (repl-buffer)))
    (when buffer
      (setf (variable-value 'lem/listener-mode:listener-set-prompt-function :buffer buffer)
            'repl-set-prompt
            (variable-value 'lem/listener-mode:listener-check-input-function :buffer buffer)
            'repl-paren-correspond-p
            (variable-value 'lem/listener-mode:listener-execute-function :buffer buffer)
            'repl-eval))))

(defun repl-change-read-line-input ()
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function)
        #'identity
        (variable-value 'lem/listener-mode:listener-check-input-function)
        (constantly t)
        (variable-value 'lem/listener-mode:listener-execute-function)
        'repl-read-line))

(defun clear-repl ()
  (when (repl-buffer)
    (lem/listener-mode:clear-listener (repl-buffer))))

(defun get-repl-window ()
  (let ((buffer (repl-buffer)))
    (when buffer
      (if (eq buffer (window-buffer (current-window)))
          (current-window)
          (first (get-buffer-windows buffer))))))

(defun repl-buffer-width ()
  (alexandria:when-let* ((window (get-repl-window))
                         (width (- (window-width window) 2)))
    width))

(defun repl-highlight-notes (notes)
  (let ((buffer (repl-buffer)))
    (when buffer
      (dolist (note notes)
        (trivia:match note
          ((and (trivia:property :location location)
                (trivia:property :message _))
           (let* ((xref-loc (source-location-to-xref-location location))
                  (offset (xref-location-position xref-loc)))
             (with-point ((start (buffer-point buffer)))
               (move-point start (lem/listener-mode:input-start-point buffer))
               (form-offset start -1)
               (character-offset start (if (plusp offset) (1- offset) offset))
               (with-point ((end start))
                 (form-offset end 1)
                 (put-text-property start end :attribute 'compiler-note-attribute))))))))))

(defun repl-completion (point)
  (with-point ((p point))
    (cond ((maybe-beginning-of-string p)
           (character-offset p 1)
           (let ((str (points-to-string p point)))
             (mapcar (lambda (filename)
                       (make-completion-item :label filename
                                             :start p
                                             :end point))
                     (completion-file str (lem:buffer-directory (point-buffer p))))))
          (t
           (completion-symbol p)))))

(defvar *repl-compiler-check* nil)

(defvar *repl-temporary-file*
  (merge-pathnames "slime-repl.tmp" (uiop:temporary-directory)))

(defun repl-eval (point string)
  (declare (ignore point))
  (check-connection)
  (cond
    (*repl-compiler-check*
     (with-open-file (stream *repl-temporary-file*
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
       (write-string string stream))
     (let ((result
             (let ((*write-string-function* (constantly nil)))
               (lisp-eval `(swank:compile-file-for-emacs *repl-temporary-file* nil)))))
       (destructuring-bind (notes successp duration loadp fastfile)
           (cdr result)
         (declare (ignore successp duration loadp fastfile))
         (repl-highlight-notes notes)
         (listener-eval string))))
    (t
     (listener-eval string))))

(defparameter *record-history-of-repl* nil)
(defvar *repl-history* '())

(defun listener-eval (string)
  (ensure-repl-buffer-exist)
  (request-listener-eval
   *connection*
   string
   (lambda (value)
     (declare (ignore value))
     (lem/listener-mode:refresh-prompt (ensure-repl-buffer-exist))
     (when *record-history-of-repl*
       (start-timer (make-idle-timer
                     (lambda ()
                       (when (position-if (complement #'syntax-space-char-p) string)
                         (push (cons string (lisp-eval-from-string "CL:/" "CL"))
                               *repl-history*))))
                    0)))
   (repl-buffer-width)))

(defun repl-read-string (thread tag)
  (let ((buffer (ensure-repl-buffer-exist)))
    (push thread (read-string-thread-stack))
    (push tag (read-string-tag-stack))
    (setf (current-window) (pop-to-buffer buffer))
    (buffer-end (current-point))
    (lem/listener-mode:change-input-start-point (current-point))
    (repl-change-read-line-input)))

(defun repl-pop-stack ()
  (let ((thread (pop (read-string-thread-stack)))
        (tag (pop (read-string-tag-stack))))
    (when (null (read-string-thread-stack))
      (repl-reset-input))
    (values thread tag)))

(defun repl-abort-read (thread tag)
  (declare (ignore thread tag))
  (repl-pop-stack)
  (message "Read aborted"))

(defun repl-read-line (point string)
  (declare (ignore point))
  (multiple-value-bind (thread tag) (repl-pop-stack)
    (dispatch-message (list :emacs-return-string
                            thread
                            tag
                            (concatenate 'string
                                         string
                                         (string #\newline))))))

(define-command start-lisp-repl (&optional (use-this-window nil)) ("P")
  (check-connection)
  (flet ((switch (buffer split-window-p)
           (if split-window-p
               (setf (current-window) (pop-to-buffer buffer))
               (switch-to-buffer buffer))))
    (lem/listener-mode:listener-start
     "*lisp-repl*"
     'lisp-repl-mode
     :switch-to-buffer-function (alexandria:rcurry #'switch (not use-this-window)))))

(define-command lisp-switch-to-repl-buffer () ()
  (let ((buffer (repl-buffer)))
    (if buffer
        (setf (current-window) (pop-to-buffer buffer))
        (start-lisp-repl))))

(defun copy-down-to-repl (slimefun &rest args)
  (unless (find-package :swank-repl)
    (make-package :swank-repl))
  (lisp-eval-async
   `(,(read-from-string "swank-repl::listener-save-value") ',slimefun ,@args)
   (lambda (result)
     (declare (ignore result))
     (lisp-eval-async
      `(,(read-from-string "swank-repl::listener-get-value"))
      (lambda (result)
        (declare (ignore result))
        (lem/listener-mode:refresh-prompt (ensure-repl-buffer-exist)))))))

(defun write-string-to-repl (string)
  (let ((buffer (ensure-repl-buffer-exist)))
    (with-point ((start (buffer-end-point buffer) :left-inserting))
      (when (text-property-at start :field -1)
        (insert-character start #\newline))
      (insert-escape-sequence-string (buffer-end-point buffer) string))
    (lem/listener-mode:change-input-start-point (buffer-end-point buffer))
    (buffer-end (buffer-point buffer))
    (alexandria:when-let ((window (get-repl-window)))
      (with-current-window window
        (buffer-end (buffer-point buffer))
        (window-see window)))))

(defvar *escape-sequence-argument-specs*
  '(("0" :bold-p nil :reverse-p nil :underline-p nil)
    ("1" :bold-p t)
    ("2")
    ("4" :underline-p t)
    ("5")
    ("7" :reverse-p t)
    ("8")
    ("22" :bold-p nil)
    ("24" :underline-p nil)
    ("25")
    ("27" :reverse-p nil)
    ("28")

    ("30" :foreground "black")
    ("31" :foreground "red")
    ("32" :foreground "green")
    ("33" :foreground "yellow")
    ("34" :foreground "blue")
    ("35" :foreground "magenta")
    ("36" :foreground "cyan")
    ("37" :foreground "white")

    ("40" :background "black")
    ("41" :background "red")
    ("42" :background "green")
    ("43" :background "yellow")
    ("44" :background "blue")
    ("45" :background "magenta")
    ("46" :background "cyan")
    ("47" :background "white")

    ("90" :foreground "dim gray")
    ("91" :foreground "red")
    ("92" :foreground "green")
    ("93" :foreground "yello")
    ("94" :foreground "royalblue")
    ("95" :foreground "darkorchid1")
    ("96" :foreground "cyan1")
    ("97" :foreground "white")

    ("100" :background "dim gray")
    ("101" :background "red")
    ("102" :background "green")
    ("103" :background "yello")
    ("104" :background "royalblue")
    ("105" :background "darkorchid1")
    ("106" :background "cyan1")
    ("107" :background "white")))

(defun raw-seq-to-attribute (string)
  (let ((arguments (uiop:split-string string :separator '(#\;)))
        (attribute-parameters '()))
    (dolist (argument arguments)
      (alexandria:when-let (spec (assoc argument *escape-sequence-argument-specs*
                                        :test #'string=))
        (loop :for (key value) :on (rest spec) :by #'cddr
              :do (setf (getf attribute-parameters key) value))))
    (apply #'make-attribute attribute-parameters)))

(defun split-escape-sequence-string (string)
  (let ((acc '())
        (pos 0))
    (loop
      (multiple-value-bind (start end reg-starts reg-ends)
          (ppcre:scan "\\e\\[([^m]*)m" string :start pos)
        (unless (and start end reg-starts reg-ends) (return))
        (unless (= pos start)
          (push (subseq string pos start) acc))
        (push (raw-seq-to-attribute
               (subseq string
                       (aref reg-starts 0)
                       (aref reg-ends 0)))
              acc)
        (setf pos end)))
    (push (subseq string pos) acc)
    (nreverse acc)))

(defun parse-escape-sequence (string)
  (split-escape-sequence-string string))

(defun insert-escape-sequence-string (point string)
  (let ((tokens (parse-escape-sequence string))
        (current-attribute nil))
    (dolist (token tokens)
      (etypecase token
        (null
         (setf current-attribute nil))
        (attribute
         (setf current-attribute token))
        (string
         (insert-string point token :attribute current-attribute))))))

(define-repl-shortcut sayonara (n)
  (declare (ignorable n))
  (if (self-connection-p *connection*)
      (message "Can't say sayonara because it's self connection.")
      (interactive-eval "(swank:quit-lisp)")))
