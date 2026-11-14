(defpackage :lem-webview/darwin
  (:use :cl)
  #+(and darwin arm64)
  (:import-from :cl-objc
                :invoke)
  (:export :set-window-appearance
           :dispatch-set-window-appearance))
(in-package :lem-webview/darwin)

;;; macOS window appearance via cl-objc
;;;
;;; Sets NSAppearance on the webview's NSWindow with cl-objc's INVOKE,
;;; which looks up each method's signature in the Objective-C runtime.
;;; set-window-appearance takes a webview handle (for pre-event-loop use).
;;; dispatch-set-window-appearance uses GCD + [NSApp keyWindow] so the
;;; editor thread can change appearance without holding a native handle.
;;;
;;; cl-objc supports macOS on arm64 only; elsewhere these are no-ops.

#+(and darwin arm64)
(progn

;; Grand Central Dispatch — dispatch work to the main thread without
;; needing a webview handle. libdispatch is plain C, not Objective-C,
;; so it is called through CFFI directly.
(cffi:defcfun ("dispatch_async_f" dispatch-async-f) :void
  (queue :pointer)
  (context :pointer)
  (work :pointer))

(defun main-dispatch-queue ()
  "Return the main dispatch queue (dispatch_get_main_queue)."
  (cffi:foreign-symbol-pointer "_dispatch_main_q"))

(defun %appearance-name (mode)
  "Return the NSAppearance name for MODE (:dark or :light)."
  (ecase mode
    (:dark "NSAppearanceNameDarkAqua")
    (:light "NSAppearanceNameAqua")))

(defun %get-key-window ()
  "Return the NSWindow that is the current key window, or the nil object."
  (invoke (invoke 'ns-application shared-application) key-window))

(defun %set-nswindow-appearance (nswindow mode)
  "Set NSAppearance on NSWINDOW to MODE (:dark or :light).
NSWINDOW is a cl-objc object or a raw foreign pointer.
Must be called on the main thread. Returns MODE, or NIL when there is
no window or the Objective-C call fails."
  (let ((window (if (cffi:pointerp nswindow)
                    (cffi:convert-from-foreign nswindow 'objc-cffi:objc-id)
                    nswindow)))
    (when (or (null window) (objc-cffi:objc-nil-object-p window))
      (return-from %set-nswindow-appearance nil))
    ;; This runs inside a GCD callback on the main thread, where an
    ;; unhandled Lisp error would stop the native event loop.
    (handler-case
        (let ((appearance (invoke 'ns-appearance
                                  :appearance-named
                                  (invoke 'ns-string
                                          :string-with-utf8-string
                                          (%appearance-name mode)))))
          (unless (objc-cffi:objc-nil-object-p appearance)
            (invoke window :set-appearance appearance)
            mode))
      (error (e)
        (log:error "Failed to set window appearance to ~S: ~A" mode e)
        nil))))

;; GCD callbacks — dispatch_async_f signature: void (*)(void *)
(cffi:defcallback gcd-set-dark-appearance :void ((context :pointer))
  (declare (ignore context))
  (%set-nswindow-appearance (%get-key-window) :dark))

(cffi:defcallback gcd-set-light-appearance :void ((context :pointer))
  (declare (ignore context))
  (%set-nswindow-appearance (%get-key-window) :light))

(defun set-window-appearance (webview-handle mode)
  "Set the macOS window frame appearance for WEBVIEW-HANDLE.
MODE should be :dark or :light. Called before the event loop starts,
so it runs directly on the main thread with the handle available."
  (%set-nswindow-appearance (webview:webview-get-window webview-handle) mode))

(defun dispatch-set-window-appearance (mode)
  "Set the macOS window frame appearance via GCD main queue dispatch.
Uses [NSApp keyWindow] to find the window, so no webview handle is needed.
Safe to call from any thread."
  (dispatch-async-f (main-dispatch-queue)
                    (cffi:null-pointer)
                    (ecase mode
                      (:dark (cffi:callback gcd-set-dark-appearance))
                      (:light (cffi:callback gcd-set-light-appearance))))
  mode)

) ; end #+(and darwin arm64) progn

#-(and darwin arm64)
(progn
(defun set-window-appearance (webview-handle mode)
  "No-op: window appearance is only supported on macOS (arm64)."
  (declare (ignore webview-handle mode))
  nil)
(defun dispatch-set-window-appearance (mode)
  "No-op: window appearance is only supported on macOS (arm64)."
  (declare (ignore mode))
  nil)
)
