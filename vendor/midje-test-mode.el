;;; midje-test-mode.el --- Minor mode for expectations tests

;; Author: Gareth Jones <gareth.e.jones@gmail.com>
;; Version: 0.0.4
;; Keywords: languages, lisp, test
;; Package-Requires: ((nrepl "0.1.5") (clojure-mode "1.11"))

;;; Code:

(require 'clojure-mode)
(require 'nrepl)

(defface midje-failure-face
  '((((class color) (background light))
     :background "orange red")
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failures in midje tests."
  :group 'midje-mode)

(defface midje-error-face
  '((((class color) (background light))
     :background "orange1")
    (((class color) (background dark))
     :background "orange4"))
  "Face for errors in midje tests."
  :group 'midje-mode)

(defface midje-success-face
  '((((class color) (background light))
     :foreground "black"
     :background "green")
    (((class color) (background dark))
     :foreground "black"
     :background "green"))
  "Face for success in midje tests."
  :group 'midje-mode)

;; vars to keep count of all/failed/errored tests

(defvar midje-count         0)
(defvar midje-failure-count 0)
(defvar midje-error-count   0)

(defconst midje-valid-results
  '(:success :fail :error)
  "Results we are interested in reporting on")

(defun midje-response-handler (callback)
  (lexical-let ((buffer (current-buffer))
                (callback callback))
    (nrepl-make-response-handler buffer
                                 (lambda (buffer value)
                                   (funcall callback buffer value))
                                 (lambda (buffer value)
                                   (nrepl-emit-interactive-output value))
                                 (lambda (buffer err)
                                   (message (format "%s" err)))
                                 '())))

(defun midje-eval (string &optional handler)
  (nrepl-send-string string
                     (midje-response-handler (or handler #'identity))
                     (nrepl-current-ns)))

(defun midje-test-clear (&optional callback)
  "Clear all counters and unmap generated vars for midje"
  (interactive)
  (remove-overlays)
  (setq midje-count         0
        midje-failure-count 0
        midje-error-count   0)
  (nrepl-load-current-buffer)
  (midje-eval
   "(do
      (require 'midje.repl)
      (doseq [[a b] (ns-interns *ns*)
              :when ((meta b) :expectation)]
        (ns-unmap *ns* a)))"
   callback))

(defun midje-highlight-problem (line event msg)
  (save-excursion
    (goto-line line)
    (let ((beg (point)))
      (end-of-line)
      (let ((overlay (make-overlay beg (point))))
        (overlay-put overlay 'face (if (equal event :fail)
                                       'midje-failure-face
                                     'midje-error-face))
        (overlay-put overlay 'message msg)))))

(defun midje-inc-counter-for (event)
  (when (member event midje-valid-results)
    (incf midje-count))
  (cond
   ((equal :fail event)  (incf midje-failure-count))
   ((equal :error event) (incf midje-error-count))))

(defun midje-extract-result (result)
  (midje-inc-counter-for (car result))
  (when (or (eq :fail (car result))
            (eq :error (car result)))
    (destructuring-bind (event msg line) (coerce result 'list)
      (midje-highlight-problem line event msg))))

(defun midje-echo-results ()
  (message
   (propertize
    (format "Ran %s tests. %s failures, %s errors."
            midje-count midje-failure-count
            midje-error-count)
    'face
    (cond ((not (= midje-error-count 0)) 'midje-error-face)
          ((not (= midje-failure-count 0)) 'midje-failure-face)
          (t 'midje-success-face)))))

(defun midje-extract-results (buffer value)
  (with-current-buffer buffer
    (let ((results (read value)))
      (mapc #'midje-extract-result results)
      (midje-echo-results))))

(defun midje-run-and-extract-results (buffer value)
  (with-current-buffer buffer
    (nrepl-load-current-buffer)
    (midje-eval
     "(do
        (println \"running midje tests\")
        (midje.repl/check-facts [*ns*])
        (for [[n s] (ns-interns *ns*)
              :let [m (meta s)]
              :when (: m)]
          (apply list (:status m))))"
     #'midje-extract-results)))

(defun midje-run-tests ()
  "Run all the tests in the current namespace."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (message "Testing midje factso...")
  (save-window-excursion
    (midje-test-clear #'midje-run-and-extract-results)))

(defun midje-show-result ()
  (interactive)
  (let ((overlay (find-if (lambda (o) (overlay-get o 'message))
                          (overlays-at (point)))))
    (if overlay
        (message (replace-regexp-in-string "%" "%%"
                                           (overlay-get overlay 'message))))))

(defvar midje-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ,")   'midje-run-tests)
    (define-key map (kbd "C-c C-,") 'midje-run-tests)
    (define-key map (kbd "C-c k")   'midje-test-clear)
    (define-key map (kbd "C-c '")   'midje-show-result)
    map))

;;;###autoload
(define-minor-mode midje-test-mode
  "A minor mode for running midje tests"
  nil " Midje" midje-mode-map)

;;;###autoload
(progn
  (defun midje-maybe-enable ()
    "Enable midje-mode and disable clojure-test-mode if
the current buffer contains a namespace with a \"test.\" bit on
it."
    (let ((ns (clojure-find-package)))  ; defined in clojure-mode.el
      (when (or (search "midje.sweet" ns)
                (search "-midje" ns))
        (save-window-excursion
          (midje-mode t)
          (clojure-test-mode 0)))))
  (add-hook 'clojure-mode-hook 'midje-maybe-enable))

(provide 'midje-test-mode)

;;; midje-mode.el ends here
