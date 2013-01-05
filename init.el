(require 'package)

(defun arrange-frame (w h x y)
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))

                                        ; (arrange-frame 190 90 1200 5)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; cargo cult form melpa
(defadvice package-compute-transaction
  (before
   package-compute-transaction-reverse (package-list requirements)
   activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))

(package-initialize)

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-ruby
                      starter-kit-eshell
                      auto-complete
                      clojure-mode
                      midje-mode
                      nrepl
                      ac-nrepl))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

(set-frame-height
  (selected-frame)
  (/ (display-pixel-height) (frame-char-height)))

(set-frame-width
  (selected-frame)
  (/ (/ (display-pixel-width) 2) (frame-char-width)))

(set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

;; solarized
(load-theme 'solarized-dark t)

;; minor textmate mode
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

;; whitespace
(require 'whitespace)
(add-hook 'clojure-mode-hook 'whitespace-mode)
(setq whitespace-line-column 110)

;; auto complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-delay 0.3)

;; rebind save to cleanup crap
(add-hook 'before-save-hook 'esk-cleanup-buffer)

;; clojure mode
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; (require 'clojure-test-mode)

(require 'midje-mode)

(defun midje-display-reward ()
  (t 'clojure-test-success-face))

;; nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(setq nrepl-popup-stacktraces nil)

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; line numbers
(global-linum-mode t)

;; change dir
(cd "~/Projects/getaroom/price_sheet")
