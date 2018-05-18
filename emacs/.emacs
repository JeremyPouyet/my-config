;;; Emacs loading:

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq user-full-name "Jérémy Pouyet"
  user-mail-address "jeremy.pouyet@gmail.com")

;;; Install and initialize packages:

(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;;; Module loading:

(use-package auto-complete
  :config (global-auto-complete-mode t))

;; limit the number of column per line
(use-package fill-column-indicator
  :init
  (setq fci-rule-column 90)
  (setq fci-rule-character-color "#00F")
  :config (add-hook 'prog-mode-hook (lambda () (fci-mode t))))

;; line numbers
(use-package nlinum
  :config (add-hook 'prog-mode-hook (lambda () (nlinum-mode t))))

(use-package highline
  :config (global-highline-mode 1))

;; (use-package speedbar
;;   :init
;;   (defconst my-speedbar-buffer-name "SPEEDBAR")
;;   (setq speedbar-buffer (get-buffer-create my-speedbar-buffer-name)
;;     speedbar-frame (selected-frame)
;;     dframe-attached-frame (selected-frame)
;;     speedbar-select-frame-method 'attached
;;     speedbar-verbosity-level 0
;;     speedbar-last-selected-file nil)
;;   (setq right-window (split-window-horizontally 24))
;;   (setq left-window  (frame-first-window))
;;   :config
;;   (set-buffer speedbar-buffer)
;;   (speedbar-mode)
;;   (speedbar-reconfigure-keymaps)
;;   (speedbar-update-contents)
;;   (speedbar-set-timer 1)
;;   (set-window-buffer left-window "SPEEDBAR")
;;   (set-window-dedicated-p left-window t)
;;   (read-only-mode)
;;   (select-window right-window)
;;   (defun select-right-window () (select-window right-window)))

(use-package editorconfig
  :config (editorconfig-mode 1))

;;; Ansi term:
(ansi-term "/bin/bash")

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
          (proc (get-buffer-process buff)))
    (set-process-sentinel
      proc
      `(lambda (process event)
         (if (string= event "finished\n")
           (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

;; (defun toolbear:term-handle-more-ansi-escapes (proc char)
;;   "Handle additional ansi escapes."
;;   (cond
;;    ;; \E[nG - Cursor Horizontal Absolute, e.g. move cursor to column n
;;    ((eq char ?G)
;;     (let ((col (min term-width (max 0 term-terminal-parameter))))
;;       (term-move-columns (- col (term-current-column)))))
;;    (t)))
;; (advice-add 'term-handle-ansi-escape :before #'toolbear:term-handle-more-ansi-escapes)

;;; Easier opertaions:

;; enable clipboard in emacs with C-shift-v
(setq select-enable-clipboard t)

;; go to function definition
(dumb-jump-mode 1)

;; indent a whole file
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;; auto add closing brackets
(electric-pair-mode 1)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;; Languages specific syntax:
;; Ruby syntax highlighting
(use-package ruby-mode
  :mode "\\.\\(?:gemspec\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'"
  :interpreter "ruby"
  :config (yard-mode))

;; javascript
(use-package js2-mode
  :mode
  ("\\.js\\'" . js2-mode)
  ("\\.js.erb\\'" . js2-mode)
  ("\\.jsx\\'" . js2-jsx-mode)
  :interpreter "node"
  :config
  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t)
  (setq js2-mode-assume-strict t)
  (setq js2-strict-cond-assign-warning t)
  :init
  (defvar custom-js-externs-global '("it" "describe" "before" "after" "afterEach" "expect" "logger" "requires"))
  (defvar js2-global-externs custom-js-externs-global)
  (defvar js2-include-node-externs t)
  (defvar js-indent-level 2))

; pip install pylint
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; Remove unwanted buffers and warnings:

;; type y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
    (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; no annoying warning
(use-package cl-lib
  :config
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (cl-letf (((symbol-function #'process-list) (lambda ())))
      ad-do-it)))

;; margins
(fringe-mode 0)

;; Disable toolbar
(tool-bar-mode -1)

;; do not generate backup files
(setq make-backup-files nil)

;;; Emacs custom variables:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(delete-selection-mode nil)
 '(doc-view-continuous t)
 '(inhibit-startup-screen t)
  '(package-selected-packages
     (quote
       (dumb-jump yard-mode use-package python-mode nlinum js2-mode highline flycheck fill-column-indicator editorconfig auto-complete anaconda-mode)))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t))

(defadvice hi-lo3ck-set-pattern (around use-overlays activate)
  (let ((font-lock-fontified nil))
    ad-do-it))

;;; Indentation:

;; change size of tabs to 2 spaces
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode 2)

;; To customize the background color
(set-face-background 'highline-face "#444")

;; show column
(column-number-mode t)

;; show file size
(size-indication-mode t)

;;; Scrolling:
;; enable horizontal scroll
(set-default 'truncate-lines t)
(scroll-bar-mode -1)

;; smooth scrolling
(setq scroll-margin 0
  scroll-conservatively 10000
  scroll-preserve-screen-position 1
  scroll-step 1)

;; Newline at end of file
(setq require-final-newline t)

;; delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Key binding:

;; Automaticaly indent when enter is pressed
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key (kbd "C-<left>") 'backward-word)
(global-set-key (kbd "C-x M-[ c") 'next-buffer)
(global-set-key (kbd "C-x M-[ d") 'previous-buffer)
(global-set-key (kbd "C-DEL") 'kill-word)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key [f1] 'kill-buffer)
(global-set-key [f2] 'delete-window)
(global-set-key [f3] 'split-window-vertically)
(global-set-key [f4] 'split-window-horizontally)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'recompile)
(global-set-key (kbd "C-c <C-left>") 'backward-sexp)
(global-set-key (kbd "C-c <C-right>") 'forward-sexp)
(global-set-key (kbd "C-c C-c") 'kill-whole-line)
(global-set-key (kbd "M-S") 'query-replace-regexp)
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "C-M-*") 'windresize)
(global-set-key (kbd "C-M-o") 'find-file-at-point)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Chapter ((t (:background "black" :foreground "red"))))
 '(border ((t (:background "black" :foreground "#2275f7"))))
 '(done ((t (:foreground "ForestGreen" :weight bold))))
 '(file-name-shadow ((t (:inherit shadow :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#F33"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "medium orchid"))))
 '(font-lock-comment-face ((t (:foreground "medium orchid"))))
 '(font-lock-constant-face ((t (:foreground "#ff3333"))))
 '(font-lock-function-name-face ((t (:foreground "#ff3333"))))
 '(font-lock-keyword-face ((t (:foreground "#ffc305" :weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face))))
 '(font-lock-string-face ((t (:foreground "#0D3"))))
 '(font-lock-type-face ((t (:foreground "yellow"))))
 '(font-lock-variable-name-face ((t (:foreground "cyan"))))
 '(gnus-cite-1 ((t (:foreground "color-99"))))
 '(gnus-header-content ((t (:foreground "brightmagenta" :slant italic))))
 '(gnus-header-name ((t (:foreground "blue"))))
 '(gnus-header-newsgroups ((t (:foreground "brightblue" :slant italic))))
 '(gnus-header-subject ((t (:foreground "red2"))))
 '(hi-pink ((t (:foreground "#dd4400" :weight bold))))
 '(hi-yellow ((t (:foreground "#bdbd00" :weight bold))))
 '(isearch-fail ((t (:background "#4400aa"))))
 '(js2-function-call ((t (:foreground "dark gray"))))
 '(js2-function-param ((t (:foreground "PeachPuff3"))))
 '(js2-jsdoc-type ((t (:foreground "brightblue"))))
 '(match ((t (:background "#111111"))))
 '(message-header-cc ((t (:foreground "green"))))
 '(minibuffer-prompt ((t (:foreground "#0AF"))))
 '(question ((t (:background "color-235" :foreground "cyan"))) t)
 '(region ((t (:background "blue"))))
 '(term-color-blue ((t (:background "deep sky blue" :foreground "deep sky blue"))))
 '(trailing-whitespace ((t nil)))
 '(warning ((t (:foreground "DarkOrange" :weight bold))))
 '(whitespace-newline ((t nil)))
 '(whitespace-space ((t (:foreground "#888"))))
 '(whitespace-tab ((t (:background "#777" :foreground "#bbb"))))
 '(widget-button ((t (:foreground "#00aa00" :weight bold)))))
