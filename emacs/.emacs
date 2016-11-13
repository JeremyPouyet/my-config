;; add the load path to the default one
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
;; loading
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(ac-config-default)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;;*****************************************
;;add ruby syntax highlighting
(add-to-list 'auto-mode-alist
	     '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
	     '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(require 'linum)
(require 'highline)
(require 'sr-speedbar)
(require 'markdown-mode)

(add-hook 'ruby-mode-hook 'yard-mode)

;; javascript
(setq js-indent-level 2)
(setq js2-include-node-externs t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; enable horizontal scroll!
(set-default 'truncate-lines t)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

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

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;;****************************************
(sr-speedbar-toggle)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(delete-selection-mode nil)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(js2-strict-cond-assign-warning t)
 '(mark-even-if-inactive t)
 '(show-paren-mode t)
 '(speedbar-before-popup-hook nil)
 '(speedbar-before-visiting-file-hook (quote (push-mark)))
 '(speedbar-default-position (quote left))
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 20)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . 0)
     (left-fringe . 1))))
 '(speedbar-show-unknown-files t)
 '(sr-speedbar-width-console 50)
 '(sr-speedbar-width-x 50))
; '(tooltip-mode nil))

(defadvice hi-lock-set-pattern (around use-overlays activate)
  (let ((font-lock-fontified nil))
    ad-do-it))
;;****************************************
;; Highline regexps

;; (defun done nil
;;   (highlight-regexp "\\[100%\\]" 'success))
;; (add-hook 'find-file-hook 'done)

;; (defun not_done nil
;;   (highlight-regexp "\\[[^1]..%\\]" 'error))
;; (add-hook 'find-file-hook 'not_done)

;; (defun comchapter nil
;;   (highlight-regexp "\\[#[0-9]+\\]" 'border))
;; (add-hook 'find-file-hook 'comchapter)

;; (defun fixme_file nil
;;   (highlight-regexp "\\(BEGINFIXME[ ]?:\\|ENDFIXME\\)" 'warning))
;; (add-hook 'find-file-hook 'fixme_file)

;; (defun login nil
;;   (highlight-regexp "[a-z][-a-z]\\{,5\\}_[a-z]{1}" 'minibuffer-prompt))
;; (add-hook 'find-file-hook 'login)

;; (defun fixme_hl nil
;;   (highlight-regexp "FIXME" 'font-lock-constant-face))
;; (add-hook 'find-file-hook 'fixme_hl)

;; (defun comchapter nil
;;   (highlight-regexp "\\[#[0-9]+\\]" 'border))
;; (add-hook 'find-file-hook 'comchapter)

;; (defun fixme_hl nil
;;   (highlight-regexp "FIXME" 'font-lock-constant-face))
;; (add-hook 'find-file-hook 'fixme_hl)

;; (defun fixme_file nil
;;   (highlight-regexp "\\(BEGINFIXME[ ]?:\\|ENDFIXME\\)" 'font-lock-variable-name-face))
;; (add-hook 'find-file-hook 'fixme_file)

;; (defun login nil
;;   (highlight-regexp "\\<[a-z][-a-z]\\{,5\\}_[a-z]\\>" 'success))
;; (add-hook 'find-file-hook 'login)

;; (defun fixme_hl_not_sure nil
;;   (highlight-regexp "FIXME~" 'warning))
;; (add-hook 'find-file-hook 'fixme_hl_not_sure)

;; (defun tc_lvl nil
;;   (highlight-regexp "\\<TC[0-9]\\>" 'font-lock-constant-face))
;; (add-hook 'find-file-hook 'tc_lvl)


;;****************************************
;; Better gdb

(setq-default gdb-many-windows t)

;; enable fci mode for all files
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'c++-mode-hook 'fci-mode)

;; columns to fill
(setq-default fci-rule-column '80)
;; color of the filling characters
(setq fci-rule-character-color "#00F")

;;****************************************
;; line numbers

;;activate linum mode by default
(global-linum-mode 1)

;;****************************************
;; accolades indentation

;; Indente automatiquement lorsque l'on appuie sur Entree
(global-set-key (kbd "RET") 'newline-and-indent)
;; gere les indentations d'accolades
(setq c-default-style "k&r")

;;****************************************
;; indentation C

;; change size of tabs to 2 spaces
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode 2)

;;****************************************
;; highlight

;;activate highline
(global-highline-mode 1)

;; To customize the background color
(set-face-background 'highline-face "#444")

;;****************************************
;;

;;show column
(column-number-mode 1)

;;****************************************
;; delete trailing whitespaces

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;****************************************
;; replace tabs by spaces

;; (add-hook 'c-mode-hook
;;           '(lambda ()
;;              (add-hook 'before-save-hook
;;                        (lambda ()
;;                          (untabify (point-min) (point-max))))))
;; (add-hook 'c++-mode-hook
;;           '(lambda ()
;;              (add-hook 'before-save-hook
;;                        (lambda ()
;;                          (untabify (point-min) (point-max))))))


;; Key binding
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key (kbd "C-<left>") 'backward-word)
(global-set-key (kbd "C-x M-[ c") 'next-buffer)
(global-set-key (kbd "C-x M-[ d") 'previous-buffer)
(global-set-key (kbd "C-DEL") 'kill-word)
(global-set-key (kbd "C-h") 'backward-kill-word)
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
(global-set-key [f7] 'list-buffers)
(global-set-key [f8] 'list-bookmarks)
(global-set-key (kbd "C-c <C-left>") 'backward-sexp)
(global-set-key (kbd "C-c <C-right>") 'forward-sexp)
(global-set-key (kbd "C-c C-c") 'kill-whole-line)
(global-set-key (kbd "M-S") 'query-replace-regexp)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "C-d") 'kill-whole-line)

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
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "red"))))
 '(font-lock-constant-face ((t (:foreground "#ff3333"))))
 '(font-lock-function-name-face ((t (:foreground "#09f"))))
 '(font-lock-keyword-face ((t (:foreground "yellow"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face))))
 '(font-lock-string-face ((t (:foreground "#0D3"))))
 '(font-lock-type-face ((t (:foreground "#0E0"))))
 '(font-lock-variable-name-face ((t (:foreground "cyan"))))
 '(gnus-cite-1 ((t (:foreground "color-99"))))
 '(gnus-header-content ((t (:foreground "brightmagenta" :slant italic))))
 '(gnus-header-name ((t (:foreground "blue"))))
 '(gnus-header-newsgroups ((t (:foreground "brightblue" :slant italic))))
 '(gnus-header-subject ((t (:foreground "red2"))))
 '(hi-pink ((t (:foreground "#dd4400" :weight bold))))
 '(hi-yellow ((t (:foreground "#bdbd00" :weight bold))))
 '(highline-face ((t (:background "#444"))))
 '(isearch-fail ((t (:background "#4400aa"))))
 '(js2-function-param ((t (:foreground "color-127"))))
 '(js2-jsdoc-html-tag-delimiter ((t (:foreground "color-214"))))
 '(lazy-highlight ((t (:background "magenta" :foreground "cyan"))))
 '(linum ((t (:inherit (shadow default) :background "color-235" :foreground "Orange"))))
 '(match ((t (:background "#111111"))))
 '(message-header-cc ((t (:foreground "green"))))
 '(minibuffer-prompt ((t (:foreground "#0AF"))))
 '(question ((t (:background "color-235" :foreground "cyan"))) t)
 '(region ((t (:background "blue"))))
 '(trailing-whitespace ((t nil)))
 '(warning ((t (:foreground "DarkOrange" :weight bold))))
 '(whitespace-newline ((t nil)))
 '(whitespace-space ((t (:foreground "#888"))))
 '(whitespace-tab ((t (:background "#777" :foreground "#bbb"))))
 '(widget-button ((t (:foreground "#00aa00" :weight bold)))))
