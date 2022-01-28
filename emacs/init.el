;;; init.el --- Init file

;;; Commentary:

;;;; Ubuntu configuration
;;; Get lateset Emacs version
;; sudo add-apt-repository -y ppa:ubuntu-elisp/ppa
;; sudo apt update
;; sudo apt install emacs-snapshot
;;; EAF dependencies
;; # browser dependency
;; sudo apt install python3-pyqt5.qtwebengine
;; # file qr code file transfer
;; sudo apt install python3-qrcode
;; # markdown
;; sudo apt install grip
;;; Flycheck dependencies
;; # flycheck eslint
;; npm install -g eslint
;; # flycheck pylint
;; python3.8 -m pip install mypy pylint

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;; Install and initialize packages:

;; Module loading:
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; autocomplete with company
(use-package company
  :defines company-dabbrev-downcase
  :init
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)                ; No delay in showing suggestions.
  (setq company-minimum-prefix-length 1)     ; Show suggestions after entering one character.
  (setq company-selection-wrap-around t)     ; Use tab key to cycle through suggestions.
  (setq company-show-quick-access t)         ; visual numbering of candidates (complete alt+nb)
  (setq company-tooltip-align-annotations t) ; align annotations to the right side
  (setq company-tooltip-minimum 4)           ; tooltip is displayed above point when less than
                                               ; 4 suggestions can be displayed bellow point
  (setq company-text-icons-add-background t)
  (company-tng-mode)                         ; ('tng' means 'tab and go')
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package editorconfig
  :config (editorconfig-mode 1))

;;;; Custom functions
(defun duplicate-line()
  "Duplicate current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
  )

(defun indent-buffer ()
  "Indent a whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

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

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
ARG: Number of line to copy"
  (interactive "p")
  (kill-ring-save (line-beginning-position) (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun insert-console-log ()
  (interactive)
  (insert (concat "console.log('" (number-to-string (line-number-at-pos)) " " (buffer-name) "');")))

;;;; Languages specific syntax:
;;; python
;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;;             (setq indent-tabs-mode nil)
;;             (setq tab-width 4)
;;             (setq-default python-indent-offset 4)))

;;; Javascript
(use-package js2-mode
  :mode ("\\js$" . js2-mode)
  :interpreter "node"
  :init
  ;; Allow private class fields starting by # while waiting js2-mode native detection
  (advice-add #'js2-identifier-start-p :after-until (lambda (c) (eq c ?#)))
  :config
  (setq js2-basic-offset 2
        js-indent-level 2
        js2-bounce-indent-p t)
  ;; Do not reindent lines after ENTER Is pressed or it is edited
  (add-hook 'js2-mode-hook (lambda () (setq-local electric-indent-inhibit t)))
  (global-set-key (kbd "M-c") 'insert-console-log)
  (setq-default js-indent-align-list-continuation nil
                js2-pretty-multiline-declarations t)
  )

(use-package flycheck
  :after (js2-mode)
  :config
  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
  ;; use eslint with js2-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  ;; vue
  ;; (flycheck-add-mode 'javascript-eslint 'vue-mode)

  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(python-flake8)))
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(python-mypy)))
  :init (global-flycheck-mode)
  )

;; Json
(use-package json-mode
  :mode "\\.json$"
  )

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; Customize emacs!
;; highlight cursor on buffer change
(defvar pulse-delay 0.04)
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(windmove-left windmove-right windmove-up windmove-down))
  (advice-add command :after #'pulse-line))

;; prettier dired-mode
(use-package diredfl
  :config (diredfl-global-mode t))

;; Remove unwanted buffers and warnings:
(defun remove-scratch-buffer ()
  "Remove *scratch* from buffer after the mode has been set."
  (if (get-buffer "*scratch*")
    (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
;; (setq-default message-log-max nil)
;; (if (get-buffer "*Messages*") (kill-buffer "*Messages*"))
;; Disabled *Completions*
(add-hook 'minibuffer-exit-hook
	  (lambda ()
            (let ((buffer "*Completions*"))
              (and (get-buffer buffer)
		   (kill-buffer buffer)))))

;;;;; Interface
(use-package fill-column-indicator
  :config
  (global-display-fill-column-indicator-mode t) ; Show FCI
  (setq-default fill-column 90))                ; default FCI to 90

(context-menu-mode 1)                          ; right click menu
(electric-pair-mode 1)                         ; Enable to auto add closing brackets
(column-number-mode t)                         ; Enable to display current column number
(size-indication-mode t)                       ; Enable to display current file size
(delete-selection-mode 1)                      ; Enable overwriting selection when typing
(setq isearch-lazy-count t)                    ; Show the number of match when searching
(setq create-lockfiles nil)                    ; Disable .#lockfiles files
(setq make-backup-files nil)                   ; Disable backup~ files
(setq auto-save-default nil)                   ; Disable #autosave# files
(fset 'yes-or-no-p 'y-or-n-p)                  ; Type y/n instead of yes/no
(setq require-final-newline t)                 ; Add a new line at the end of files
(setq frame-resize-pixelwise t)
(setq select-enable-clipboard t)               ; Enable clipboard in emacs with C-shift-v
(setq-default indent-tabs-mode nil)            ; Use space instead of tabs for indentation
(global-display-line-numbers-mode t)           ; Enable to disaply line number
(setq-default bidi-display-reordering nil)     ; Disable bidirectional editing. Faster long line check
(setq-default frame-title-format '("%f [%m]")) ; Set window name as "filename [major-mode]"
;;;;; Long lines
(setq truncate-lines t)                       ; Hide the end of line longer than the interface
(setq-local auto-hscroll-mode 'current-line)  ; Enable scrolling on long lines
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode)) ; highlight current line
(add-to-list 'default-frame-alist '(buffer-predicate . buffer-file-name))

;;; Emacs custom variables:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tsdh-dark))
 '(doc-view-continuous t)
 '(doc-view-resolution 150)
 '(exec-path-from-shell-check-startup-files nil)
 '(package-selected-packages
   '(markdown-mode diredfl company json-mode use-package elpy flycheck editorconfig eaf exec-path-from-shell add-node-modules-path js2-mode))
 '(show-paren-mode t))

;; smooth scrolling
(setq scroll-margin 0
  scroll-conservatively 10000
  scroll-preserve-screen-position 1
  scroll-step 1)

;; delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; web browser
;; (use-package eaf
;;   :defer t
;;   :load-path "~/.emacs.d/emacs-application-framework"
;;   :custom
;;   (eaf-find-alternate-file-in-dired t)
;;   :config
;;   ; github token without rights to access github pages
;;   (setq eaf-grip-token "2e43bdcba5dc8049eedbe5cf65635f12ef955f10")
;;   (eaf-setq eaf-browser-download-path "~/Downloads")
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding))

;;; Key binding:
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key (kbd "C-<left>") 'backward-word)
(global-set-key (kbd "C-DEL") 'kill-word)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key [f1] 'kill-buffer)
(global-set-key [f2] 'delete-window)
(global-set-key [f3] 'split-window-vertically)
(global-set-key [f4] 'split-window-horizontally)
(global-set-key [f12] 'indent-buffer)
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "C-M-o") 'find-file-at-point)
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "C-c C-k") 'copy-line)
(global-set-key (kbd "C-c C-c") 'replace-string)
(global-set-key (kbd "C-c C-r") 'query-replace-regexp)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-d") 'duplicate-line)

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
 '(js2-function-call ((t (:inherit default :foreground "gray"))))
 '(js2-function-param ((t (:foreground "cyan"))))
 '(js2-object-property ((t (:inherit default :foreground "peru"))))
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

(provide 'init)

;;; init.el ends here
