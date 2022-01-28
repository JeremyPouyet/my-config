;;; early-init.el --- File loaded before graphical environment

;;; Commentary:

;;; Code:

;; Reduce GC during init. Increase it to 64 Mb after init
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(add-hook 'after-init-hook
	  `(lambda ()
	     (setq gc-cons-threshold 67108864 gc-cons-percentage 0.1)
	     (garbage-collect)
	     ) t)

(fringe-mode 0)                      ; Disable margins
(tool-bar-mode -1)                   ; Disable toolbar
(scroll-bar-mode -1)                 ; Disable scroll bar
(setq package-quickstart t)          ; Enable package cache
(setq inhibit-startup-screen t)      ; Disable startup screen
(setq inhibit-startup-buffer-menu t) ; Disable *Buffer list* when opening multiple files

(provide 'early-init)

;;; early-init.el ends here
