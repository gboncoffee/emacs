;;
;; GB's Emacs UwU
;;
(setq user-mail-address "gabrielgbrito@icloud.com")

;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;
;; appearance (theme is set at the bottom to make sure everything is ok)
;;
(tool-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode 1)
(global-display-fill-column-indicator-mode)
(setq-default display-line-numbers-width 3)
(setq inhibit-splash-screen t)
(add-hook 'prog-mode-hook (lambda ()
			    (setq display-line-numbers 'relative)
			    (setq show-trailing-whitespace t)))

(set-frame-font "Monospace 24" nil t)

(use-package rainbow-mode ;; highlight colors like magenta and #cafebb
  :config
  (add-hook 'text-mode-hook #'rainbow-mode)
  (add-hook 'prog-mode-hook #'rainbow-mode))

;;
;; general
;;
(defalias 'yes-or-no-p 'y-or-n-p)  ;; disables yes-or-no-p and use just y-or-n-p
(setq-default fill-column 80)      ;; text width
(setq make-backup-files nil)       ;; disable backups
(setq compile-command "")          ;; no default compile command

;;
;; ivy, completion and friends
;;

(electric-indent-mode)
(setq ido-enable-flex-matching t)
(ido-mode t)
(setq isearch-wrap-pause 'no)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-file-name-partially
	try-complete-file-name
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

;;
;; programs, extensions, etc
;;
(use-package magit
  :config
  (defun custom-magit-open ()
    "Opens magit and kills other windows"
    (interactive)
    (magit)
    (delete-other-windows))
  (global-set-key (kbd "C-c g") #'custom-magit-open))
(use-package magit-todos
  :config
  (magit-todos-mode 1))
(use-package pdf-tools)

(use-package lice
  :config
  (setq-default lice:default-license "mit"))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; dired
(setq dired-listing-switches "-agho --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)

;;
;; keybinds
;;
(global-set-key (kbd "C-x C-b") #'ibuffer-other-window)
;; three compile keybinds because some modes override C-c C-c
(global-set-key (kbd "C-c C-c") #'compile)
(global-set-key (kbd "C-c C-b")   #'compile)
(global-set-key (kbd "C-c 5")   #'compile)
(global-set-key (kbd "C-c /")   #'rgrep)
(global-set-key (kbd "C-M-n")   #'scroll-up-line)
(global-set-key (kbd "C-M-p")   #'scroll-down-line)
(global-set-key (kbd "C-c a")   #'org-agenda)
(global-set-key (kbd "M-/")     #'hippie-expand)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))
(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this))

;;
;; filetypes
;;
(use-package erlang)
(use-package tuareg) ;; OCaml

;; Better Web
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

;; Lisp
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode)) ;; Emacs only reconizes .lisp as Common Lisp

;; Clojure
(use-package cider
  :config
  (setq cider-use-overlays nil)
  (setq cider-prompt-for-symbol t)
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (cider-mode)
	      (add-hook 'before-save-hook 'cider-format-buffer t t))))

;; Go
(use-package go-mode
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-c C-f") #'godoc-at-point)
	      (add-hook 'before-save-hook #'gofmt-before-save))))

;; Rust
(use-package rust-mode
  :config
  (setq rust-format-on-save t))

;; Markdown
(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook #'auto-fill-mode))

;; Haskell
(use-package haskell-mode)

;; Lua
(use-package lua-mode
  :config
  (setq lua-indent-level 4)
  (setq lua-documentation-function 'eww)
  (add-hook 'lua-mode-hook
	    (lambda ()
	      (indent-tabs-mode -1)
	      (local-set-key (kbd "C-c C-e") #'lua-send-region))))

;; LaTeX
(add-hook 'LaTeX-mode-hook #'auto-fill-mode)
(add-hook 'LaTeX-mode-hook (lambda () (setq display-line-numbers 'relative)))

;; C/C++
(setq c-default-style "linux")
(defun c-cpp-mode ()
  (local-set-key (kbd "C-c C-f") #'man)
  (local-set-key (kbd "C-c C-e") #'c-macro-expand)
  ;; override C-c C-c being used for comments
  (local-set-key (kbd "C-c C-c") #'compile))
(add-hook 'c-mode-hook #'c-cpp-mode)
(add-hook 'c++-mode-hook #'c-cpp-mode)

;; txt
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook (lambda ()
			    (setq display-line-numbers 'relative)
			    (setq show-trailing-whitespace t)))

;; theme
(use-package cybercafe-theme
  :config
  (setq cybercafe-soft t)
  (load-theme 'cybercafe t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(markdown-header-scaling t)
 '(package-selected-packages
   '(cybercafe-theme magit-todos package-lint cider web-mode nubox modus-themes editorconfig tuareg lice erlang haskell-mode lua-mode magit markdown-mode pdf-tools rust-mode rg multiple-cursors rainbow-mode go-mode use-package)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
