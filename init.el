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
;; appearance
;;
(use-package cybercafe-theme
  :config
  (load-theme 'cybercafe t))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode 1)
(setq scroll-step 1)
(setq frame-resize-pixelwise t)
(global-display-fill-column-indicator-mode)
(setq-default display-line-numbers-width 3)
(setq inhibit-splash-screen t)
(add-hook 'prog-mode-hook (lambda ()
			    (setq display-line-numbers 'relative)
			    (setq show-trailing-whitespace t)))

(set-face-attribute 'default nil :height 220) ;; font size

;; modeline
(column-number-mode t)
(size-indication-mode t)

(use-package rainbow-mode ;; highlight colors like magenta and #cafebb
  :config
  (add-hook 'text-mode-hook #'rainbow-mode)
  (add-hook 'prog-mode-hook #'rainbow-mode))

;;
;; general
;;
(setq ring-bell-function #'ignore) ;; disables bell
(defalias 'yes-or-no-p 'y-or-n-p)  ;; disables yes-or-no-p and use just y-or-n-p
(setq-default fill-column 80)      ;; text width
(setq make-backup-files nil)       ;; disable backups
(setq compile-command "")          ;; no default compile command

;;
;; ivy, completion and friends
;;
(electric-pair-mode)
(electric-indent-mode)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
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
  (global-set-key (kbd "C-c g")   #'magit))
(use-package xkcd)
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

;; org mode
(setq org-hide-emphasis-markers t)
(setq org-directory "~/Documents/org")
(setq org-agenda-files '("~/Documents/org/agenda.org"))
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook (lambda () (setq display-line-numbers 'relative)))

;;
;; keybinds
;;
(global-set-key (kbd "C-x C-b") #'ibuffer-other-window)
;; two compile keybinds: C-c 5 will be used as a fallback for modes that
;; I want to overwrite C-c C-c
(global-set-key (kbd "C-c C-c") #'compile)
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

;; org
(defun org-open-agenda-file ()
  "Open the first Org agenda file"
  (interactive)
  (find-file (car org-agenda-files)))
(global-set-key (kbd "C-c o")   #'org-open-agenda-file)

;;
;; filetypes
;;
(use-package julia-mode)
(use-package toml-mode)
(use-package yaml-mode)
(use-package erlang)
(use-package elixir-mode)
(use-package tuareg) ;; OCaml
(add-hook 'python-mode-hook #'prettify-symbols-mode)

;; Better Web
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

;; Lisp(s)
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (prettify-symbols-mode)))
(add-hook 'lisp-mode-hook (lambda ()
				  (prettify-symbols-mode)))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode)) ;; Emacs only reconizes .lisp as Common Lisp

;; Clojure (yeah deserves it's own section)
(use-package cider
  :config
  (setq cider-use-overlays nil)
  (setq cider-prompt-for-symbol t)
  (add-hook 'clojure-mode-hook (lambda ()
				 (cider-mode)
				 (prettify-symbols-mode)
				 (add-hook 'before-save-hook 'cider-format-buffer t t))))

;; Go
(use-package go-mode
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-c C-i") #'go-goto-imports)
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
(use-package dante
  :after haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'dante-mode)
  ;; fix eldoc
  (add-hook 'haskell-mode-hook
	    (lambda ()
	      (setq eldoc-documentation-strategy #'eldoc-documentation-default))))

;; Lua
(use-package lua-mode
  :config
  (setq lua-indent-level 4)
  (setq lua-documentation-function 'eww)
  (add-hook 'lua-mode-hook
	    (lambda ()
	      (indent-tabs-mode -1)
	      (local-set-key (kbd "C-c C-c") #'lua-send-buffer)
	      (local-set-key (kbd "C-c C-e") #'lua-send-region))))

;; LaTeX
(add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)
(add-hook 'LaTeX-mode-hook #'auto-fill-mode)
(add-hook 'LaTeX-mode-hook (lambda () (setq display-line-numbers 'relative)))
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-view-program-selection t)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(markdown-header-scaling t)
 '(package-selected-packages
   '(cybercafe-theme package-lint cider web-mode nubox modus-themes editorconfig dante tuareg lice auctex elixir-mode erlang haskell-mode julia-mode lua-mode magit markdown-mode pdf-tools rust-mode toml-mode xkcd yaml-mode rg multiple-cursors rainbow-mode go-mode use-package)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
