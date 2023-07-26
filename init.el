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
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(visual-line-mode 0)
(setq scroll-margin 10)
(setq maximum-scroll-margin 0.5)
(setq scroll-step 1)
(setq frame-resize-pixelwise t)
(setq truncate-partial-width-windows nil)
(setq-default show-trailing-whitespace t)
(setq-default truncate-lines t)
(global-display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook (lambda ()
			    (setq display-line-numbers-width 3)
			    (setq display-line-numbers 'relative)))

;; modeline
(column-number-mode t)
(size-indication-mode t)

(use-package solarized-theme
  :init
  (load-theme 'solarized-dark t))
(use-package rainbow-mode ;; highlight colors like pink and #cafebb
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(set-face-attribute 'default nil :height 220) ;; font size
(use-package mixed-pitch
  :init
  (add-hook 'text-mode-hook #'mixed-pitch-mode))

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
(use-package counsel
  :init
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode))

;;
;; programs, extensions, etc
;;
(use-package magit
  :init
  (global-set-key (kbd "C-x g")   #'magit))
(use-package xkcd)
(use-package pdf-tools)
(use-package rg
  :init
  (setq rg-command-line-flags
	'("--no-heading" "--with-filename" "--line-number" "--column" "--smart-case" "--hidden" "-g" "!.git/")))

;; dired
(setq dired-listing-switches "-agho --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)
(use-package diredfl
  :init
  (diredfl-global-mode t))
(use-package dired-atool)

;; org mode
(setq org-hide-emphasis-markers t)
(setq org-directory "~/Documents/org")
(setq org-agenda-files '("~/Documents/org/agenda.org"))
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)

;;
;; keybinds
;;
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x C-x") #'compile)
(global-set-key (kbd "C-x C-/") #'rg)
(global-set-key (kbd "C-x /")   #'rg)
(global-set-key (kbd "C-s")     #'swiper-isearch)
(global-set-key (kbd "C-x f")   #'find-file)
(global-set-key (kbd "C-h f")   #'describe-function)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))
(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this))

;; org
(global-set-key (kbd "C-c a")   #'org-agenda)
(global-set-key (kbd "C-c C-a") #'org-agenda)
(defun org-open-agenda-file ()
  "Open the first Org agenda file"
  (interactive)
  (find-file (car org-agenda-files)))
(global-set-key (kbd "C-c o")   #'org-open-agenda-file)
(global-set-key (kbd "C-c C-o") #'org-open-agenda-file)

;;
;; filetypes
;;
(use-package json-mode)
(use-package coffee-mode)
(use-package julia-mode)
(use-package tuareg) ;; OCaml
(use-package nix-mode)
(use-package racket-mode)
(use-package toml-mode)
(use-package yaml-mode)
(use-package erlang)
(use-package elixir-mode)
;; Lisp(s)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
(add-hook 'common-lisp-mode-hook #'prettify-symbols-mode)
(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode)) ;; Emacs only reconizes .lisp as Common Lisp
;; Go
(use-package go-mode
  :init
  (add-hook 'go-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-c C-i")     #'go-goto-imports)
	      (local-set-key (kbd "C-c i")       #'go-goto-imports))))
;; Rust
(use-package rust-mode
  :init
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-c C-m") #'rust-toggle-mutability)
	      (local-set-key (kbd "C-c m")   #'rust-toggle-mutability))))
;; Markdown
(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook #'auto-fill-mode))
;; Haskell
(use-package haskell-mode
  :init
  (setq haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-c C-d") #'haskell-describe)
	      (local-set-key (kbd "C-c C-i") #'haskell-navigate-imports)
	      (local-set-key (kbd "C-c C-z") #'haskell-hide-toggle)
	      (local-set-key (kbd "C-c C-m") #'haskell-hide-toggle-all)
	      (local-set-key (kbd "C-c d")   #'haskell-describe)
	      (local-set-key (kbd "C-c i")   #'haskell-navigate-imports)
	      (local-set-key (kbd "C-c z")   #'haskell-hide-toggle)
	      (local-set-key (kbd "C-c m")   #'haskell-hide-toggle-all)
	      (haskell-collapse-mode))))
;; Lua
(use-package lua-mode
  :init
  (setq lua-indent-level 4))
;; LaTeX
(add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)
(add-hook 'LaTeX-mode-hook #'auto-fill-mode)
(add-hook 'LaTeX-mode-hook #'display-line-numbers-mode)
(use-package tex
  :ensure auctex
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-view-program-selection t)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))
;; C/C++
(setq c-default-style "k&r"
      c-basic-offset 4)
;; txt
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "0c08a5c3c2a72e3ca806a29302ef942335292a80c2934c1123e8c732bb2ddd77" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" default))
 '(fancy-splash-image "~/.config/emacs/splash.png")
 '(fringe-mode '(0) nil (fringe))
 '(package-selected-packages
   '(solarized-theme mixed-pitch counsel auctex coffee-mode dired-atool diredfl elixir-mode erlang haskell-mode json-mode julia-mode lua-mode magit markdown-mode nix-mode pdf-tools racket-mode rust-mode toml-mode tuareg xkcd yaml-mode rg multiple-cursors rainbow-delimiters rainbow-mode go-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
