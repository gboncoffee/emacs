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

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

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

(use-package doom-themes
  :init
  (load-theme 'doom-gruvbox t))
(use-package rainbow-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package doom-modeline
  :init
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-mode t))
(use-package all-the-icons)
(use-package all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(set-face-attribute 'default nil :height 220)

(global-font-lock-mode)
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)

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
;; buffer management
(setq hated-buffers '("*Backtrace*" "*GNU Emacs*" "*Messages*" "*scratch*" "*Ibuffer*" "*Warnings*" "*Help*" "*Compile-Log*" "*Async-native-compile-log*" "*Packages*" "*rg*" "emacs"))
(defun string-in-list (str l)
  (if (null l)
      nil
    (if (string= (car l) str)
	1
      (string-in-list str (cdr l)))))

(defun next-buffer-with-hate-int ()
  (next-buffer)
  (if (string-in-list (buffer-name) hated-buffers)
      (next-buffer-with-hate-int)))

(defun next-buffer-with-hate ()
  "Like next-buffer, but ignores buffers from hated-buffers"
  (interactive)
  (if (not (string-in-list (buffer-name) hated-buffers))
      (next-buffer-with-hate-int)))

(defun previous-buffer-with-hate-int ()
  (previous-buffer)
  (if (string-in-list (buffer-name) hated-buffers)
      (previous-buffer-with-hate-int)))

(defun previous-buffer-with-hate ()
  "Like previous-buffer, but ignores buffers from hated-buffers"
  (interactive)
  (if (not (string-in-list (buffer-name) hated-buffers))
      (previous-buffer-with-hate-int)))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

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
  (counsel-mode)
  (global-set-key (kbd "C-s") #'swiper-isearch-thing-at-point))

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

;;
;; keybinds
;;
(global-set-key (kbd "C-x C-b")       #'ibuffer)
(global-set-key (kbd "C-x C-<right>") #'next-buffer-with-hate)
(global-set-key (kbd "C-x C-<left>")  #'previous-buffer-with-hate)
(global-set-key (kbd "C-x <right>")   #'next-buffer-with-hate)
(global-set-key (kbd "C-x <left>")    #'previous-buffer-with-hate)
(global-set-key (kbd "C-x C-x")       #'compile)
(global-set-key (kbd "C-x C-/")       #'rg)
(global-set-key (kbd "C-x C-f")       #'find-file)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(use-package move-text
  :init
  (move-text-default-bindings))
(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this))

;; org
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c o") #'(lambda () (interactive) (find-file "~/Documents/org/agenda.org")))

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
   '(all-the-icons-dired doom-modeline rg highlight-indent-guides doom-themes multiple-cursors rainbow-delimiters move-text rainbow-mode go-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "UbuntuMono Nerd Font Mono"))))
 '(line-number ((t (:inherit mode-line :slant normal :weight normal))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :extend nil :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :extend nil :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :extend nil :height 1.0))))
 '(variable-pitch ((t (:family "Ubuntu Nerd Font")))))
