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
(use-package smyx-theme
  :init
  (load-theme 'smyx t)
  (set-face-foreground 'line-number-current-line "#f6dc69")
  (set-face-attribute 'line-number-current-line nil :weight 'bold)
  (set-face-foreground 'font-lock-function-name-face "#f7f7f7")
  (set-face-foreground 'font-lock-variable-name-face "#f7f7f7")
  (set-face-foreground 'font-lock-constant-face "#f7f7f7")
  (set-face-foreground 'font-lock-comment-face "#fab1ab")
  (set-face-foreground 'font-lock-preprocessor-face "#96d9f1"))
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

(set-face-attribute 'default nil :height 250) ;; font size

;; modeline
(column-number-mode t)
(size-indication-mode t)

(use-package rainbow-mode ;; highlight colors like pink and #cafebb
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
(electric-indent-mode)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
(setq isearch-wrap-pause 'no)

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
(defun lisps-hook ()
  (prettify-symbols-mode)
  (electric-pair-local-mode))
(add-hook 'emacs-lisp-mode-hook #'lisps-hook)
(add-hook 'common-lisp-mode-hook #'lisps-hook)
(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode)) ;; Emacs only reconizes .lisp as Common Lisp

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
   '(web-mode smyx-theme nubox modus-themes editorconfig dante tuareg lice auctex elixir-mode erlang haskell-mode julia-mode lua-mode magit markdown-mode pdf-tools rust-mode toml-mode xkcd yaml-mode rg multiple-cursors rainbow-mode go-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:foreground "#96d9f1"))))
 '(ido-first-match ((t (:background "#282828" :foreground "#f6dc69"))))
 '(ido-only-match ((t (:inherit ido-first-match))))
 '(ido-subdir ((t (:background "#282828" :foreground "#96d9f1"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :height 1.1)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
