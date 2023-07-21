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
(setq scroll-margin 10)
(setq maximum-scroll-margin 0.5)
(setq scroll-step 1)
(setq frame-resize-pixelwise t)
(global-display-fill-column-indicator-mode)
(global-prettify-symbols-mode t)

(add-hook 'prog-mode-hook (lambda ()
			    (setq display-line-numbers-width 3)
			    (setq display-line-numbers 'relative)))

(use-package doom-themes
  :init
  (load-theme 'doom-monokai-pro t))
(use-package rainbow-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(set-face-attribute 'default nil :height 220)

(global-font-lock-mode)
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)

;;
;; general
;;
(setq ring-bell-function #'ignore) ;; disables bell
(defalias 'yes-or-no-p 'y-or-n-p)  ;; disables yes-or-no-p and use just y-or-n-p
(setq-default fill-column 80)      ;; text width
(setq make-backup-files nil)       ;; disable backups
(setq compile-command "")          ;; no default compile command
;; buffer management
(setq hated-buffers '("*Backtrace*" "*GNU Emacs*" "*Messages*" "*scratch*" "*Ibuffer*"))
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
  (interactive)
  (if (not (string-in-list (buffer-name) hated-buffers))
      (next-buffer-with-hate-int)))

(defun previous-buffer-with-hate-int ()
  (previous-buffer)
  (if (string-in-list (buffer-name) hated-buffers)
      (previous-buffer-with-hate-int)))

(defun previous-buffer-with-hate ()
  (interactive)
  (if (not (string-in-list (buffer-name) hated-buffers))
      (previous-buffer-with-hate-int)))

;;
;; ido, completion and friends
;;
(electric-pair-mode)
(electric-indent-mode)
(use-package ido-completing-read+
  :init
  (ido-mode t)
  (ido-everywhere t)
  (ido-ubiquitous-mode t))
(use-package smex
  :init
  (smex-initialize))
(use-package ido-vertical-mode
  :init
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;;
;; programs, extensions, etc
;;
(use-package magit
  :init
  (global-set-key (kbd "C-x g")   #'magit)
  (global-set-key (kbd "C-x C-g") #'magit))
(use-package xkcd)
(use-package gnugo)

;; org mode
(setq org-hide-emphasis-markers t)
(setq org-directory "~/Documents/org")
(setq org-agenda-files '("~/Documents/org/agenda.org"))
(add-hook 'org-mode-hook #'variable-pitch-mode)

;;
;; keybinds
;;
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x C-<right>") #'next-buffer-with-hate)
(global-set-key (kbd "C-x C-<left>") #'previous-buffer-with-hate)
(global-set-key (kbd "C-x C-x") #'compile)
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
(use-package go-mode)
(use-package haskell-mode)
(use-package json-mode)
(use-package coffee-mode)
(use-package rust-mode)
(use-package lua-mode)
(use-package julia-mode)
(use-package tuareg) ;; OCaml
(use-package markdown-mode)
;; c
(setq c-default-style "k&r"
      c-basic-offset 4)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes '(default))
 '(package-selected-packages
   '(doom-themes multiple-cursors rainbow-delimiters move-text rainbow-mode ido-completing-read+ go-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "UbuntuMono Nerd Font Mono"))))
 '(line-number ((t (:inherit default :foreground "dim gray" :slant italic :weight normal))))
 '(variable-pitch ((t (:family "Ubuntu Nerd Font")))))
