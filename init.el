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
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

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
(setq isearch-wrap-pause 'no)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;
;; keybinds
;;
(global-set-key (kbd "C-x C-b") #'ibuffer-other-window)
(global-set-key (kbd "C-c C-c") #'compile)
(global-set-key (kbd "C-c /")   #'rgrep)
(global-set-key (kbd "C-M-n")   #'scroll-up-line)
(global-set-key (kbd "C-M-p")   #'scroll-down-line)
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
(use-package caml)

;; Better Web
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

;; Lisp(s)
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(use-package clojure-mode)

;; Go
(use-package go-mode
  :config
  (add-hook
   'go-mode-hook (lambda () (add-hook 'before-save-hook #'gofmt-before-save))))

;; Rust
(use-package rust-mode
  :config
  (setq rust-format-on-save t))

;; Haskell
(use-package haskell-mode)

;; Lua
(use-package lua-mode
  :config
  (setq lua-indent-level 4)
  (add-hook 'lua-mode-hook (lambda () (indent-tabs-mode -1))))

;; C/C++
(setq c-default-style "linux")
(defun c-cpp-mode ()
  ;; override C-c C-c being used for comments
  (local-set-key (kbd "C-c C-c") #'compile))
(add-hook 'c-mode-hook #'c-cpp-mode)
(add-hook 'c++-mode-hook #'c-cpp-mode)

;; Markdown
(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (auto-fill-mode)
	      (setq show-trailing-whitespace t)
	      (local-set-key (kbd "C-c C-c") #'compile))))

;; LaTeX and txt
(add-hook 'LaTeX-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))

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
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(indicate-empty-lines t)
 '(package-selected-packages
   '(caml clojure-mode cybercafe-theme web-mode nubox modus-themes editorconfig erlang haskell-mode lua-mode rust-mode rg multiple-cursors rainbow-mode go-mode use-package))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
