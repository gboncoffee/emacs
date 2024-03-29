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

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;;
;; appearance (theme is set at the bottom to make sure everything is ok)
;;
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode 1)
(global-display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(set-frame-font "Cascadia Code 18")

(when window-system
  (setq-default display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (set-frame-size (selected-frame) 80 24))

(use-package rainbow-mode ;; highlight colors like magenta and #cafebb
  :config
  (add-hook 'text-mode-hook #'rainbow-mode)
  (add-hook 'prog-mode-hook #'rainbow-mode))

(setq frame-resize-pixelwise t)

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

(use-package magit
  :config
  (global-set-key (kbd "C-c g") #'magit))

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :config
  (define-key copilot-completion-map (kbd "M-/") 'copilot-accept-completion))

(electric-pair-mode)

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
;; ls command when Plan 9 is before the system tools in PATH.
;;
(let ((lspath (shell-command-to-string "which ls")))
  (if (not (or
	    (string= lspath "/usr/bin/ls")
	    (string= lspath "/bin/ls")))
      (setq insert-directory-program "/bin/ls")))

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
  (setq rust-format-on-save t)
  (local-set-key (kbd "C-c C-c") #'compile))

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
(add-hook 'tex-mode-hook (lambda ()
			   (auto-fill-mode)
			   (setq show-trailing-whitespace t)
			   (local-set-key (kbd "C-c C-c") #'compile)))
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; theme
(when window-system
  (use-package cybercafe-theme
    :config
    (setq cybercafe-light t)
    (load-theme 'cybercafe t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(magit cybercafe-theme caml clojure-mode web-mode nubox modus-themes editorconfig erlang haskell-mode lua-mode rust-mode rg multiple-cursors rainbow-mode go-mode use-package))
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
 '(fixed-pitch-serif ((t (:family "Go Mono")))))
