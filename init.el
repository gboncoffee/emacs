;;
;; GB's Emacs UwU
;;
(setq user-mail-address "gabrielgbrito@icloud.com")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(global-display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(when window-system
  (set-frame-font "Ubuntu Mono 16")
  (setq-default display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

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

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dark+ t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-themes editorconfig magit multiple-cursors rainbow-mode use-package gnu-elpa-keyring-update cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
