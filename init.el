;; Gabriel's Emacs configs UwU
;;
;; <3
;;
;; MIT licensed btw. Sorry Stallman!!

;;
;; package management
;;
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;;
;; evil mode hihi I'M SO BAD
;;
(setq evil-want-C-u-scroll t)
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (evil-mode))
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))
(use-package evil-numbers
  :ensure t
  :after evil)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

;;
;; appearance
;;

;; theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-org-config))

;; fonts
(set-face-attribute 'default nil
  :font "CaskaydiaCove Nerd Font Mono-18"
  :weight 'normal)
(set-face-attribute 'variable-pitch nil
  :font "CaskaydiaCove Nerd Font-16"
  :weight 'normal)
(set-face-attribute 'fixed-pitch nil
  :font "CaskaydiaCove Nerd Font Mono-18"
  :weight 'normal)
;; for emacsclient
(add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font Mono-18"))
;; icons for free
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-ivy
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
(setq all-the-icons-scale-factor 0.8)
;; 🙂 
(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))
;; ligatures
(use-package ligature
  :ensure t
  :load-path "path-to-ligature-repo"
  :config
  (ligature-set-ligatures 't '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://" "www"))
  (global-ligature-mode t))

;; disable GUI stuff (why the heck all this stuff is default?)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; line numbers, scrolloff, etc
(setq-default display-line-numbers-width 3)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq scroll-margin 5)
(setq scroll-conservatively 1000)
(blink-cursor-mode 0)
(global-visual-line-mode 1)

;; doom modeline
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))


;; dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-banner-logo-title "Gb's Kawaii Lisp Operating System")
  (setq dashboard-startup-banner "/home/gb/.config/emacs/dash.jpg")
  (setq dashboard-items '())
  :config
  (dashboard-setup-startup-hook))
;; needed so the client starts in the dashboard and not in the stupid scratch
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;; I dont know what TODO
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

;; zen mode
(use-package darkroom
  :ensure t)

;;
;; tools
;;

;; pdf
(use-package pdf-tools
  :ensure t)

;;
;; completion/fuzzy/etc
;;
(setq ivy-initial-inputs-alist nil)
(use-package consult
  :ensure t)
(use-package counsel
  :ensure t)
(use-package flx
  :ensure t)
(use-package ivy
  :ensure t
  :init
  (ivy-mode t))
(setq ivy-re-builders-alist
  '((t . ivy--regex-fuzzy)))

(use-package smartparens
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

;;
;; femboy programmer stuff
;;

(setq compile-command "")
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))
;; ripgrep
(use-package rg
  :ensure t)

;; Markdown V
(use-package markdown-mode
  :ensure t)
;; Haskell λ
(use-package haskell-mode
  :ensure t)
;; JaVaScRiPt
(use-package nodejs-repl
  :ensure t)
;; Julia <3
(use-package julia-mode
  :ensure t)
;; Lua UwU <3 <3 💙
(use-package lua-mode
  :ensure t)
;; Rust, btw
(setq rust-format-on-save t)
(use-package rust-mode
  :ensure t)

;;
;; applications
;;

;; magit
(use-package magit
  :ensure t)
;; dired
(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode t))
(setq dired-listing-switches "-lah -v --group-directories-first")

;;
;; keybinds
;;

(evil-set-leader 'normal (kbd "SPC"))
(define-key minibuffer-local-map (kbd "ESC") 'abort-recursive-edit)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

;; zooming like a true zoomer
(define-key evil-normal-state-map (kbd "C-=") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "C--") 'text-scale-decrease)
(define-key evil-normal-state-map (kbd "<C-wheel-up>") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "<C-wheel-down>") 'text-scale-decrease)
(define-key evil-normal-state-map (kbd "C-0") '(lambda () (interactive) (text-scale-adjust 0)))
;; zen
(define-key evil-normal-state-map (kbd "<leader>z") 'darkroom-mode)

;; running things
(define-key evil-normal-state-map (kbd "<leader>b") 'compile)
(define-key evil-normal-state-map (kbd "<leader>/") 'rg)
(define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-M-x)
(define-key evil-normal-state-map (kbd "!") 'shell-command)

;; inserting things 😳
(define-key evil-normal-state-map (kbd "<leader>x") 'counsel-unicode-char)

;; navigation
(define-key evil-normal-state-map (kbd "<leader>f") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "<leader>.") 'counsel-file-jump)
(define-key evil-normal-state-map (kbd "<leader>a") 'counsel-linux-app)

;; ivy
(define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)

;;
;; applications keybinds
;;
(define-key evil-normal-state-map (kbd "<leader>g") 'magit)
(define-key evil-normal-state-map (kbd "<leader>ch") 'run-haskell)
(define-key evil-normal-state-map (kbd "<leader>cp") 'run-python)
(define-key evil-normal-state-map (kbd "<leader>cl") 'run-lua)

;; dired
(evil-define-key 'normal dired-mode-map
  (kbd "!") 'shell-command
  (kbd "$") 'dired-do-shell-command
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file
  (kbd "v") 'dired-mark
  (kbd "V") 'dired-mark-files-containing-regexp
  (kbd "u") 'dired-unmark-backward
  (kbd "c") 'dired-unmark-all-marks
  (kbd "c") 'dired-toggle-marks
  (kbd "<backspace>") 'dired-do-delete
  (kbd "M") 'dired-create-directory
  (kbd "r") 'dired-do-rename
  (kbd "y") 'dired-do-copy
  (kbd "Y") 'dired-copy-filename-as-kill)

;;
;; custom
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vertico use-package unicode-fonts rg pdf-tools nodejs-repl markdown-mode magit lua-mode ligature julia-mode hl-todo haskell-mode evil-numbers evil-commentary evil-collection editorconfig doom-themes doom-modeline diredfl dashboard darkroom all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
