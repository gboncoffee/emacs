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
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

;; general configs
(fset 'yes-or-no-p 'y-or-n-p) ;; turns all yes/no to y/n

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
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (setq all-the-icons-scale-factor 0.8))
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
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)
(setq scroll-conservatively 1000)
(setq scroll-margin 20)
(blink-cursor-mode 0)
(global-visual-line-mode 1)
(global-prettify-symbols-mode 1)

;; doom modeline
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))


;; dashboard
(setq dashboard-center-content t)
(setq dashboard-banner-logo-title "Gb's Kawaii Lisp Operating System")
(setq dashboard-startup-banner '"/home/gb/.config/emacs/dash.jpg")
(setq dashboard-items '((recents . 3)
			(agenda . 3)))
(use-package dashboard
  :ensure t
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
(use-package counsel
  :ensure t)
(use-package flx
  :ensure t)
(use-package ivy
  :ensure t
  :config
  (setq ivy-initial-inputs-alist nil)
  :init
  (ivy-mode t)
  (setq ivy-re-builders-alist
    '((t . ivy--regex-fuzzy))))

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
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively t
        markdown-open-command "markdown"
        markdown-asymmetric-header t
        markdown-header-scaling t
        markdown-hide-urls t))
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
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))
;; yaml
(use-package yaml-mode
  :ensure t)

;;
;; applications
;;

;; magit
(use-package magit
  :ensure t)
;; disable evil in magit
(eval-after-load 'evil-core
  '(evil-set-initial-state 'magit-popup-mode 'emacs))

;; dired
(setq dired-listing-switches "-lah -v --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)
(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode t))

;; terminal integration
(setq terminal-here-linux-terminal-command 'alacritty)
(use-package terminal-here
  :ensure t)

;;
;; keybinds
;;

(evil-set-leader 'normal (kbd "SPC"))
(define-key minibuffer-local-map (kbd "ESC") 'abort-recursive-edit)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

(evil-define-key 'normal 'global
  ;; zooming like a true zoomer
  (kbd "C-=") 'text-scale-increase
  (kbd "C--") 'text-scale-decrease
  (kbd "<C-wheel-up>") 'text-scale-increase
  (kbd "<C-wheel-down>") 'text-scale-decrease
  (kbd "C-0") '(lambda () (interactive) (text-scale-adjust 0))
  ;; zen
  (kbd "<leader>z") 'darkroom-mode

  ;; running things
  (kbd "<leader>b") 'compile
  (kbd "<leader>/") 'rg
  (kbd "SPC SPC") 'counsel-M-x
  (kbd "!") 'shell-command

  ;; inserting things 😳
  (kbd "<leader>x") 'counsel-unicode-char

  ;; navigation
  (kbd "<leader>f") 'counsel-find-file
  (kbd "<leader>.") 'counsel-file-jump
  (kbd "<leader>a") 'counsel-linux-app
  (kbd "<leader>e") 'dired-jump

  ;;
  ;; applications keybinds
  ;;
  (kbd "<leader>g") 'magit
  (kbd "<leader>RET") 'terminal-here
  (kbd "<leader>ch") '(lambda () (interactive) (terminal-here-launch (list "ghci")))
  (kbd "<leader>cp") '(lambda () (interactive) (terminal-here-launch (list "python")))
  (kbd "<leader>cc") '(lambda () (interactive) (terminal-here-launch (list "julia")))
  (kbd "<leader>cj") '(lambda () (interactive) (terminal-here-launch (list "deno")))
  (kbd "<leader>ct") '(lambda () (interactive) (terminal-here-launch (list "htop")))
  (kbd "<leader>cm") '(lambda () (interactive) (terminal-here-launch (list "ncmpcpp")))
  (kbd "<leader>cl") '(lambda () (interactive) (terminal-here-launch (list "lua"))))

;; ivy
(define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word) 
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line) 
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line) 

;; dired
(evil-define-key 'normal dired-mode-map
  (kbd "t") 'terminal-here
  (kbd "!") 'shell-command
  (kbd "g") 'counsel-find-file
  (kbd "$") 'dired-do-shell-command
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file
  (kbd "v") 'dired-mark
  (kbd "V") 'dired-mark-files-containing-regexp
  (kbd "u") 'dired-unmark
  (kbd "c") 'dired-unmark-all-marks
  (kbd "U") 'dired-toggle-marks
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
   '(vertico use-package unicode-fonts rg pdf-tools markdown-mode magit lua-mode ligature julia-mode hl-todo haskell-mode evil-numbers evil-commentary evil-collection editorconfig doom-themes doom-modeline diredfl darkroom all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
