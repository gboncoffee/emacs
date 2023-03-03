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
(use-package evil-mc
  :ensure t
  :after evil
  :config
  (global-evil-mc-mode))

;; general configs
(fset 'yes-or-no-p 'y-or-n-p) ;; turns all yes/no to y/n
(setq make-backup-files nil)
(setq user-full-name "Gabriel G. de Brito")
(setq user-mail-address "gabrielgbrito@icloud.com")
(setq org-directory "~/doc/org")
(setq org-agenda-files "~/doc/org/agenda-files")

;; stolen from doom emacs: https://github.com/doomemacs/doomemacs
(defvar +doom-quit-messages
  '(;; from Doom 1
    "Please don't leave, there's more demons to toast!"
    "Let's beat it -- This is turning into a bloodbath!"
    "I wouldn't leave if I were you. DOS is much worse."
    "Don't leave yet -- There's a demon around that corner!"
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    ;; from Portal
    "Thank you for participating in this Aperture Science computer-aided enrichment activity."
    "You can't fire me, I quit!"
    "I don't know what you think you are doing, but I don't like it. I want you to stop."
    "This isn't brave. It's murder. What did I ever do to you?"
    "I'm the man who's going to burn your house down! With the lemons!"
    "Okay, look. We've both said a lot of things you're going to regret..."
    ;; Custom
    "(setq nothing t everything 'permitted)"
    "Emacs will remember that."
    "Emacs, Emacs never changes."
    "Hey! Hey, M-x listen!"
    "It's not like I'll miss you or anything, b-baka!"
    "Wake up, Mr. Stallman. Wake up and smell the ashes."
    "You are *not* prepared!"
    "Please don't go. The drones need you. They look up to you.")
  "A list of quit messages, picked randomly by `+doom-quit'. Taken from
http://doom.wikia.com/wiki/Quit_messages and elsewhere.")
(defun +doom-quit-fn (&rest _)
  (y-or-n-p
   (format "%s %s"
           (propertize (nth (random (length +doom-quit-messages))
                            +doom-quit-messages)
                       'face '(italic default))
           "Really quit Emacs?")))
(setq confirm-kill-emacs '+doom-quit-fn)

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
  :font "CaskaydiaCove Nerd Font-18"
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
(blink-cursor-mode 0)
;; line numbers
(setq-default display-line-numbers-width 3)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
;; scrolling
(setq scroll-conservatively 1000)
(setq maximum-scroll-margin 0.5)
(setq scroll-margin 10)
;; visual line/hl line
(global-visual-line-mode t)
(global-hl-line-mode t)
;; prettify symbols and concealling
(add-hook 'emacs-lisp-mode-hook (lambda () (interactive) (prettify-symbols-mode t)))
(add-hook 'LaTeX-mode-hook (lambda () (interactive) (prettify-symbols-mode t)))
(setq org-hide-emphasis-markers t)

;; modeline
(setq doom-modeline-buffer-encoding nil)
(display-time-mode t)
(display-battery-mode t)
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

;; dashboard
(setq dashboard-center-content t)
(setq dashboard-banner-logo-title "Gb's Kawaii Lisp Operating System")
(setq dashboard-startup-banner '"~/.config/emacs/dash.jpg")
(setq dashboard-items '())
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
(use-package smex
  :ensure t)
(use-package counsel
  :ensure t)
(use-package counsel-projectile
  :ensure t
  :after counsel)
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
(setq dired-listing-switches "-lAhf --ignore-backups")
(setq ranger-cleanup-on-disable t)
(setq ranger-cleanup-eagerly t)
(setq ranger-show-hidden t)
(setq ranger-modify-header nil)
(setq ranger-hide-cursor t)
(setq ranger-preview-file t)
(setq ranger-show-literal nil)
(setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
(setq ranger-override-dired t)
(setq ranger-width-preview 0.55)
(setq ranger-max-preview-size 400)
(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t))
(use-package diredfl
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'diredfl-mode))

;; terminal integration
(setq terminal-here-linux-terminal-command 'alacritty)
(use-package terminal-here
  :ensure t)

;; project management
(setq projectile-project-search-path '("~/src/"))
(setq projectile-find-dir-includes-top-level t)
(setq projectile-switch-project-action #'projectile-find-dir)
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

;; multimedia
(use-package emms
  :ensure t)

;; to Rin Shima camp with me 💖
(use-package fireplace
  :ensure t)

;; 🌈🐈
(use-package zone-nyan
  :ensure t)

;; ILoveTheWeb
(use-package htmlize
  :ensure t)

;; org mode
(use-package evil-org
  :ensure t)
(use-package org-noter
  :ensure t)
(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; patent office
(use-package lice
  :ensure t
  :config
  (setq lice:default-license "mit"))

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
  (kbd "<leader>m") 'man
  (kbd "<leader>/") 'rg
  (kbd "SPC SPC") 'counsel-M-x
  (kbd "!") 'shell-command

  ;; inserting things 😳
  (kbd "<leader>x") 'counsel-unicode-char

  ;; navigation
  (kbd "<leader>p") 'projectile-switch-project
  (kbd "<leader>q") 'projectile-kill-buffers
  (kbd "<leader>f") 'counsel-find-file
  (kbd "<leader>.") 'counsel-projectile-find-file
  (kbd "<leader>a") 'counsel-linux-app
  (kbd "<leader>e") 'deer
  (kbd "<leader>TAB") 'projectile-next-project-buffer
  (kbd "<leader><backtab>") 'projectile-previous-project-buffer

  ;; cursors (normal mode)
  (kbd ",") 'evil-mc-undo-all-cursors
  (kbd "S") 'evil-mc-pause-cursors
  (kbd "Q") 'evil-mc-resume-cursors
  (kbd "s") 'evil-mc-make-cursor-here
  (kbd ";") 'evil-mc-undo-last-added-cursor

  ;; editor
  (kbd "U") 'evil-redo

  ;; applications
  (kbd "<leader>g") 'magit
  (kbd "<leader>RET") 'terminal-here
  (kbd "<leader>oa") 'org-agenda
  (kbd "<leader>ch") '(lambda () (interactive) (terminal-here-launch (list "ghci")))
  (kbd "<leader>cp") '(lambda () (interactive) (terminal-here-launch (list "python")))
  (kbd "<leader>cc") '(lambda () (interactive) (terminal-here-launch (list "julia")))
  (kbd "<leader>cj") '(lambda () (interactive) (terminal-here-launch (list "deno")))
  (kbd "<leader>ct") '(lambda () (interactive) (terminal-here-launch (list "htop")))
  (kbd "<leader>cm") '(lambda () (interactive) (terminal-here-launch (list "ncmpcpp")))
  (kbd "<leader>cl") '(lambda () (interactive) (terminal-here-launch (list "lua"))))

;; cursors (visual mode)
(evil-define-key 'visual 'global
  (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end
  (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)

;; ivy
(define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word) 
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line) 
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line) 
(define-key ivy-minibuffer-map (kbd "C-h") 'ivy-beginning-of-buffer) 
(define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done) 

;; dired/ranger/deer
(define-key ranger-mode-map (kbd "w") 'terminal-here)
(define-key ranger-mode-map (kbd "!") 'shell-command)
(define-key ranger-mode-map (kbd "f") 'counsel-find-file)
(define-key ranger-mode-map (kbd "R") 'ranger-refresh)
(define-key ranger-mode-map (kbd "$") 'dired-do-shell-command)
(define-key ranger-mode-map (kbd "h") 'ranger-up-directory)
(define-key ranger-mode-map (kbd "l") 'ranger-find-file)
(define-key ranger-mode-map (kbd "RET") 'ranger-find-file)
(define-key ranger-mode-map (kbd "M-RET") 'ranger-open-file-horizontally)
(define-key ranger-mode-map (kbd "SPC") 'ranger-toggle-mark)
(define-key ranger-mode-map (kbd "c") 'dired-unmark-all-marks)
(define-key ranger-mode-map (kbd "<backspace>") 'dired-do-delete)
(define-key ranger-mode-map (kbd "M") 'dired-create-directory)
(define-key ranger-mode-map (kbd "r") 'dired-do-rename)
(define-key ranger-mode-map (kbd "y") 'ranger-copy)
(define-key ranger-mode-map (kbd "d") 'ranger-cut)
(define-key ranger-mode-map (kbd "p") 'ranger-paste)
(define-key ranger-mode-map (kbd "Y") 'ranger-copy-filename)

;; I don't like image buffers
;; (add-hook 'image-mode-hook 'turn-off-evil-mode)
(evil-define-key 'normal image-mode-map
  (kbd "q") 'kill-buffer-and-window)

;;
;; custom
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" default))
 '(evil-undo-system 'undo-redo)
 '(package-selected-packages
   '(diredfl lice org-superstar org-noter evil-org htmlize zone-nyan fireplace ranger awesome-tab emms counsel-projectile projectile smex vertico use-package unicode-fonts rg pdf-tools markdown-mode magit lua-mode ligature julia-mode hl-todo haskell-mode evil-numbers evil-commentary evil-collection editorconfig doom-themes doom-modeline darkroom all-the-icons-dired))
 '(warning-suppress-types '((use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2))))
 '(variable-pitch ((t (:weight normal :family "CaskaydiaCove Nerd Font")))))
