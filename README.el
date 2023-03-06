(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (evil-mode))

(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq user-full-name "Gabriel G. de Brito")
(setq user-mail-address "gabrielgbrito@icloud.com")
(setq fortune-file "/home/gb/.config/emacs/fortune")

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

;; basically plain evil-quit but with a confirmation if quitting client or emacs
;; so partially stolen from https://github.com/emacs-evil/evil
(evil-define-command evil-quit (&optional force)
  :repeat nil
  (interactive "<!>")
  (condition-case nil
      (delete-window)
    (error
     (if (and (boundp 'server-buffer-clients)
              (fboundp 'server-edit)
              (fboundp 'server-buffer-done)
              server-buffer-clients)
         (if force
             (server-buffer-done (current-buffer))
           (server-edit))
       (if (+doom-quit-fn) (condition-case nil
           (delete-frame)
         (error
          (condition-case nil
              (tab-bar-close-tab)
            (error
             (if force
                 (kill-emacs)
               (save-buffers-kill-emacs)))))))))))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-org-config))

(set-face-attribute 'default nil
  :font "CaskaydiaCove Nerd Font Mono-18"
  :weight 'normal)
(set-face-attribute 'variable-pitch nil
  :font "CaskaydiaCove Nerd Font-18"
  :weight 'normal)
(set-face-attribute 'fixed-pitch nil
  :font "CaskaydiaCove Nerd Font Mono-18"
  :weight 'normal)
(add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font Mono-18"))
(use-package all-the-icons
  :ensure t)

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

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

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(setq-default display-line-numbers-width 3)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(add-hook 'package-menu-mode-hook 'hl-line-mode)

(setq scroll-conservatively 1000)
(setq maximum-scroll-margin 0.5)
(setq scroll-margin 10)
(setq-default truncate-lines -1)
(setq truncate-partial-width-windows nil)

(add-hook 'emacs-lisp-mode-hook (lambda () (interactive) (prettify-symbols-mode t)))
(add-hook 'LaTeX-mode-hook (lambda () (interactive) (prettify-symbols-mode t)))

(setq doom-modeline-buffer-encoding nil)
(display-time-mode t)
(display-battery-mode t)
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package dashboard
  :ensure t
  :after all-the-icons
  :config
  (setq dashboard-center-content t)
  (setq dashboard-force-refresh t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Gb's Kawaii Lisp Operating System")
  (setq dashboard-startup-banner '"~/.config/emacs/dash.jpg")
  (setq dashboard-items '())
  (setq dashboard-footer-icon (all-the-icons-fileicon "emacs"
                                                     :face 'font-lock-keyword-face))
  (add-hook 'server-after-make-frame-hook 'dashboard-refresh-buffer)
  (dashboard-setup-startup-hook))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

(use-package darkroom
  :ensure t)

(use-package pdf-tools
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(use-package lice
  :ensure t
  :config
  (setq lice:default-license "mit"))

(setq projectile-project-search-path '("~/src/"))
(setq projectile-find-dir-includes-top-level t)
(setq projectile-switch-project-action 'counsel-find-file)
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))
(use-package counsel-projectile
  :ensure t
  :after counsel)

(use-package htmlize
  :ensure t)

(use-package smex
  :ensure t)
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
(use-package all-the-icons-ivy
  :ensure t
  :after all-the-icons
  :config
  (all-the-icons-ivy-setup))

(use-package rg
  :ensure t)

(setq compile-command "")
(add-hook 'prog-mode-hook 'hs-minor-mode)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively t
        markdown-open-command "markdown"
        markdown-asymmetric-header t
        markdown-header-scaling t
        markdown-hide-urls t))

(use-package haskell-mode
  :ensure t)

(use-package julia-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package yaml-mode
  :ensure t)

(setq c-default-style "k&r"
      c-basic-offset 4)

(add-to-list 'auto-mode-alist '("\\.rasi\\'" . css-mode))

(add-hook 'lisp-mode-hook 'electric-pair-local-mode t)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-local-mode t)

(use-package magit
  :ensure t
  :config
  (add-hook 'git-commit-post-finish-hook 'magit)
  (add-hook 'magit-mode-hook 'turn-off-evil-mode))

(setq dired-listing-switches "-lAhf")
(setq ranger-cleanup-on-disable t)
(setq ranger-cleanup-eagerly t)
;; (setq ranger-show-hidden nil)
(setq ranger-modify-header nil)
(setq ranger-preview-file t)
(setq ranger-show-literal nil)
(setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
(setq ranger-override-dired t)
(setq ranger-width-preview 0.55)
(setq ranger-max-preview-size 400)
(setq ranger-hide-cursor nil)
(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t))
(use-package diredfl
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'diredfl-mode))
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (setq all-the-icons-scale-factor 0.8))

(setq terminal-here-linux-terminal-command 'alacritty)
(use-package terminal-here
  :ensure t)

;; to Rin Shima camp with me 💖
(use-package fireplace
  :ensure t)

;; 🌈🐈
(use-package zone-nyan
  :ensure t)

;; quite ironic
(use-package xkcd
  :ensure t)

(use-package org-alert
  :ensure t
  :config
  (setq alert-default-style 'libnotify)
  (setq org-alert-interval 120
       org-alert-notify-cutoff 10
       org-alert-notify-after-event-cutoff 10)
  (setq org-alert-notification-title "Org Mode Alert")
  (org-alert-enable))
(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
(setq org-directory "~/doc/org")
(setq org-agenda-files "~/doc/org/agenda-files")
(setq org-hide-emphasis-markers t)
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

(defun org-edit-src-code nil)

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
(use-package evil-visualstar
  :ensure t
  :config
    (global-visualstar-mode))

(evil-set-leader 'normal (kbd "SPC"))
(define-key minibuffer-local-map (kbd "ESC") 'abort-recursive-edit)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-V") 'clipboard-yank)
(define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word) 
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line) 
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line) 
(define-key ivy-minibuffer-map (kbd "C-h") 'ivy-beginning-of-buffer) 
(define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done)

(evil-define-key 'normal 'global
  (kbd "C-=") 'text-scale-increase
  (kbd "C--") 'text-scale-decrease
  (kbd "<C-wheel-up>") 'text-scale-increase
  (kbd "<C-wheel-down>") 'text-scale-decrease
  (kbd "C-0") '(lambda () (interactive) (text-scale-adjust 0))
  (kbd "<leader>z") 'darkroom-mode

(kbd "<leader>b") 'project-compile
(kbd "<leader>cb") 'compile
(kbd "<leader>m") 'man
(kbd "<leader>/") 'rg
(kbd "SPC SPC") 'counsel-M-x
(kbd "!") 'shell-command
(kbd "C-c C-x") 'eval-defun

(kbd "<leader>x") 'counsel-unicode-char

(kbd "<leader>p") 'projectile-switch-project
(kbd "<leader>q") 'projectile-kill-buffers
(kbd "<leader>f") 'counsel-find-file
(kbd "<leader>.") 'counsel-projectile-find-file
(kbd "<leader>a") 'counsel-linux-app
(kbd "<leader>e") 'deer
(kbd "TAB") 'projectile-next-project-buffer
(kbd "<backtab>") 'projectile-previous-project-buffer
(kbd "<leader>ot") '(lambda () (interactive)
  		    (split-window-below)
  		    (windmove-down)
  		    (find-file "~/doc/org/todo.org"))
(kbd "<leader>om") '(lambda () (interactive)
  		    (split-window-below)
  		    (windmove-down)
  		    (find-file "~/doc/org/marker.org"))
(kbd "<leader>oo") '(lambda () (interactive)
  		    (split-window-below)
  		    (windmove-down)
  		    (find-file "~/doc/org/agenda.org"))

(kbd "<leader>g") 'magit
(kbd "<leader>RET") 'terminal-here
(kbd "<leader>oa") 'org-agenda
(kbd "<leader>ch") '(lambda () (interactive) (terminal-here-launch (list "ghci")))
(kbd "<leader>cp") '(lambda () (interactive) (terminal-here-launch (list "python")))
(kbd "<leader>cc") '(lambda () (interactive) (terminal-here-launch (list "julia")))
(kbd "<leader>cj") '(lambda () (interactive) (terminal-here-launch (list "deno")))
(kbd "<leader>cl") '(lambda () (interactive) (terminal-here-launch (list "lua")))
(kbd "<leader>cm") '(lambda () (interactive) (terminal-here-launch (list "ncmpcpp")))
(kbd "<leader>cs") '(lambda () (interactive) (terminal-here-launch (list "pulsemixer")))
(kbd "<leader>ct") '(lambda () (interactive) (terminal-here-launch (list "htop"))))

(evil-define-key 'visual 'global
  (kbd "C-x") 'eval-region)

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
(define-key ranger-mode-map (kbd "a") 'ranger-toggle-dotfiles)

(with-eval-after-load 'pdf-view
  (evil-define-key 'normal pdf-view-mode-map
    (kbd "q") 'kill-buffer-and-window
    (kbd "t") 'pdf-view-fit-page-to-window
    (kbd "s") 'pdf-view-fit-width-to-window
    (kbd "j") 'pdf-view-next-line-or-next-page
    (kbd "k") 'pdf-view-previous-line-or-previous-page))

(evil-define-key 'normal image-mode-map
  (kbd "q") 'kill-buffer-and-window)
(evil-define-key 'normal calendar-mode-map
  (kbd "q") 'kill-buffer-and-window)
(evil-define-key 'normal compilation-mode-map
  (kbd "q") 'kill-buffer-and-window)
