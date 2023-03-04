;; Gabriel's Emacs configs UwU
;;
;; They actually live in the README.org document!
;;
;; <3

(org-babel-load-file
  (expand-file-name
    "README.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" default))
 '(evil-undo-system 'undo-redo)
 '(package-selected-packages
   '(xkcd pdf-tools zone-nyan yaml-mode use-package unicode-fonts terminal-here smex rust-mode rg ranger powerline org-superstar org-noter markdown-mode magit lua-mode ligature lice julia-mode htmlize hl-todo haskell-mode flx fireplace expand-region evil-org evil-numbers evil-mc evil-commentary evil-collection editorconfig doom-themes doom-modeline diredfl dashboard darkroom counsel-projectile all-the-icons-ivy all-the-icons-dired))
 '(pdf-tools-enabled-modes
   '(pdf-history-minor-mode pdf-isearch-minor-mode pdf-links-minor-mode pdf-misc-minor-mode pdf-outline-minor-mode pdf-misc-size-indication-minor-mode pdf-misc-menu-bar-minor-mode pdf-annot-minor-mode pdf-sync-minor-mode pdf-misc-context-menu-minor-mode pdf-cache-prefetch-minor-mode pdf-occur-global-minor-mode pdf-virtual-global-minor-mode))
 '(warning-suppress-types '((use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :foreground "#ff5555" :weight bold :family "CaskaydiaCove Nerd Font"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7 :family "CaskaydiaCove Nerd Font"))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3 :family "CaskaydiaCove Nerd Font"))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.1 :family "CaskaydiaCove Nerd Font"))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.0 :family "CaskaydiaCove Nerd Font"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.5 :family "CaskaydiaCove Nerd Font"))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.3 :family "CaskaydiaCove Nerd Font"))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :height 1.1 :family "CaskaydiaCove Nerd Font"))))
 '(org-level-4 ((t (:inherit outline-4 :extend nil :height 1.0 :family "CaskaydiaCove Nerd Font"))))
 '(variable-pitch ((t (:weight normal :family "CaskaydiaCove Nerd Font")))))
