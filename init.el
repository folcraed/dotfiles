;; Setting up my own Emacs. W.I.P

(setq inhibit-startup-message t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
         '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;==============================================
;;  Diminish for the rest
;;==============================================

(use-package diminish
  :ensure t)
;;==============================================
;;  Sanity settings
;;==============================================

(setq-default make-backup-files nil
	      backup-inhibited t
	      create-lockfiles nil
	      auto-save-default nil)

;;==============================================
;;  Misc packages
;;==============================================

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-word-1))

(use-package company
  :ensure t
  :diminish "C"
  :config
  (setq company-idle-delay 2)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t))

;;==============================================
;;  Spaceline
;;==============================================

(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'curve)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;;==============================================
;;  Winum settings
;;==============================================

(use-package winum
  :ensure t
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8))
  :init
  (setq winum-auto-setup-mode-line nil)
  (winum-mode))

;;==============================================
;;  Org-mode
;;==============================================

(use-package org
  :ensure t
  :pin org)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq-default org-file-apps
	      (quote
	       ((auto-mode . emacs)
		("\\.png\\'" . "xdg-open %s")
		("\\.jpg\\'" . "xdg-open %s")
		("\\.pdf\\'" . "xdg-open %s"))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
	 "* TODO %t %?")
	("c" "Changes" entry (file+headline "~/Dropbox/Notes/agenda.org" "Changes")
	 "* %?\n %t\n")
	("g" "Genealogy" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
	 "* TODO %t %?\n %a\n %?\n")))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("[%a %b %e %Y]" . "<%a %b %e %Y %H:%M>"))
(setq-default org-hide-emphasis-markers t)
(setq org-agenda-files (directory-files-recursively "~/Dropbox/Notes" "\.org$"))

;;==============================================
;;  Helm settings
;;==============================================

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
        ("C-x C-f" . helm-find-files)
        ("C-x b" . helm-buffers-list))
  :init
        (setq helm-M-x_fuzzy-match t
              helm-autoresize-mode t
              helm-autoresize-max-height 30
              helm-autoresize-min-height 20
              helm-split-window-inside-p t)
  :config
  (helm-mode 1))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
        ("C-x M-i" . helm-swoop-back-to-last-point)
        ("C-c M-i" . helm-multi-swoop)
        ("M-I" . helm-multi-swoop-all))
  :init
        (setq helm-swoop-split-with-multiple-windows t
              helm-swoop-split-direction 'split-window-vertically))

(use-package helm-rg
  :ensure t)

(use-package helm-flyspell
  :ensure t)

;;==============================================
;; Projectile
;;==============================================

(use-package projectile
  :ensure t
;;  :bind ("C-c p" . projectile-command-map)
  :diminish "P"
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;===============================================
;;  Magit
;;===============================================

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;===============================================
;; Some personal keybindings
;;===============================================

(define-prefix-command 'z-map)
(global-set-key (kbd "C-z") 'z-map)
(define-key z-map (kbd "c") 'org-capture)
(define-key z-map (kbd "r") 'helm-rg)
(define-key z-map (kbd "s") 'helm-flyspell-correct)
(define-key z-map (kbd "k") 'helm-show-kill-ring)
(define-key z-map (kbd "f") 'flyspell-buffer)
(define-key z-map (kbd "F") 'flyspell-mode)

;;==============================================
;; Custom settings
;;==============================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(beacon-color "#f2777a")
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("39464ed440476d616c5671ff4d9cfc2393846132390e0d80e611dfa0b4bd6983" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(display-line-numbers t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(global-visual-line-mode t)
 '(helm-ag-base-command "rg --no-heading")
 '(org-startup-folded (quote content))
 '(package-selected-packages
   (quote
    (magit diminish helm-rg helm-swoop atom-one-dark-theme avy company org helm-ag color-theme-sanityinc-tomorrow winum eyebrowse spaceline helm org-bullets which-key use-package)))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :foundry "CYEL" :family "Iosevka")))))
(put 'dired-find-alternate-file 'disabled nil)
