;; Emacs settings Ver 0.2
;; File or commit timestamp show when last updated.

(setq inhibit-startup-message t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq-default display-line-numbers t)
(set-default-font "Iosevka 12")
(put 'dired-find-alternate-file 'disabled nil)
(global-visual-line-mode t)
;; (display-line-numbers t)

;;==============================================
;;  Set up repositories
;;==============================================

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
  (setq company-idle-delay 1)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t))

;;===============================================
;;  Doom modeline
;;===============================================

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
(setq doom-modeline-height 18)
(setq doom-modeline-minor-modes 1)
(setq doom-modeline-major-mode-color-icon 1)
(setq doom-modeline-buffer-file-name-style 'relative-from-project)
(setq column-number-mode 1)

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
	 "* %t %?")
	("g" "Genealogy" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
	 "* TODO %t %?\n %a\n")))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("[%a %b %e %Y]" . "<%a %b %e %Y %H:%M>"))
(setq-default org-hide-emphasis-markers t)
(setq org-agenda-files (directory-files-recursively "~/Dropbox/Notes" "\.org$"))
(setq org-startup-folded nil)

;;==============================================
;;  Ivy, Counsel and friends
;;==============================================

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-S-p" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-o" . counsel-outline))
  :config
  (setq counsel-outline-display-style 'headline))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d%d ")
  (setq ivy-display-style 'fancy)
  (setq ivy-height 16
        ivy-fixed-height-minibuffer t)
  (setq ivy-initial-inputs-alist
      '((org-refile . "^")
        (org-agenda-refile . "^")
        (org-capture-refile . "^")
        (counsel-M-x . "")
        (counsel-describe-function . "^")
        (counsel-describe-variable . "^")
        (counsel-org-capture . "^")
        (Man-completion-table . "^")
        (woman . "^"))))

(use-package swiper
  :ensure t
  :bind (("M-i" . swiper)
         ("M-I" . swiper-all))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)))

;;==============================================
;;  Flyspell stuff
;;==============================================

(use-package flyspell-correct
  :ensure t)
  
(use-package flyspell-correct-ivy
  :ensure t
  :bind ("C-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;;==============================================
;; Projectile
;;==============================================

(use-package projectile
  :ensure t
  :diminish "P"
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))
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
(define-key z-map (kbd "r") 'counsel-rg)
(define-key z-map (kbd "s") 'flyspell-correct-at-point)
(define-key z-map (kbd "k") 'counsel-yank-po)
(define-key z-map (kbd "f") 'flyspell-buffer)
(define-key z-map (kbd "F") 'flyspell-mode)
(global-set-key (kbd "C-p") 'projectile-find-file)
(global-set-key (kbd "C-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "M-f") 'goto-line)


;;==============================================
;;  Sanity settings
;;==============================================

(setq-default make-backup-files nil
	      backup-inhibited t
	      create-lockfiles nil
	      auto-save-default nil)

(add-hook 'minibuffer-setup-hook
	  (lambda () (setq truncate-lines nil)))

(show-paren-mode 1)

;;==============================================
;; Custom settings
;;==============================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "f7ef6451d988d6e2fc86deea398eee02b3371703d88f265d31a011bd240dcf99" "f56393685517a0c58952a5fefdc45e29b52cc1798688992d112f7c299325a889" "39464ed440476d616c5671ff4d9cfc2393846132390e0d80e611dfa0b4bd6983" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (railscasts-reloaded-theme flyspell-correct-ivy flyspell-correct magit projectile diminish atom-one-dark-theme doom-modeline all-the-icons undo-tree avy company org color-theme-sanityinc-tomorrow winum eyebrowse ivy counsel swiper org-bullets which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
