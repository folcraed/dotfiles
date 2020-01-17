;; -*- lexical-binding: t -*-
;; My Emacs settings Ver 0.9
;; File or commit timestamp show when last updated.

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
;; (menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(prefer-coding-system 'utf-8)
(set-default-font "Hack 11")
(put 'dired-find-alternate-file 'disabled nil)
(global-visual-line-mode 1)
;; This is suppose to fix ??? displaying instead
;; of line numbers in modeline
(setq line-number-display-limit-width 2000000)

;;==============================================
;;  Set up repositories
;;==============================================
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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
;;  Minion for the rest
;;==============================================
(use-package minions
  :ensure t)

;;==============================================
;;  Misc packages
;;==============================================

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package perspective
  :ensure t)
(persp-mode)
(use-package persp-projectile
  :ensure t)
(require 'persp-projectile)

(use-package avy
  :ensure t
  :config
  (setq avy-case-fold-search nil)
  :bind ("M-j" . avy-goto-char-timer))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 1
	company-minimum-prefix-length 3)
  (global-company-mode t))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
	company-box-max-candidates 1000
	company-box-doc-enable nil))

(use-package iedit
  :ensure t)
(require 'iedit)

(use-package expand-region
  :ensure t
  :config 
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package rainbow-mode
  :ensure t)

;;==============================================
;;  Dired enhancements
;;==============================================

(use-package dired-narrow
  :ensure t)

(use-package peep-dired
  :ensure t
  :defer t
  :bind (:map dired-mode-map
              ("P" . peep-dired))
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-ignored-extensions '("mkv" "webm" "mp4" "mp3" "ogg" "iso")))

(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map))

(use-package all-the-icons-dired
  :ensure t)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq delete-by-moving-to-trash t
      dired-listing-switches "-ahlv --group-directories-first"
      wdired-create-parent-directories t
      dired-dwim-target t)

;;===============================================
;;  Multiple Cursors
;;===============================================
(use-package multiple-cursors
  :ensure t)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)

;;===============================================
;;  Doom modeline & theme
;;===============================================

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 15
	doom-modeline-minor-modes t
	doom-modeline-major-mode-color-icon t
	doom-modeline-buffer-modification-icon t
	doom-modeline-buffer-file-name-style 'relative-from-project
	column-number-mode 1))
(minions-mode 1)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t))
;; (doom-themes-org-config)

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
  :pin org
  :config
  (setq-default org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (setq org-startup-folded nil
	org-startup-indented t
	org-support-shift-select t
	org-ellipsis " ➥"
	org-tags-column 0))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("●" "○" "◉")))

(setq-default org-file-apps
	      (quote
	       ((auto-mode . emacs)
		("\\.png\\'" . "sxiv %s")
		("\\.jpg\\'" . "sxiv %s")
		("\\.pdf\\'" . "zathura %s"))))

(setq org-directory "~/Dropbox/Notes")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
	 "* TODO %t %?")
	("c" "Changes" entry (file+headline "~/Dropbox/Notes/changes.org" "Changes")
	 "* %t %?")
	("s" "Software" entry (file+headline "~/Dropbox/Notes/Software.org" "Refile")
	 "* %?")))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("[%a %b %e %Y]" . "<%a %b %e %Y %H:%M>")
      org-agenda-files (quote ("~/Dropbox/Notes/agenda.org"))
      org-goto-interface 'outline-path-completion
      org-outline-path-complete-in-steps nil)

;;==============================================
;;  Helm and friends
;;==============================================

(use-package helm
  :ensure t
  :init (setq helm-M-x_fuzzy-match 1
              helm-autoresize-mode 0
              helm-display-buffer-default-height 18
              helm-split-window-inside-p 1
              helm-descbinds-window-style 'same-window)
  :config
  (helm-mode 1))
(require 'helm-config)

(use-package helm-swoop
  :ensure t
  :init (setq helm-swoop-split-with-multiple-windows t
              helm-swoop-split=direction 'split-window-vertically))
(require 'helm-swoop)

(use-package helm-rg
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package helm-org
  :ensure t)

;;==============================================
;;  Flyspell stuff
;;==============================================

(use-package flyspell-correct
  :ensure t)

(use-package flyspell-correct-helm
  :ensure t
  :init
  (setq flyspell-correct-interface #'flyspell-correct-helm))

(setq ispell-program-name "aspell")
(global-set-key (kbd "<f8>") 'flyspell-mode)
(global-set-key (kbd "C-<f8>") 'flyspell-buffer)
(global-set-key (kbd "M-<f8>") 'flyspell-correct-at-point)
;;==============================================
;; Projectile
;;==============================================

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm))
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

;;===============================================
;;  Magit
;;===============================================

(use-package magit
  :ensure t)

;;===============================================
;;  Lua mode for Awesome editing
;;===============================================
(use-package lua-mode
  :ensure t
  :config
  (setq lua-indent-level 4))

;;===============================================
;; Some personal keybindings
;;===============================================

(define-prefix-command 'd-map)
(global-set-key (kbd "C-d") 'd-map)
(define-key d-map (kbd "a") 'org-agenda)
(define-key d-map (kbd "c") 'org-capture)
(define-key d-map (kbd "n") 'dired-narrow)
(define-key d-map (kbd "k") 'helm-show-kill-ring)
(define-key d-map (kbd "l") 'org-store-link)
(define-key d-map (kbd "r") 'helm-resume)
(define-key d-map (kbd "t") 'org-time-stamp)
(define-key d-map (kbd "x") 'kill-buffer-and-window)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-s w") 'helm-swoop)
(global-set-key (kbd "M-s g") 'helm-rg)
(global-set-key (kbd "M-s s") 'helm-swoop-without-pre-input)
(global-set-key (kbd "M-s o") 'helm-multi-swoop-org)
(global-set-key (kbd "C-p") 'projectile-find-file)
(global-set-key (kbd "C-b") 'helm-mini)
(global-set-key (kbd "M-o") 'helm-org-in-buffer-headings)
(global-set-key (kbd "C-o") 'org-open-at-point)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-l") 'persp-switch)
(global-set-key (kbd "M-;") 'comment-line)
(define-key org-mode-map (kbd "<C-M-S-left>") nil)
(define-key org-mode-map (kbd "<C-M-S-right>") nil)
(global-set-key (kbd "<C-M-S-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-M-S-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-M-S-down>") 'shrink-window)
(global-set-key (kbd "<C-M-S-up>") 'enlarge-window)

;;==============================================
;;  Sanity settings
;;==============================================

(setq-default make-backup-files nil
	      backup-inhibited t
	      create-lockfiles nil
	      auto-save-default nil
	      scroll-preserve-screen-position t
	      size-indication-mode 1)

(add-hook 'minibuffer-setup-hook
	  (lambda () (setq truncate-lines nil)))

(show-paren-mode 1)

(defun rob-scroll-down ()
      (interactive)
      (scroll-up 1))

(defun rob-scroll-up ()
      (interactive)
      (scroll-down 1))

(global-set-key (kbd "M-n") 'rob-scroll-down)
(global-set-key (kbd "M-p") 'rob-scroll-up)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.3)))

;;==============================================
;; Narrow or widen whatever I'm working on
;;==============================================

(defun narrow-or-widen-dwim (p)
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	((region-active-p)
	 (narrow-to-region (region-beginning) (region-end)))
	((derived-mode-p 'org-mode)
	 (cond ((ignore-errors (org-edit-src-code))
		(delete-other-windows))
	       ((org-at-block-p)
		(org-narrow-to-block))
	       (t (org-narrow-to-subtree))))
	(t (narrow-to-defun))))
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

;;==============================================
;; Sane copy org link to clipboard function
;;==============================================

(defun my-org-export-url ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
	 (text (when link-info
		 (buffer-substring-no-properties (or (cadr link-info) (point-min))
						 (or (caddr link-info) (point-max))))))
  (if (not text)
      (error "Not in org link!")
    (string-match org-bracket-link-regexp text)
    (kill-new (substring text (match-beginning 1) (match-end 1))))))
(global-set-key (kbd "C-c e") 'my-org-export-url)


;;==============================================
;; Custom settings
;;==============================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-enable-icon nil)
 '(cursor-type (quote (bar . 3)))
 '(custom-enabled-themes (quote (doom-one-light)))
 '(custom-safe-themes
   (quote
    ("99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "b9dda6ca36e825766dfada5274cf18d8a5bce70676b786e3260094e0cd8c0e62" "6cbf6003e137485fb3f904e76fb15bc48abc386540f43f54e2a47a9884e679f6" "7f74a3b9a1f5e3d31358b48b8f8a1154aab2534fae82c9e918fb389fca776788" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "d6f04b6c269500d8a38f3fabadc1caa3c8fdf46e7e63ee15605af75a09d5441e" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "f7ef6451d988d6e2fc86deea398eee02b3371703d88f265d31a011bd240dcf99" "f56393685517a0c58952a5fefdc45e29b52cc1798688992d112f7c299325a889" "39464ed440476d616c5671ff4d9cfc2393846132390e0d80e611dfa0b4bd6983" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(org-export-backends (quote (ascii html md odt)))
 '(package-selected-packages
   (quote
    (company-box all-the-icons-dired dired-subtree tablist peep-dired rainbow-mode helm helm-swoop helm-rg helm-projectile helm-org flyspell-correct-helm dired-narrow doom-themes smex multiple-cursors lua-mode expand-region minions iedit rainbow-delimiters persp-projectile perspective flyspell-correct magit projectile doom-modeline all-the-icons undo-tree avy company org winum org-bullets which-key use-package)))
 '(persp-modestring-dividers (quote ("(" ")" "|"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#5B6268" :slant italic))))
 '(org-table ((t (:family "Hack"))))
 '(persp-selected-face ((t (:foreground "orange" :weight bold))))
 '(variable-pitch ((t (:family "Noto Sans")))))
(put 'narrow-to-region 'disabled nil)
