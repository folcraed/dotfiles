;; My Emacs settings Ver 0.7
;; File or commit timestamp show when last updated.

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
;; (menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; (global-display-line-numbers-mode)
(set-default-font "Ubuntu Mono-13")
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
  :ensure t
  :config (persp-mode))
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

(use-package pdf-tools
  :ensure t
  :pin manual
  :config
  (pdf-tools-install))

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
  :config
  (setq doom-modeline-height 15
	doom-modeline-minor-modes 1
	doom-modeline-major-mode-color-icon 1
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
  (setq org-startup-folded nil
	org-startup-indented t
	org-tags-column 80))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("◉" "○" "✸")))

(setq-default org-file-apps
	      (quote
	       ((auto-mode . emacs)
		("\\.png\\'" . "xdg-open %s")
		("\\.jpg\\'" . "xdg-open %s")
		("\\.pdf\\'" . "xdg-open %s"))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
	 "* TODO %t %?")
	("c" "Changes" entry (file+headline "~/Dropbox/Notes/changes.org" "Changes")
	 "* %t %?")
	("r" "Reference" entry (file+headline "~/Dropbox/Notes/Reference.org" "Refile")
	 "* %?")
	("s" "Software" entry (file+headline "~/Dropbox/Notes/Software.org" "Refile")
	 "* %?")))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("[%a %b %e %Y]" . "<%a %b %e %Y %H:%M>")
      org-agenda-files (directory-files-recursively "~/Dropbox/Notes" "\.org$")
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
;; Elfeed
;;===============================================

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory "~/Dropbox/elfeed"))

(setq elfeed-feeds
    '("https://sachachua.com/blog/category/emacs-news/feed/"
      "https://forum.manjaro.org/c/announcements.rss"
      "http://feeds.feedburner.com/d0od"
      "http://feeds.arstechnica.com/arstechnica/index/"
      "https://viking-archaeology-blog.blogspot.com/feeds/posts/default"
      "http://archaeology-in-europe.blogspot.com/feeds/posts/default"
      "https://www.gnome-look.org/gnome-look-content.rdf"
      "https://www.heritagedaily.com/feed"
      "https://planet.ubuntu.com/rss20.xml"
      "https://www.linuxinsider.com/perl/syndication/rssfull.pl"
      "https://www.linuxjournal.com/node/feed"
      "http://planetkde.org/rss20.xml"
      "https://www.zdnet.com/blog/open-source/rss.xml"
      "https://www.phoronix.com/rss.php"
      "http://www.kde.org/dotkdeorg.rdf"
      "http://jonathanabennett.github.io/rss.xml"
      "http://planet.gnome.org/rss20.xml"
      "https://www.johngrenham.com/blog/feed/"
      "https://feeds.feedburner.com/familyhistorydaily"))
(setq shr-inhibit-images t)

;;===============================================
;; Some personal keybindings
;;===============================================

(define-prefix-command 'd-map)
(global-set-key (kbd "C-d") 'd-map)
(define-key d-map (kbd "c") 'org-capture)
(define-key d-map (kbd "a") 'org-agenda)
(define-key d-map (kbd "t") 'org-time-stamp)
(define-key d-map (kbd "k") 'helm-show-kill-ring)
(define-key d-map (kbd "l") 'org-store-link)
(define-key d-map (kbd "i") 'iedit-mode)
(define-key d-map (kbd "f") 'dired-narrow)
(define-key d-map (kbd "n") 'org-toggle-narrow-to-subtree)
(define-key d-map (kbd "r") 'helm-resume)
(define-key d-map (kbd "s") 'helm-swoop)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-p") 'projectile-find-file)
(global-set-key (kbd "C-b") 'helm-mini)
(global-set-key (kbd "M-o") 'helm-org-in-buffer-headings)
; (global-set-key (kbd "M-o") 'org-goto)
(global-set-key (kbd "C-f") 'helm-rg)
;; (global-set-key (kbd "C-s") 'helm-swoop-without-pre-input)
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
;; Custom settings
;;==============================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (doom-one)))
 '(custom-safe-themes
   (quote
    ("34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "f7ef6451d988d6e2fc86deea398eee02b3371703d88f265d31a011bd240dcf99" "f56393685517a0c58952a5fefdc45e29b52cc1798688992d112f7c299325a889" "39464ed440476d616c5671ff4d9cfc2393846132390e0d80e611dfa0b4bd6983" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(org-export-backends (quote (ascii html md odt)))
 '(package-selected-packages
   (quote
    (peep-dired rainbow-mode helm helm-swoop helm-rg helm-projectile helm-org flyspell-correct-helm dired-narrow doom-themes smex multiple-cursors lua-mode expand-region pdf-tools minions elfeed iedit rainbow-delimiters persp-projectile perspective flyspell-correct magit projectile doom-modeline all-the-icons undo-tree avy company org winum org-bullets which-key use-package)))
 '(persp-modestring-dividers (quote ("(" ")" "|"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#5B6268" :slant italic))))
 '(persp-selected-face ((t (:foreground "orange" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
