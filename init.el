;; -*- lexical-binding: t -*-
;; My Emacs settings Ver 1.1
;; File or commit timestamp show when last updated.

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq frame-title-format "Emacs")
(setq icon-title-format "Emacs")
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(prefer-coding-system 'utf-8)
(global-visual-line-mode 1)
(global-hl-line-mode 1)

;; This is suppose to fix ??? displaying instead
;; of line numbers in modeline
(setq line-number-display-limit-width 2000)

;; ==============================================
;;  Set up repositories
;; ==============================================

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ==============================================
;;  Keep keyring up to date
;; ==============================================

(use-package gnu-elpa-keyring-update)

;; ==============================================
;;  Minion for the rest
;; ==============================================

(use-package minions)

;; ==============================================
;;  My necessary packages
;; ==============================================

(use-package which-key
  :config (which-key-mode))

(use-package avy
  :config
  (setq avy-case-fold-search nil))

(use-package company
  :config
  (setq company-idle-delay 0.5
	company-minimum-prefix-length 3))
(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
	company-box-max-candidates 50
	company-box-doc-enable nil))

(use-package iedit)
(require 'iedit)

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package rainbow-mode)

(use-package rg)
(rg-enable-default-bindings)

;; ==============================================
;;  Dired enhancements
;; ==============================================

(setq delete-by-moving-to-trash t
      dired-listing-switches "-ahlv --group-directories-first"
      dired-dwim-target t)

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(put 'dired-find-alternate-file 'disabled nil)

;; ===============================================
;;  Doom modeline & theme
;; ===============================================

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25
	doom-modeline-minor-modes t
	doom-modeline-major-mode-color-icon t
	doom-modeline-buffer-modification-icon t
	doom-modeline-buffer-encoding nil
	doom-modeline-buffer-file-name-style 'relative-from-project
	column-number-mode 1))
(minions-mode 1)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t))

;; ==============================================
;;  Winum settings
;; ==============================================

(use-package winum
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

;; ==============================================
;;  Transpose windows
;; ==============================================

(use-package transpose-frame)

;; ==============================================
;;  Org-mode
;; ==============================================

(use-package org
  :config
  (setq-default org-hide-emphasis-markers t)
  (setq org-startup-folded nil
	org-startup-indented t
	org-support-shift-select t
	org-ellipsis " ➥"
	org-tags-column 0))

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-headline-bullets-list '("●" "○"))
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?◦)
          (?- . ?•))))

(use-package org-roam
  :hook
  (org-mode . org-roam-mode)
  :custom
  (org-roam-directory "~/Dropbox/Notes/roam/")
  (org-roam-db-location "~/Dropbox/Notes/roam/org-roam.db")
  (org-roam-graph-executable "dot")
  (org-roam-graph-viewer "/usr/bin/firefox")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images nil
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  :bind ("C-c n s" . org-roam-server-mode))

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
        ("j" "Jots" entry (file+headline "~/Dropbox/Notes/jots.org" "Refile")
         "* %?")
        ("s" "Software" entry (file+headline "~/Dropbox/Notes/Software.org" "Refile")
         "* %?")))

(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "DONE")))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("[%a %b %e %Y]" . "<%a %b %e %Y %H:%M>")
      org-agenda-files (quote ("~/Dropbox/Notes/agenda.org"))
      org-use-tag-inheritance nil)

(setq org-refile-targets
      '((nil :maxlevel . 2)
	(org-agenda-files :maxlevel . 2)))

(require 'org-roam-protocol)
(setq org-roam-buffer-window-parameters '((no-delete-other-windows . t)))

;; ==============================================
;;  Helm and friends
;; ==============================================
(use-package helm
  :config (setq helm-M-x_fuzzy-match 1
		helm-autoresize-mode 0
		helm-display-buffer-default-height 18
		helm-split-window-inside-p 1
		helm-descbinds-window-style 'same-window))
(require 'helm-config)
(helm-mode 1)

(use-package helm-rg)
(use-package helm-projectile)

(use-package helm-org
  :config (setq helm-org-format-outline-path 1))
(require 'helm-org)

;; ==============================================
;;  Flyspell stuff
;; ==============================================

(use-package flyspell-correct
  :after flyspell)

(use-package flyspell-correct-helm
  :after flyspell-correct)

(setq ispell-program-name "aspell"
      ispell-dictionary "en_US"
      ispell-local-dictionary "american")
(global-set-key (kbd "<f8>") 'flyspell-buffer)

;; ==============================================
;; Projectile
;; ==============================================

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm))
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

;; ===============================================
;;  Magit
;; ===============================================

(use-package magit)

;; ===============================================
;; Lua for Awesome
;; ===============================================

(use-package lua-mode)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 4)

;; ===============================================
;; Some personal keybindings
;; ===============================================

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c f") 'projectile-find-file-other-window)
(global-set-key (kbd "C-c i") 'org-table-insert-row)
(global-set-key (kbd "C-c k") 'helm-show-kill-ring)
(global-set-key (kbd "C-c q") 'helm-resume)
(global-set-key (kbd "C-c s") 'flyspell-mode)
(global-set-key (kbd "C-c t") 'org-time-stamp)
(global-set-key (kbd "C-c w") 'flyspell-correct-wrapper)
(global-set-key (kbd "C-c x") 'kill-buffer-and-window)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-s g") 'helm-rg)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "C-p") 'projectile-find-file)
(global-set-key (kbd "C-b") 'helm-mini)
(global-set-key (kbd "M-o") 'helm-org-in-buffer-headings)
(global-set-key (kbd "C-o") 'org-open-at-point)
(global-set-key (kbd "M-g") 'avy-goto-line)
(global-set-key (kbd "M-G") 'goto-line)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-q") 'delete-frame)
(define-key org-mode-map (kbd "<C-M-S-left>") nil)
(define-key org-mode-map (kbd "<C-M-S-right>") nil)
(global-set-key (kbd "<C-M-S-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-M-S-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-M-S-down>") 'shrink-window)
(global-set-key (kbd "<C-M-S-up>") 'enlarge-window)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key dired-mode-map (kbd "C-c o") 'dired-open-file)

;; ==============================================
;;  Sanity settings
;; ==============================================

(setq-default make-backup-files nil
	      backup-inhibited t
	      create-lockfiles nil
	      auto-save-default nil
	      scroll-preserve-screen-position t
	      size-indication-mode 1
	      shr-max-image-proportion 0.9
	      shr-image-animate nil)

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
(cua-mode t)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.3)))

(set-face-attribute 'default nil :font "JetBrains Mono-10")
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono-10")

;; ==============================================
;; Narrow or widen whatever I'm working on
;; ==============================================

(defun narrow-or-widen-dwim (p)
  "Makes it easier to switch between narrow and wide"
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
(global-set-key (kbd "<f5>") #'narrow-or-widen-dwim)

;; ==============================================
;; Sane copy org link to clipboard function
;; ==============================================

(defun my-org-export-url ()
  "Copies the org link to the clipboard"
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

;; ==============================================
;; Custom settings
;; ==============================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-enable-icon nil)
 '(cua-normal-cursor-color "#61afef")
 '(cursor-type '(bar . 2))
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes
   '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "f9cae16fd084c64bf0a9de797ef9caedc9ff4d463dd0288c30a3f89ecf36ca7e" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "3e3a1caddeee4a73789ff10ba90b8394f4cd3f3788892577d7ded188e05d78f4" "37148381b35916d717945f3d0e1b2beb23c8b8383e5a7a879f1eaa4dde01d026" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "a83f05e5e2f2538376ea2bfdf9e3cd8b7f7593b16299238c1134c1529503fa88" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "b9dda6ca36e825766dfada5274cf18d8a5bce70676b786e3260094e0cd8c0e62" "6cbf6003e137485fb3f904e76fb15bc48abc386540f43f54e2a47a9884e679f6" "7f74a3b9a1f5e3d31358b48b8f8a1154aab2534fae82c9e918fb389fca776788" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "d6f04b6c269500d8a38f3fabadc1caa3c8fdf46e7e63ee15605af75a09d5441e" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "f7ef6451d988d6e2fc86deea398eee02b3371703d88f265d31a011bd240dcf99" "f56393685517a0c58952a5fefdc45e29b52cc1798688992d112f7c299325a889" "39464ed440476d616c5671ff4d9cfc2393846132390e0d80e611dfa0b4bd6983" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(org-export-backends '(ascii html md odt))
 '(package-selected-packages
   '(org-roam-server org-roam org helm flyspell-correct flyspell-correct-helm lua-mode rg helm-rg helm-org helm-projectile winum which-key use-package undo-fu tablist rainbow-mode rainbow-delimiters org-superstar minions magit iedit gnu-elpa-keyring-update expand-region doom-themes doom-modeline company company-box avy transpose-frame async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#5B6268" :slant italic))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "#ECBE7B" :weight normal)))))
 ;; '(org-block ((t (:inherit (shadow fixed-pitch)))))
 ;; '(org-code ((t (:inherit (shadow fixed-pitch)))))
 ;; '(org-table ((t (:inherit (shadow fixed-pitch))))))
(put 'narrow-to-region 'disabled nil)
(server-start)
;;; init.el ends here
