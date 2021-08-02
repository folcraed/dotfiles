;; My Emacs settings Ver 1.6
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
  :bind (:map company-active-map
	      ("<tab>" . company-auto-commit))
  :config
  (setq company-idle-delay 0.5
	company-minimum-prefix-length 3))
(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
	company-box-doc-delay 0.2
	company-box-max-candidates 50))

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

(setq-default org-file-apps
    (quote
        ((auto-mode . emacs)
        ("\\.png\\'" . "sxiv %s")
        ("\\.jpg\\'" . "sxiv %s")
        ("\\.pdf\\'" . "zathura %s"))))

(setq org-directory "~/Dropbox/Notes")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
         "* TODO %?")
        ("c" "Changes" entry (file+headline "~/Dropbox/Notes/Software.org" "Changes")
         "* %t %?")
        ("j" "Jots" entry (file+headline "~/Dropbox/Notes/jots.org" "Refile")
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

(use-package helm-lsp)
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

;; ==============================================
;;  Flyspell stuff
;; ==============================================

(use-package flyspell-correct
  :after flyspell)

(use-package flyspell-correct-helm
  :after flyspell-correct)

(setq ispell-program-name "aspell"
      ispell-dictionary "en_US"
      ispell-local-dictionary "american"
      flyspell-issue-message-flag nil)
(global-set-key (kbd "<f8>") 'flyspell-region)

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
;; Markdown editing
;; ===============================================

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-open-command "okular"))

;; ===============================================
;; LSP Mode
;; ===============================================
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "M-l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-clients-lua-language-server-command "/usr/bin/lua-language-server")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil))

(use-package typescript-mode
  :hook ((typescript-mode . lsp-deferred)
	 (js-mode . lsp-deferred))
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
  :hook (python-mode . lsp-deferred))

(use-package lsp-jedi
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)))

(use-package lua-mode
  :hook (lua-mode . lsp-deferred))

;; ===============================================
;; Some personal keybindings
;; ===============================================

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c f") 'projectile-find-file-other-window)
(global-set-key (kbd "C-c i") 'org-table-insert-row)
(global-set-key (kbd "C-c k") 'helm-show-kill-ring)
(global-set-key (kbd "C-c m") 'helm-resume)
(global-set-key (kbd "C-c rb") 'rotate-frame-anticlockwise)
(global-set-key (kbd "C-c rc") 'rotate-frame-clockwise)
(global-set-key (kbd "C-c rs") 'rotate-frame)
(global-set-key (kbd "C-c s") 'flyspell-mode)
(global-set-key (kbd "C-c t") 'org-time-stamp)
(global-set-key (kbd "C-c w") 'flyspell-correct-wrapper)
(global-set-key (kbd "C-c x") 'kill-buffer-and-window)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-s g") 'helm-rg)
(global-set-key (kbd "M-f") 'helm-occur)
(global-set-key (kbd "C-p") 'projectile-find-file)
(global-set-key (kbd "C-b") 'helm-mini)
(global-set-key (kbd "M-m") 'push-mark-command)
(global-set-key (kbd "M-b") 'avy-pop-mark)
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
(global-set-key (kbd "<C-M-up>") 'org-table-move-row-up)
(global-set-key (kbd "<C-M-down>") 'org-table-move-row-down)
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

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-n") 'rob-scroll-down)
(global-set-key (kbd "M-p") 'rob-scroll-up)
(global-set-key (kbd "M-S-<up>") 'move-line-up)
(global-set-key (kbd "M-S-<down>") 'move-line-down)
(cua-mode t)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.3)))

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-10")
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font-10")
(set-face-attribute 'variable-pitch nil :font "Noto Sans-10")

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
(setq custom-safe-themes t)
(load-theme 'doom-one t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   '(company-capf company-bbdb company-semantic company-files company-cmake company-clang
		  (company-dabbrev-code company-gtags company-etags company-keywords)
		  company-oddmuse company-dabbrev))
 '(cursor-type '(bar . 2))
 '(org-export-backends '(ascii html md odt))
 '(package-selected-packages
   '(helm-lsp lua-mode python-mode lsp-jedi typescript-mode lsp-ui lsp-mode org markdown-mode helm projectile flyspell-correct flyspell-correct-helm rg helm-rg helm-org helm-projectile winum which-key use-package tablist rainbow-mode rainbow-delimiters org-superstar minions magit iedit gnu-elpa-keyring-update expand-region doom-themes doom-modeline company company-box avy transpose-frame async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#5B6268" :slant italic)))))
(put 'narrow-to-region 'disabled nil)
