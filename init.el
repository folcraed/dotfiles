;; My Emacs settings Ver 2.7
;; File or commit timestamp show when last updated.
;; Some settings are in the early-init.el file

;; ==============================================
;;  Set up some defaults
;; ==============================================
(setq lexical-binding t)
(setq use-short-answers t)
(set-fringe-mode 10)
(prefer-coding-system 'utf-8)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(setq read-process-output-max (* 1024 1024))
(cua-mode t)
(set-face-attribute 'default nil :font "JetBrainsMono-10")
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono-10")
(set-face-attribute 'variable-pitch nil :font "Roboto-10")

;; This is suppose to fix ??? displaying instead
;; of line numbers in modeline
(setq line-number-display-limit-width 2000)

(package-initialize)

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; ==============================================
;;  Minion for the rest
;; ==============================================
(use-package minions)

;; ==============================================
;;  My necessary packages
;; ==============================================
(use-package which-key)
(which-key-mode)

(use-package avy
  :config
  (setq avy-case-fold-search nil))

(use-package iedit)
(require 'iedit)

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package rainbow-mode)

(use-package rg)
(setq rg-use-transient-menu t)

(use-package fzf)

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
(setq dired-kill-when-opening-new-dired-buffer t)

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

(use-package all-the-icons)

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
	org-ellipsis " ▼"
	org-tags-column 0
	org-use-tag-inheritance nil))

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
        ("\\.pdf\\'" . "xdg-open %s"))))

(setq org-directory "~/Dropbox/Notes")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
         "* TODO %?")
        ("c" "Changes" entry (file+headline "~/Dropbox/Notes/Software.org" "Changes")
         "* %t %?")
        ("j" "Jots" entry (file+headline "~/Dropbox/Notes/Notebook.org" "Refile")
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

(add-hook 'org-mode-hook 'variable-pitch-mode)
(custom-theme-set-faces
  'user
  '(org-code ((t (:inherit fixed-pitch))))
  '(org-property-value ((t (:inherit fixed-pitch))))
  '(org-table ((t (:inherit fixed-pitch))))
  '(org-block ((t (:inherit fixed-pitch))))
  '(org-tag ((t (:inherit fixed-pitch))))
  '(org-verbatim ((t (:inherit fixed-pitch)))))

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
;; Get Org to show heading path so it can be
;; assigned to a keybind defined later
;; ==============================================
(defun rw/show-org-path ()
  "Shows the full heading path of the current org point"
  (interactive)
  (org-display-outline-path nil t " > " nil))

;; ==============================================
;;  Vertico and friends
;; ==============================================
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c m" . consult-mode-command)
         ;; Other custom bindings
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         :map isearch-mode-map
         ("M-e" . consult-isearch-history))         ;; orig. isearch-edit-string
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package company
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 3)
  :init
  (global-company-mode))

(use-package company-posframe)
(company-posframe-mode 1)

;; ==============================================
;;  Programming
;; ==============================================
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-symbol-highlighting nil)
  :config
  (lsp-enable-which-key-integration t)
  :hook
  (python-mode . lsp-deferred)
  (go-mode . lsp-deferred))

(use-package lsp-jedi
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

(use-package go-mode)
(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-enabled-clients 'gopls))

;; ==============================================
;;  Flyspell stuff
;; ==============================================
(use-package flyspell-correct
  :after flyspell)

(setq ispell-program-name "aspell"
      ispell-dictionary "en_US"
      ispell-local-dictionary "american"
      flyspell-issue-message-flag nil)
(global-set-key (kbd "<f8>") 'flyspell-mode)

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
  :custom ((markdown-enable-wiki-links t)
	   (markdown-wiki-link-alias-first nil)
	   (markdown-wiki-link-search-type '(parent-directories)))
  :init (setq markdown-open-command "okular"))

;; ===============================================
;; Some personal keybindings
;; ===============================================
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c i") 'org-table-insert-row)
(global-set-key (kbd "C-c p") 'rw/show-org-path)
(global-set-key (kbd "C-c k") 'consult-yank-from-kill-ring)
(global-set-key (kbd "C-c rb") 'rotate-frame-anticlockwise)
(global-set-key (kbd "C-c rc") 'rotate-frame-clockwise)
(global-set-key (kbd "C-c rs") 'rotate-frame)
(global-set-key (kbd "C-c s") 'flyspell-region)
(global-set-key (kbd "C-c t") 'org-time-stamp)
(global-set-key (kbd "C-c w") 'flyspell-correct-wrapper)
(global-set-key (kbd "C-c x") 'kill-buffer-and-window)
(global-set-key (kbd "C-c y") 'org-store-link)
(global-set-key (kbd "M-s g") 'consult-ripgrep)
(global-set-key (kbd "M-s f") 'fzf-find-file)
(global-set-key (kbd "M-f") 'consult-line)
(global-set-key (kbd "C-p") 'find-file)
(global-set-key (kbd "C-b") 'consult-buffer)
(global-set-key (kbd "M-m") 'push-mark-command)
(global-set-key (kbd "M-b") 'avy-pop-mark)
(global-set-key (kbd "M-o") 'consult-org-heading)
(global-set-key (kbd "C-o") 'org-open-at-point)
(global-set-key (kbd "M-g g") 'avy-goto-line)
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
	      shr-image-animate nil
	      tab-always-indent 'complete)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'minibuffer-setup-hook
	  (lambda () (setq truncate-lines nil)))

(show-paren-mode 1)

;; ==============================================
;; Some line movement keys I find easier
;; ==============================================
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
;; Custom settings
;; ==============================================
(setq custom-safe-themes t)
(load-theme 'doom-one-light t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type '(bar . 2))
 '(org-export-backends '(ascii html md odt))
 '(package-selected-packages
   '(go-mode lsp-jedi lsp-mode fzf all-the-icons company-posframe company vertico consult orderless marginalia project org markdown-mode flyspell-correct rg winum which-key use-package tablist rainbow-mode rainbow-delimiters org-superstar minions magit iedit expand-region doom-themes doom-modeline avy transpose-frame async))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#9ca0a4" :slant italic))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit fixed-pitch))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit fixed-pitch))))
 '(org-verbatim ((t (:inherit fixed-pitch)))))

;; Some package was overriding this, so put it last
(put 'narrow-to-region 'disabled nil)
