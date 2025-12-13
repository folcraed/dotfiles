;;; -*- lexical-binding: t; -*-
;; Rob's Emacs settings
;; File or commit timestamp show when last updated.
;; Startup settings are in the early-init.el file

;; ==============================================
;;  Set up some defaults
;; ==============================================
(setopt use-short-answers t)
(prefer-coding-system 'utf-8)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(setq read-process-output-max (* 1024 1024))
(cua-mode t)
(set-face-attribute 'default nil :font "JetBrainsMono" :height 105)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono" :height 105)

;; This is suppose to fix ??? displaying instead
;; of line numbers in modeline
(setopt line-number-display-limit-width 2000)

(package-initialize)

(require 'use-package)
(setopt use-package-always-ensure t)

;; ==============================================
;;  Org-mode
;; ==============================================
(use-package org
  :config
  (setopt org-hide-emphasis-markers t
		  org-startup-indented t
		  org-support-shift-select t
		  org-ellipsis " ▼"
		  org-tags-column 0
		  org-use-tag-inheritance nil
		  org-hide-leading-stars t))

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setopt org-superstar-headline-bullets-list '("●" "○"))
  (setopt org-superstar-item-bullet-alist
          '((?* . ?•)
			(?+ . ?◦)
			(?- . ?•))))

(setq-default org-file-apps
			  (quote
			   ((auto-mode . emacs)
				("\\.png\\'" . "xdg-open %s")
				("\\.jpg\\'" . "xdg-open %s")
				("\\.pdf\\'" . "xdg-open %s")
				("\\.ods\\'" . "xdg-open %s"))))

(setopt org-directory "~/Dropbox/Notes")
(setopt org-capture-templates
		'(("t" "Todo" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
           "* TODO %?")
          ("g" "Genealogy" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
           "* TODO %a %?")
          ("c" "Changes" entry (file "~/Dropbox/Notes/Changes.org")
           "* %t %?")
          ("j" "Jots" entry (file+headline "~/Dropbox/Notes/jots.org" "Refile")
           "* %?")))

(setopt org-todo-keywords
		'((sequence "TODO" "WORKING" "DONE")))

(setq-default org-display-custom-times t)
(setq-default org-export-headline-levels 6)
(setopt org-time-stamp-custom-formats '("%a %e %b %Y" . "%a %e %b %Y %H:%M")
		org-agenda-files (quote ("~/Dropbox/Notes/agenda.org"))
		org-archive-location "./archive.org::* Finished"
		org-id-link-to-org-use-id 'use-existing
		org-keep-stored-link-after-insertion t
		org-link-file-path-type 'noabbrev
		calendar-latitude 46.8
		calendar-longitude -113.9
		calendar-location-name "Missoula")

(setopt org-refile-targets
		'((nil :maxlevel . 2)
		  (org-agenda-files :maxlevel . 2)))

;; ==============================================
;;  Turn off holidays not wanted in agenda
;; ==============================================
(setopt holiday-bahai-holidays nil
		holiday-hebrew-holidays nil
		holiday-islamic-holidays nil
		holiday-oriental-holidays nil)

;; ==============================================
;; Get Org to show heading path so it can be
;; assigned to a keybind defined later
;; ==============================================
(defun rw/show-org-path ()
  "Show the full heading path of the current org point."
  (interactive)
  (org-display-outline-path nil t " > " nil))

;; ==============================================
;;  Org insert a file link (keybind below)
;; ==============================================
(defun rw/insert-file-link ()
    "Insert a file link at the current point."
  (interactive)
  (let ((current-prefix-arg '(4))) ;; emulates C-u
    (call-interactively 'org-insert-link)))

(keymap-set org-mode-map "C-c f" 'rw/insert-file-link)

;; Use the shortcuts for source and quote blocks
(require 'org-tempo)

;; ==============================================
;;  Org mind-map
;; ==============================================
(add-to-list 'load-path "/home/rob/.config/emacs/other/org-graphviz-mindmap")
(require 'org-graphviz-mindmap)
(setopt org-graphviz-mindmap-output-format "png")

;; ==============================================
;;  Minion for the rest
;; ==============================================
(use-package minions)

;; ==============================================
;;  My necessary packages
;; ==============================================
(which-key-mode)

(use-package avy
  :config
  (setq avy-case-fold-search nil))

(use-package iedit)
(require 'iedit)

(use-package expand-region
  :config
  (keymap-global-set "C-=" 'er/expand-region))

(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package rainbow-mode)

(use-package rg)
(rg-enable-default-bindings)
(setopt rg-use-transient-menu t)

;; ==============================================
;;  Dired enhancements
;; ==============================================
(setopt delete-by-moving-to-trash t
		dired-listing-switches "-ahlv --group-directories-first"
		dired-dwim-target t)

(defun dired-open-file ()
  "In Dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(put 'dired-find-alternate-file 'disabled nil)
(setopt dired-kill-when-opening-new-dired-buffer t)

;; ===============================================
;;  Doom modeline & theme
;; ===============================================
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setopt doom-modeline-height 25
		  doom-modeline-minor-modes t
		  doom-modeline-major-mode-color-icon t
		  doom-modeline-buffer-modification-icon t
		  doom-modeline-buffer-encoding nil
		  doom-modeline-buffer-file-name-style 'relative-from-project
		  doom-modeline-env-enable-python t
		  doom-modeline-env-version t
		  mode-line-right-align-edge 'right-fringe
		  column-number-mode t))
(minions-mode 1)

(use-package doom-themes
  :config
  (setopt doom-themes-enable-bold t
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
  (setopt winum-auto-setup-mode-line nil)
  (winum-mode))

;; ==============================================
;;  Transpose windows
;; ==============================================
(use-package transpose-frame)

;; ==============================================
;;  Vertico and friends
;; ==============================================
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c d" . consult-mode-command)
         ;; Other custom bindings
         ;; ("C-h a" . consult-apropos)            ;; orig. apropos-command
         :map isearch-mode-map
         ("M-e" . consult-isearch-history))	;; orig. isearch-edit-string
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setopt register-preview-delay 0
          register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  :config
  (setopt consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-root project))))))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package corfu
 :custom
 (corfu-auto t)
 (corfu-auto-prefix 3)
 (corfu-auto-delay 0.5)
 (corfu-preview-current nil)
 (completion-ignore-case t)
 (tab-always-indent 'complete)
 (text-mode-ispell-word-completion nil)
 :bind
 (:map corfu-map
	("RET" . nil))
 :init
 (global-corfu-mode))

(use-package cape
 :after corfu
 :init
 (add-to-list 'completion-at-point-functions #'cape-file)
 (add-to-list 'completion-at-point-functions #'cape-dict)
 (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; ==============================================
;;  Flyspell stuff
;; ==============================================
(use-package flyspell-correct
  :after flyspell)

(setopt ispell-program-name "aspell"
		ispell-dictionary "en_US"
		ispell-local-dictionary "american"
		flyspell-issue-message-flag nil
		dictionary-server "dict.org")

;; ===============================================
;;  Magit
;; ===============================================
(use-package magit)

;; ===============================================
;; Markdown editing
;; ===============================================
(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom ((markdown-enable-wiki-links t)
		   (markdown-wiki-link-alias-first nil)
		   (markdown-wiki-link-search-type '(parent-directories))
		   (markdown-hide-markup t))
  :config (setq markdown-open-command "okular")
  :bind (:map markdown-mode-map
			  ("C-o" . markdown-follow-link-at-point)))

;; Use default apps for handling URLs in markdown
(setopt browse-url-handlers
		'(("\\.pdf\\'" . browse-url-xdg-open)
		  ("\\.jpg\\'" . browse-url-xdg-open)
		  ("\\.png\\'" . browse-url-xdg-open)))

;; ===============================================
;; Lua code
;; ===============================================
(use-package lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editong mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; ===============================================
;; Treesitter
;; ===============================================
(setopt major-mode-remap-alist
		'((lua-mode . lua-ts-mode)
		  (js-mode . js-ts-mode)
		  (python-mode . python-ts-mode)))

;; ===============================================
;; Some personal keybindings
;; ===============================================
(keymap-global-set "C-c k" 'consult-yank-from-kill-ring)
(keymap-global-set "C-c r b" 'rotate-frame-anticlockwise)
(keymap-global-set "C-c r c" 'rotate-frame-clockwise)
(keymap-global-set "C-c r s" 'rotate-frame)
(keymap-global-set "<f6>" 'flyspell-buffer)
(keymap-global-set "<f7>" 'flyspell-mode)
(keymap-global-set "<f8>" 'flyspell-region)
(keymap-global-set "<f9>" 'insert-char)
(keymap-global-set "C-c w" 'flyspell-correct-wrapper)
(keymap-global-set "C-c x" 'kill-buffer-and-window)
(keymap-global-set "C-c #" 'rainbow-mode)
(keymap-global-set "M-s g" 'consult-ripgrep)
(keymap-global-set "M-s b" 'rgrep)
(keymap-global-set "M-s f" 'consult-fd)
(keymap-global-set "M-s l" 'consult-line-multi)
(keymap-global-set "M-s r" 'consult-register)
(keymap-global-set "M-s s" 'consult-register-store)
(keymap-global-set "M-f" 'consult-line)
(keymap-global-set "C-p" 'find-file)
(keymap-global-set "C-b" 'consult-buffer)
(keymap-global-set "C-c b" 'consult-buffer-other-window)
(keymap-global-set "M-'" 'push-mark-command)
(keymap-global-set "M-m" 'consult-mark)
(keymap-global-set "M-l" 'avy-goto-line)
(keymap-global-set "M-o" 'consult-outline)
(keymap-global-set "M-g" 'consult-goto-line)
(keymap-global-set "M-G" 'goto-line)
(keymap-global-set "M-j" 'avy-goto-char-timer)
(keymap-global-set "M-;" 'comment-line)
(keymap-global-set "M-d" 'duplicate-dwim)
(keymap-global-set "M-#" 'dictionary-lookup-definition)
(keymap-global-set "C-q" 'delete-frame)
(keymap-set org-mode-map "C-c a" 'org-agenda)
(keymap-set org-mode-map "C-c c" 'org-capture)
(keymap-set org-mode-map "C-c m" 'org-emphasize)
(keymap-set org-mode-map "C-c p" 'rw/show-org-path)
(keymap-set org-mode-map "C-c e" 'org-toggle-link-display)
(keymap-set org-mode-map "C-c t" 'org-time-stamp)
(keymap-set org-mode-map "C-c y" 'org-store-link)
(keymap-set org-mode-map "C-c z" 'org-id-get-create)
(keymap-set org-mode-map "C-o" 'org-open-at-point)
(keymap-set org-mode-map "C-M-S-<left>" nil) ; Originally decrease numbers
(keymap-set org-mode-map "C-M-S-<right>" nil) ; Originally increase numbers
(keymap-global-set "C-M-S-<left>" 'shrink-window-horizontally)
(keymap-global-set "C-M-S-<right>" 'enlarge-window-horizontally)
(keymap-global-set "C-M-S-<down>" 'shrink-window)
(keymap-global-set "C-M-S-<up>" 'enlarge-window)
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-set dired-mode-map "C-c o" 'dired-open-file)

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
			  case-fold-search nil
			  tab-width 4
			  xref-search-program 'ripgrep
			  grep-command "rg -ns --no-heading ")

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'minibuffer-setup-hook
		  (lambda () (setq truncate-lines nil)))

(show-paren-mode 1)
(electric-indent-mode -1)
(electric-pair-mode 1)

;; ==============================================
;; Some line movement keys I find easier
;; ==============================================
(defun rob-scroll-down ()
  "Scrolls the page down by one line at a time."
  (interactive)
  (scroll-up 1))

(defun rob-scroll-up ()
  "Scrolls the page up by one line at a time."
  (interactive)
  (scroll-down 1))

(defun rob-move-line-up ()
  "Move the cursor line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun rob-move-line-down ()
  "Move the cursor line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(keymap-global-set "M-n" 'rob-scroll-down)
(keymap-global-set "M-p" 'rob-scroll-up)
(keymap-global-set "M-S-<up>" 'rob-move-line-up)
(keymap-global-set "M-S-<down>" 'rob-move-line-down)

;; ==============================================
;; Narrow or widen whatever I'm working on
;; ==============================================
(defun narrow-or-widen-dwim (p)
  "Make it easier to switch between narrow and wide P."
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
(keymap-global-set "<f5>" #'narrow-or-widen-dwim)

;; ==============================================
;; Move to the new window when opened
;; ==============================================
(defun split-and-follow-horizontally ()
  "Split the window horizontally and go to new split."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(keymap-global-set "C-x 2" 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
    "Split the window vertically and go to new split."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(keymap-global-set "C-x 3" 'split-and-follow-vertically)

;; ==============================================
;; Sort selection by common delimiter
;; Found in the EmacsWiki
;; ==============================================
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;; ===============================================
;; Aliases
;; ===============================================
(defalias 'lp 'list-packages)

;; ==============================================
;; Custom settings
;; ==============================================
(setopt custom-safe-themes t)
(load-theme 'doom-one-light t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-mark-holidays-flag t)
 '(cursor-type '(bar . 2))
 '(org-export-backends '(ascii html md odt))
 '(package-selected-packages
   '(all-the-icons avy cape consult corfu doom-modeline doom-themes
				   expand-region flyspell-correct
				   gnu-elpa-keyring-update iedit lua-mode magit
				   marginalia markdown-mode minions orderless
				   org-superstar ox-gfm project rainbow-delimiters
				   rainbow-mode rg transpose-frame vertico winum))
 '(project-vc-extra-root-markers '("project.txt"))
 '(safe-local-variable-values '((flycheck-checker . python-flake8))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#9ca0a4" :slant italic))))
 '(link ((t (:foreground "#51afef" :underline nil :weight bold))))
 '(outline-6 ((t (:extend t :foreground "#2257a0" :weight bold))))
 '(outline-7 ((t (:extend t :foreground "#c678dd" :weight bold))))
 '(outline-8 ((t (:extend t :foreground "#5699AF" :weight bold)))))

;; Some package was overriding this, so put it last
(put 'narrow-to-region 'disabled nil)
(setopt gc-cons-threshold (* 2 1000 1000))
(provide 'init)
;;; init.el ends here.
