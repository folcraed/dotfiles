;; Rob's Emacs settings
;; File or commit timestamp show when last updated.
;; Startup settings are in the early-init.el file

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

;; This is suppose to fix ??? displaying instead
;; of line numbers in modeline
(setq line-number-display-limit-width 2000)

(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

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
        ("g" "Genealogy" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
         "* TODO %a %?")
        ("c" "Changes" entry (file+headline "~/Dropbox/Notes/Software.org" "Changes")
         "* %t %?")
        ("j" "Jots" entry (file+headline "~/Dropbox/Notes/Notebook.org" "Refile")
         "* %?")))

(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "DONE")))

(setq-default org-display-custom-times t)
(setq-default org-export-headline-levels 6)
(setq org-time-stamp-custom-formats '("[%a %b %e %Y]" . "<%a %b %e %Y %H:%M>")
      org-agenda-files (quote ("~/Dropbox/Notes/agenda.org"))
      org-id-link-to-org-use-id 'use-existing)

(setq org-refile-targets
      '((nil :maxlevel . 2)
	(org-agenda-files :maxlevel . 2)))

;; ==============================================
;; Sane copy org link to clipboard function
;; ==============================================
(defun my-org-export-url ()
  "Copies the org link to the clipboard."
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
	 (text (when link-info
		 (buffer-substring-no-properties (or (cadr link-info) (point-min))
						 (or (caddr link-info) (point-max))))))
    (if (not text)
	(error "Not in org link!")
      (string-match org-link-bracket-re text)
      (kill-new (substring text (match-beginning 1) (match-end 1))))))
(global-set-key (kbd "C-c e") 'my-org-export-url)

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

(define-key org-mode-map (kbd "C-c f") 'rw/insert-file-link)

;; Use the shortcuts for source and quote blocks
(require 'org-tempo)

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
(rg-enable-default-bindings)
(setq rg-use-transient-menu t)

;; ==============================================
;;  Dired enhancements
;; ==============================================
(setq delete-by-moving-to-trash t
      dired-listing-switches "-ahlv --group-directories-first"
      dired-dwim-target t)

(defun dired-open-file ()
  "In Dired, open the file named on this line."
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
	doom-modeline-env-enable-python t
	doom-modeline-env-enable-go t
	doom-modeline-env-version t
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
;;  Vertico and friends
;; ==============================================
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c d" . consult-mode-command)
         ;; Other custom bindings
         ;; ("<help> a" . consult-apropos)            ;; orig. apropos-command
         :map isearch-mode-map
         ("M-e" . consult-isearch-history))         ;; orig. isearch-edit-string
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-root project))))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  :bind
  (:map corfu-map
	("RET" . nil))
  :init
  (global-corfu-mode))

(setq corfu-auto-delay 0.5
      completion-styles '(orderless basic)
      corfu-preview-current t)

(use-package cape
  :bind ("C-c p f" . cape-file)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; ==============================================
;;  Flyspell stuff
;; ==============================================
(use-package flyspell-correct
  :after flyspell)

(setq ispell-program-name "aspell"
      ispell-dictionary "en_US"
      ispell-local-dictionary "american"
      flyspell-issue-message-flag nil)

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
	   (markdown-wiki-link-search-type '(parent-directories))
	   (markdown-hide-markup t))
  :init (setq markdown-open-command "okular"))

;; Use default apps for handling URLs in markdown
(setq browse-url-handlers
   '(("\\.pdf\\'" . browse-url-xdg-open)
     ("\\.jpg\\'" . browse-url-xdg-open)
     ("\\.png\\'" . browse-url-xdg-open)))

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
(global-set-key (kbd "<f6>") 'electric-pair-mode)
(global-set-key (kbd "<f7>") 'flyspell-buffer)
(global-set-key (kbd "<f8>") 'flyspell-mode)
(global-set-key (kbd "<f9>") 'flyspell-region)
(global-set-key (kbd "C-c t") 'org-time-stamp)
(global-set-key (kbd "C-c w") 'flyspell-correct-wrapper)
(global-set-key (kbd "C-c x") 'kill-buffer-and-window)
(global-set-key (kbd "C-c y") 'org-store-link)
(global-set-key (kbd "M-s g") 'consult-ripgrep)
(global-set-key (kbd "M-s b") 'rgrep)
(global-set-key (kbd "M-s f") 'consult-fd)
(global-set-key (kbd "M-f") 'consult-line)
(global-set-key (kbd "C-p") 'find-file)
(global-set-key (kbd "C-b") 'consult-buffer)
(global-set-key (kbd "M-'") 'push-mark-command)
(global-set-key (kbd "M-m") 'consult-mark)
(global-set-key (kbd "M-b") 'consult-register-store)
(global-set-key (kbd "M-g b") 'consult-register)
(global-set-key (kbd "M-l") 'markdown-follow-link-at-point)
(global-set-key (kbd "M-o") 'consult-outline)
(global-set-key (kbd "C-o") 'org-open-at-point)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-G") 'goto-line)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-q") 'delete-frame)
(define-key org-mode-map (kbd "<C-M-up>") 'org-table-move-row-up)
(define-key org-mode-map (kbd "<C-M-down>") 'org-table-move-row-down)
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
  "Scrolls the page down by one line at a time."
  (interactive)
  (scroll-up 1))

(defun rob-scroll-up ()
  "Scrolls the page up by one line at a time."
  (interactive)
  (scroll-down 1))

(defun move-line-up ()
  "Move the cursor line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move the cursor line down."
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
(global-set-key (kbd "<f5>") #'narrow-or-widen-dwim)

;; ==============================================
;; Move to the new window when opened
;; ==============================================
(defun split-and-follow-horizontally ()
  "Split the window horizontally and go to new split."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
    "Split the window vertically and go to new split."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; ==============================================
;; Select all inside delimiters Vim-like
;; ==============================================
(defun rw/select-text-in-delimiters ()
  "Select text between the nearest left and right delimiters."
  (interactive)
  (let (start end)
    (skip-chars-backward "^<>([{\"'")
    (setq start (point))
    (skip-chars-forward "^<>)]}\"'")
    (setq end (point))
    (set-mark start)))
(global-set-key (kbd "C-+") 'rw/select-text-in-delimiters)

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
 '(calendar-mark-holidays-flag t)
 '(cursor-type '(bar . 2))
 '(org-export-backends '(ascii html md odt))
 '(package-selected-packages
   '(all-the-icons vertico consult orderless marginalia corfu cape project markdown-mode flyspell-correct rg winum which-key use-package tablist rainbow-mode rainbow-delimiters org-superstar minions magit iedit expand-region doom-themes doom-modeline avy transpose-frame async))
 '(project-vc-extra-root-markers '("project.txt")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#9ca0a4" :slant italic))))
 '(link ((t (:foreground "#51afef" :underline nil :weight bold)))))

;; Some package was overriding this, so put it last
(put 'narrow-to-region 'disabled nil)
(provide 'init)
;;; init.el ends here.
