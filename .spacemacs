;; -*- mode: emacs-lisp -*-
;; File is subject to changes at any time, I experiment a lot ;-)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(lua
     (helm :variables spacemacs-helm-rg-max-column-number nil)
     theming
     (auto-completion :variables
                      auto-completion-use-company-box t)
     emacs-lisp
     git
     org
     (elfeed :variables
             rmh-elfeed-org-files (list "~/Dropbox/Notes/elfeed.org"))
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     treemacs
     search-engine
     )
   dotspacemacs-additional-packages '(doom-themes
                                      rainbow-mode
                                      dired-narrow
                                      dired-subtree
                                      peep-dired)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(vi-tilde-fringe
                                    neotree
                                    fancy-battery
                                    elfeed-goodies
                                    elfeed-web
                                    yasnippet
                                    auto-yasnippet
                                    google-translate)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-gc-cons '(20000000 0.1)
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-new-empty-buffer-major-mode 'text-mode
   dotspacemacs-themes '(doom-one-light
                         doom-tomorrow-night
                         doom-one
                         monokai
                         spacemacs-dark
                         spacemacs-light)
   dotspacemacs-mode-line-theme '(doom)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("JetBrains Mono"
                               :size 14
                               :weight normal
                               :width normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ";"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.3
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server nil
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "grep")
   dotspacemacs-frame-title-format "Spacemacs"
   dotspacemacs-icon-title-format "Spacemacs"
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-env ()
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  )

(defun dotspacemacs/user-load ()
  )

(defun dotspacemacs/user-config ()
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
;;==============================================================================
;; My preferred defaults
;;==============================================================================
  (setq-default make-backup-files nil
                backup-inhibited t
                create-lockfiles nil
                auto-save-default nil
                size-indication-mode 1)
  (setq-default tab-width 4 indent-tabs-mode nil)
  (setq ispell-program-name "aspell"
        ispell-dictionary "en_US"
        ispell-local-dictionary "american")
  (global-company-mode)
  (setq company-idle-delay 1)
  (setq avy-case-fold-search nil)
  (global-visual-line-mode t)
  (prefer-coding-system 'utf-8)
  (setq line-number-display-limit-width 2000000)
  (setq delete-by-moving-to-trash t)
  (setq shr-max-image-proportion 0.5)
  (treemacs-resize-icons 16)
;;==============================================================================
;; Custom keybindings
;;==============================================================================
  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/set-leader-keys "or" 'rainbow-mode)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "oe" 'my-org-export-url)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ol" 'org-store-link)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)
  (global-set-key (kbd "<f8>") 'flyspell-mode)
;;==============================================================================
;; Doom modeline settings
;;==============================================================================
  (setq-default doom-modeline-height 15)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil)
;;==============================================================================
;; Dired settings
;;==============================================================================
  (setq dired-listing-switches "-lah1v --group-directories-first")
  (setq wdired-create-parent-directories t)
  (setq dired-dwim-target t)
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-ignored-extensions '("mkv" "webm" "mp4" "mp3" "ogg" "iso"))
  (evil-define-key 'normal dired-mode-map "P" 'peep-dired)
  (evil-define-key 'normal dired-mode-map "o" 'dired-subtree-toggle)
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
    (kbd "k") 'peep-dired-prev-file)
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "xdg-open")
;;==============================================================================
;; Org settings
;;==============================================================================
  (setq-default org-startup-with-inline-images nil)
  (setq org-ellipsis " ➥")
  (setq org-superstar-headline-bullets-list '("●" "○"))
  (setq org-superstar-item-bullet-alist
  '((?* . ?•)
    (?+ . ?◦)
    (?- . ?•)))
  (setq-default org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.png\\'" . "sxiv %s")
     ("\\.pdf\\'" . "okular %s")
     ("\\.jpg\\'" . "sxiv %s"))))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
	 "* TODO %t %?")
	("c" "Changes" entry (file+headline "~/Dropbox/Notes/changes.org" "Changes")
	 "* %t %?")
	("j" "Jots" entry (file+headline "~/Dropbox/Notes/jots.org" "Refile")
	 "* %?")
	("s" "Software" entry (file+headline "~/Dropbox/Notes/Software.org" "Refile")
	 "* %?")))
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org")
  (setq-default org-startup-folded (quote showall))
  (setq org-startup-indented t)
  (setq org-todo-keywords
      '((sequence "TODO" "WORKING" "DONE")))
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("[%a %e %b %Y]" . "<%a %e %b %Y %H:%M>"))
  (setq-default org-hide-emphasis-markers t)
  (setq org-directory "~/Dropbox/Notes")
  (setq org-agenda-files (quote ("~/Dropbox/Notes/")))
  (setq org-tags-column 0))

;;==============================================================================
;; Emacs Custom settings
;;==============================================================================
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-enable-icon nil)
 '(cursor-type (quote (bar . 2)))
 '(package-selected-packages
   (quote
    (helm-gtags ivy company-lua lua-mode vmd-mode yaml-mode utop tuareg caml tide typescript-mode tern seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe rbenv rake pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements ocp-indent ob-elixir nodejs-repl mvn minitest meghanada maven-test-mode lsp-ui lsp-treemacs livid-mode skewer-mode simple-httpd live-py-mode json-navigator hierarchy json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc helm-pydoc helm-lsp lsp-mode markdown-mode groovy-mode groovy-imports pcache gradle-mode git-gutter-fringe+ fringe-helper git-gutter+ flycheck-pos-tip pos-tip flycheck-ocaml merlin flycheck-mix flycheck-credo emojify emoji-cheat-sheet-plus dune cython-mode company-emoji company-anaconda chruby bundler inf-ruby browse-at-remote blacken anaconda-mode pythonic alchemist elixir-mode ansi package-build shut-up epl git commander f s rainbow-mode srefactor yapfify stickyfunc-enhance importmagic epc ctable concurrent deferred xcscope ggtags flycheck counsel-gtags web-mode orgit magit-svn evil-nerd-commenter evil-magit dumb-jump doom-modeline aggressive-indent counsel company magit-popup magit transient haml-mode all-the-icons powerline ace-window dash org-plus-contrib evil yasnippet xterm-color ws-butler writeroom-mode winum which-key web-beautify volatile-highlights uuidgen use-package toc-org tagedit symon swiper string-inflection spaceline-all-the-icons smeargle slim-mode shrink-path shell-pop scss-mode sass-mode restart-emacs rainbow-delimiters pug-mode prettier-js popwin pfuture persp-mode pcre2el password-generator paradox overseer org-projectile org-preview-html org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file nameless multi-term move-text monokai-theme mmm-mode markdown-toc magit-gitflow macrostep lv lorem-ipsum link-hint indent-guide impatient-mode hungry-delete ht hl-todo highlight-parentheses highlight-numbers highlight-indentation goto-chg golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-commit gh-md fuzzy font-lock+ flx-ido fill-column-indicator eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav eldoc-eval editorconfig dotenv-mode diminish define-word counsel-projectile company-web company-statistics column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-dictionary auto-compile ace-link ac-ispell)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:slant italic)))))
)

