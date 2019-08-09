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
   '(csv
     (helm :variables spacemacs-helm-rg-max-column-number nil)
     theming
     auto-completion
     emacs-lisp
     lua
     git
     multiple-cursors
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     pdf
     treemacs
     search-engine
     elfeed
     )
   dotspacemacs-additional-packages '(doom-themes
                                      rainbow-mode
                                      org-preview-html)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(vi-tilde-fringe
                                    neotree
                                    fancy-battery
                                    auto-yasnippet
                                    elfeed-goodies
                                    elfeed-org
                                    elfeed-web
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
   dotspacemacs-themes '(doom-one
                         doom-one-light
                         monokai
                         spacemacs-dark
                         spacemacs-light)
   dotspacemacs-mode-line-theme '(doom)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Ubuntu Mono"
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
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "grep")
   dotspacemacs-frame-title-format "%b - %I"
   dotspacemacs-icon-title-format nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-env ()
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  )

(defun dotspacemacs/user-load ()
  )

(defun dotspacemacs/user-config ()
  (spacemacs/set-leader-keys "mxl" 'org-store-link)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "xl" 'org-store-link)
  (setq-default make-backup-files nil
                backup-inhibited t
                create-lockfiles nil
                auto-save-default nil
                size-indication-mode 1)
  (setq-default tab-width 4 indent-tabs-mode nil)
  (setq ispell-program-name "aspell")
  (global-company-mode)
  (setq company-idle-delay 1)
  (setq avy-case-fold-search nil)
  (global-visual-line-mode t)
  (setq line-number-display-limit-width 2000000)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "gs") 'avy-goto-char-timer)
  (global-set-key (kbd "<f8>") 'flyspell-mode)
  (setq-default doom-modeline-height 16)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-color-icon 1
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil)
  ;; (setq-default powerline-height 16)
  (setq lua-indent-level 4)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "xdg-open")
  (setq-default org-startup-with-inline-images nil)
  (setq-default org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.png\\'" . "sxiv %s")
     ("\\.jpg\\'" . "sxiv %s")
     ("\\.pdf\\'" . "zathura %s"))))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Notes/agenda.org" "Todos")
	 "* TODO %t %?")
	("c" "Changes" entry (file+headline "~/Dropbox/Notes/changes.org" "Changes")
	 "* %t %?")
	("r" "Reference" entry (file+headline "~/Dropbox/Notes/Reference.org" "Refile")
	 "* %?")
	("s" "Software" entry (file+headline "~/Dropbox/Notes/Software.org" "Refile")
	 "* %?")))
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org")
  (setq-default org-startup-folded (quote showall))
  (setq org-startup-indented t)
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("[%a %e %b %Y]" . "<%a %e %b %Y %H:%M>"))
  (setq-default org-hide-emphasis-markers t)
  (setq org-agenda-files (directory-files-recursively "~/Dropbox/Notes" "\.org$")))
  (setq org-tags-column 80)
  (setq elfeed-db-directory "~/Dropbox/elfeed")
  (setq elfeed-feeds
    '("https://sachachua.com/blog/category/emacs-news/feed/"
	  "https://forum.manjaro.org/c/announcements.rss"
      "https://feeds.feedburner.com/d0od"
      "https://feeds.arstechnica.com/arstechnica/index/"
      "https://viking-archaeology-blog.blogspot.com/feeds/posts/default"
      "http://archaeology-in-europe.blogspot.com/feeds/posts/default"
      "https://www.gnome-look.org/gnome-look-content.rdf"
      "https://www.heritagedaily.com/feed"
      "https://planet.ubuntu.com/rss20.xml"
      "https://www.linuxinsider.com/perl/syndication/rssfull.pl"
      "https://www.linuxjournal.com/node/feed"
      "https://planetkde.org/rss20.xml"
      "https://www.zdnet.com/blog/open-source/rss.xml"
      "https://www.phoronix.com/rss.php"
      "https://www.kde.org/dotkdeorg.rdf"
      "http://jonathanabennett.github.io/rss.xml"
      "https://planet.gnome.org/rss20.xml"
      "https://www.johngrenham.com/blog/feed/"
      "https://feeds.feedburner.com/familyhistorydaily"))
;; (setq shr-inhibit-images t)
(defun dotspacemacs/emacs-custom-settings ()
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-mode csv-mode srefactor yapfify stickyfunc-enhance importmagic epc ctable concurrent deferred xcscope ggtags flycheck counsel-gtags web-mode orgit magit-svn evil-nerd-commenter evil-magit dumb-jump doom-modeline aggressive-indent counsel company magit-popup magit transient haml-mode all-the-icons powerline ace-window dash org-plus-contrib evil yasnippet xterm-color ws-butler writeroom-mode winum which-key web-beautify volatile-highlights uuidgen use-package toc-org tagedit symon swiper string-inflection spaceline-all-the-icons smeargle slim-mode shrink-path shell-pop scss-mode sass-mode restart-emacs rainbow-delimiters pug-mode prettier-js popwin pfuture persp-mode pcre2el password-generator paradox overseer org-projectile org-preview-html org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file nameless multi-term move-text monokai-theme mmm-mode markdown-toc magit-gitflow macrostep lv lorem-ipsum link-hint indent-guide impatient-mode hungry-delete ht hl-todo highlight-parentheses highlight-numbers highlight-indentation goto-chg golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-commit gh-md fuzzy font-lock+ flx-ido fill-column-indicator eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav eldoc-eval editorconfig dotenv-mode diminish define-word counsel-projectile company-web company-statistics column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-dictionary auto-compile ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:slant italic)))))
)

