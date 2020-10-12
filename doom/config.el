;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq frame-title-format "Doom Emacs")

(setq doom-font (font-spec :family "JetBrains Mono" :size 13))
(setq doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13))
(setq doom-theme 'doom-one)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(setq-default org-file-apps
	      (quote
	       ((auto-mode . emacs)
		("\\.png\\'" . "sxiv %s")
		("\\.jpg\\'" . "sxiv %s")
		("\\.pdf\\'" . "zathura %s"))))

(setq doom-modeline-buffer-encoding nil)
(setq-default org-hide-emphasis-markers t)

(setq org-directory "~/Dropbox/Notes")

(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "DONE")))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("[%a %b %e %Y]" . "<%a %b %e %Y %H:%M>")
      org-agenda-files (quote ("~/Dropbox/Notes/agenda.org"))
      org-use-tag-inheritance nil
      org-outline-path-complete-in-steps nil
      org-ellipsis " ➥"
      org-tags-column 0)
      
(setq org-superstar-headline-bullets-list '("●" "○"))
(setq org-superstar-item-bullet-alist
      '((?* . ?•)
        (?+ . ?◦)
        (?- . ?•)))

(setq display-line-numbers-type nil)

(map! (:map org-mode-map
       :localleader
       :desc "Open link" "v" #'org-open-at-point))

(map! :n "g s g" #'counsel-rg
      :n "g s l" #'avy-goto-line)

(add-hook! org-mode-hook 'variable-pitch-mode t)

(custom-set-faces!
 '(font-lock-comment-face :foreground "#5B6268" :slant italic)
 '(org-block :inherit fixed-pitch)
 '(org-code :inherit fixed-pitch)
 '(org-table :inherit fixed-pitch))

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
;; Some other sanity savers.
;;==============================================
(remove-hook 'text-mode-hook #'spell-fu-mode)
(setq counsel-rg-base-command
      "rg --max-columns 500 --max-columns-preview --with-filename --no-heading --line-number %s")
(setq ivy-truncate-lines nil)
