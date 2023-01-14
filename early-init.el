;;; -*- lexical-binding: t -*-
(setq gc-cons-threshold 100000000)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq ring-bell-function 'ignore)
(setq frame-title-format "Emacs")
(setq icon-title-format "Emacs")
