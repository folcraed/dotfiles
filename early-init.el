;;; -*- lexical-binding: t -*-
(setq gc-cons-threshold 100000000)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq inhibit-startup-message t)
