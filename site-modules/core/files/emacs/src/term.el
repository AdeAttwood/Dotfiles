;;; term.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(defun aa/term-init ()
  ;; Hide the mode line in the terminal
  (hide-mode-line-mode))

(use-package vterm
  :quelpa (vterm :fetcher github :repo "akermu/emacs-libvterm")
  :commands (vterm)
  :hook (vterm-mode . aa/term-init)
  :init
  (defvar vterm-install t)
  :config
  ;; Set the custom term prompt the oh-my-zsh "pygmalion" theme
  (setq term-prompt-regexp ".*⇒")
  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t))
