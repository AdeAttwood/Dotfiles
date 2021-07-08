;;; term.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(defun aa/term-init ()
  ;; Hide the mode line in the terminal
  (hide-mode-line-mode)
  (evil-emacs-state)
  (setq cursor-type 'bar))

(use-package vterm
  :quelpa (vterm :fetcher github :repo "akermu/emacs-libvterm")
  :commands (vterm)
  :bind (:map vterm-mode-map
         ("C-o" . previous-buffer))
  :hook (vterm-mode . aa/term-init)
  :init
  (defvar vterm-install t)
  :config
  ;; Set the custom term prompt the oh-my-zsh "pygmalion" theme
  (setq term-prompt-regexp ".*⇒")
  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t))

(use-package auto-dim-other-buffers
  :config (auto-dim-other-buffers-mode)
  :custom-face
    (auto-dim-other-buffers-face ((t (:background "#e0e0e0")))))

(unless (display-graphic-p)
  (use-package tmux-pane
	:config (tmux-pane-mode))
  (use-package evil-terminal-cursor-changer
	:config (evil-terminal-cursor-changer-activate))

  (use-package xclip
	:config (xclip-mode))

  (xterm-mouse-mode))
