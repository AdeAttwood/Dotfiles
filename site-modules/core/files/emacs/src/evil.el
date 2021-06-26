;;; evil.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(defun aa/semicolon-at-end-of-line ()
  (interactive)
    (end-of-line)
    (insert ";"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  ;; Set window movement to CTRL hjkl to emulate tmux vim interaction
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-j") 'evil-window-down)
  (global-set-key (kbd "C-l") 'evil-window-right)
  (global-set-key (kbd "C-k") 'evil-window-up)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (define-key evil-insert-state-map (kbd "C-;") 'aa/semicolon-at-end-of-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :init
  (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :init
  ;; `s' for surround instead of `substitute'
  ;; see motivation here:
  ;; https://github.com/syl20bnr/spacemacs/blob/develop/doc/DOCUMENTATION.org#the-vim-surround-case
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
  :config
  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :after evil-collection
  :config
  (evil-multiedit-default-keybinds))