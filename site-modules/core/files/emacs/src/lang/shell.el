;;; shell.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

;; sh-mode is already installed just need to enable lsp in that mode
(add-hook 'sh-mode-hook 'lsp)

;; Add .env file to be highlighted with the sh-mode syntax
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.env\\.example\\'" . sh-mode))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package bats-mode :ensure t);
