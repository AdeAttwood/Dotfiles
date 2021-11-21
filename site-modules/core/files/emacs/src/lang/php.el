;;; php.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")
  :hook ((php-mode . (lambda() (setq flycheck-local-checkers '((lsp . ((next-checkers . (php))))))))
		 (php-mode . lsp-deferred)
		 (php-mode . tree-sitter-hl-mode))
