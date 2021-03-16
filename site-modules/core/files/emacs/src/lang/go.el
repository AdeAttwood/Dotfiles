;;; go.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/.local/share/go/bin"))
(use-package go-mode
  :ensure t
  :hook (go-mode . lsp-deferred)
  :mode "\\.go\\'")
