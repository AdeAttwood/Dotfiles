;;; docker.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :hook (dockerfile-mode . lsp-deferred)
  :mode "\\Dockerfile\\'")
