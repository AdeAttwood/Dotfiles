;;; php.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(use-package php-mode
  :ensure t
  :hook (php-mode . lsp-deferred)
  :mode "\\.php\\'")
