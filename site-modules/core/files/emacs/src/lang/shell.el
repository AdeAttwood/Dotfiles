;;; shell.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

;; sh-mode is already installed just need to enable lsp in that mode
(add-hook 'sh-mode-hook 'lsp)
