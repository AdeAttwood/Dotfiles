;;; zoekt.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/
;;
;; Zoekt code search integration. Allows you to search the zoekt index with a
;; search term and display the results in ivy. This will work the same as
;; `counsel-ag` but over the
;;
;; See: https://github.com/google/zoekt

(defun zoekt--function (query)
  "The internal search function that will be run by `ivy-read`"
  (or
   (let ((ivy-text query))
	 (ivy-more-chars))
   (progn
	 (counsel--async-command
	  (concat "zoekt -r " (shell-quote-argument query) " 2> /dev/null"))
	 '("" "Searching..."))))

(defun zoekt (&optional initial-input)
  "Zoekt code search integration. Allows you to search the zoekt index with a
search term and display the results in ivy. This will work the same as
`counsel-ag` but over the zoeket index"
  (interactive)
  (let ((default-directory "~/Code/src"))
  (ivy-read "zoekt: " #'zoekt--function
			:initial-input initial-input
			:dynamic-collection t
			:keymap counsel-ag-map
			:action #'counsel-git-grep-action
			:caller 'zoekt
			:require-match t)))
