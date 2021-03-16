;;; notmuch.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(defvar notmuch-message-deleted-tags '("+deleted" "-inbox" "-unread"))

(defun spacemacs/notmuch-search-archive-thread-down ()
  "Search thread up."
  (interactive)
  (notmuch-search-archive-thread))

(defun spacemacs/notmuch-search-archive-thread-up ()
  "Search thread down."
  (interactive)
  (notmuch-search-archive-thread)
  (notmuch-search-previous-thread)
  (notmuch-search-previous-thread))

(defun spacemacs//notmuch-message-delete (go-next)
  "Delete message and select GO-NEXT message."
  (notmuch-search-tag notmuch-message-deleted-tags)
  (if (eq 'up go-next)
      (notmuch-search-previous-thread)
    (notmuch-search-next-thread)))

(defun spacemacs/notmuch-message-delete-down ()
  "Delete a message and select the next message."
  (interactive)
  (spacemacs//notmuch-message-delete 'down))

(defun spacemacs/notmuch-message-delete-up ()
  "Delete a message and select the previous message."
  (interactive)
  (spacemacs//notmuch-message-delete 'up))

(defun aa/notmuch-search-inbox ()
  "Search the notmuch inbox."
  (interactive)
  (notmuch-search "tag:inbox"))

(use-package notmuch
  :defer t
  :commands notmuch
  :config
  (setq notmuch-saved-searches '((:name "inbox"
					:key "i"
					:query "tag:inbox"
					:sort-order newest-first)
				 (:name "unread"
					:key "u"
					:query "tag:unread"
					:sort-order newest-first)
				 (:name "all"
					:key "a"
					:query "*"
					:sort-order newest-first)))
  ;; key bindings
  (evil-define-key 'normal notmuch-search-mode-map
    "=" 'notmuch-refresh-this-buffer
    "?" 'notmuch-help
    "*" 'notmuch-search-tag-all
    "-" 'notmuch-search-remove-tag
    "+" 'notmuch-search-add-tag
    "J" 'notmuch-jump-search
    "m" 'notmuch-mua-new-mail
    "s" 'notmuch-search
    "a" 'spacemacs/notmuch-search-archive-thread-down
    "A" 'spacemacs/notmuch-search-archive-thread-up
    "d" 'spacemacs/notmuch-message-delete-down
    "D" 'spacemacs/notmuch-message-delete-up)
  (evil-define-key 'normal notmuch-show-mode-map
    "B" 'notmuch-show-browse-urls)
  (evil-define-key 'visual notmuch-search-mode-map
    "-" 'notmuch-search-remove-tag
    "+" 'notmuch-search-add-tag
    "d" 'spacemacs/notmuch-message-delete-down))

(use-package counsel-notmuch :defer t)
