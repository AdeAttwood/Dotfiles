;;; org.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :pin org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  ;; Don't fold headers when opening files I find it better to see all of the
  ;; files and navigate between the headers
  (setq org-startup-folded nil)

  ;;(setq org-agenda-start-with-log-mode t)
  ;;(setq org-log-done 'time)
  (setq org-agenda-window-setup 'current-window)

  ;; Open file links in current window, rather than new ones
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/config.el#L632
  (setf (alist-get 'file org-link-frame-setup) #'find-file)

  (setq org-refile-targets '(("Archive.org" :maxlevel . 1)))
  (setq org-agenda-files '("~/Nextcloud/org/workbook"
						   "~/Nextcloud/org/notes"
						   "~/Nextcloud/org/wiki"))
  (setq org-directory "~/Nextcloud/org")

  ;; Add notmuch link capture mainly for org-capture
  (org-link-set-parameters "notmuch"
			 :follow 'org-notmuch-open
			 :store 'org-notmuch-store-link)

  ;; Define a kanban style set of stages for todo tasks
  (setq org-todo-keywords
	'((sequence "TODO" "WAITING" "REVIEW" "|" "DONE" "ARCHIVED")))

  (require 'org-protocol)
  (require 'org-id)
  (require 'org-habit)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
	 (shell . t)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
		(todo "WAITING"
		  ((org-agenda-overriding-header "Waiting on External")))
		(todo "TODO"
			  ((org-agenda-overriding-header "Scheduled")
			   (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottimestamp))))
	    (todo "TODO"
			  ((org-agenda-overriding-header "Backlog")
			   (org-agenda-todo-list-sublevels nil)
			   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))))

	  ("w" "Workflow Status"
	   ((todo "WAITING"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
	    (todo "TODO"
		  ((org-agenda-overriding-header "Todo")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
		`(("t" "Task" entry (file aa/create-workbook-file) "\n\n*  TODO %?\n  %U\n  %a\n  %i\n\n")
		  ("c" "Task Clock In" entry (file aa/create-workbook-file) "\n\n* TODO %?\n:LOGBOOK:\nCLOCK: %U\n:END:\n  %U\n  %a\n  %i\n\n")
		  ("n" "New Note" entry (file aa/create-notes-file) "* Content")


		  ("p" "Protocol" entry (file aa/create-workbook-file) "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
		  ("L" "Protocol Link" entry (file aa/create-workbook-file) "* TODO %? [[%:link][%:description]] \nCaptured On: %U"))))

(defun org-wiki-search ()
  "Search the whole wiki for a pattern"
  (interactive)
  (let ((default-directory "~/Nextcloud/org"))
	(counsel-ag)))

(defun org-wiki-search-title ()
  "Search the though all of the wiki titles for a pattern"
  (interactive)
  (let ((default-directory "~/Nextcloud/org"))
	(counsel-ag "title: ")))

(defun org-wiki-search-heading ()
  "Search the though all of the wiki headings for a pattern"
  (interactive)
  (let ((default-directory "~/Nextcloud/org"))
	(counsel-ag "^\\* ")))

(defun aa/create-notes-file ()
  "Create an org file in ~/notes/."
  (expand-file-name (format "%s-%s.org"
							(format-time-string "%Y-%m-%d")
							(string-trim (shell-command-to-string "uuidgen")))
					"~/Nextcloud/org/notes/"))

(defun aa/create-workbook-file ()
  "Create an org file in ~/workbook/."
  (expand-file-name (format "%s.org"
							(format-time-string "%Y-%m"))
					"~/Nextcloud/org/workbook/"))

(defun workbook-current ()
  "Create an org file in ~/notes/."
  (interactive)
  (find-file
   (expand-file-name (format "%s.org"
							 (format-time-string "%Y-%m"))
					 "~/Nextcloud/org/workbook/")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-re-reveal
  :after org)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; This is needed as of Org 9.2
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("php" . "src php"))
