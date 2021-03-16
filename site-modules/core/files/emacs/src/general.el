;;; general.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq inhibit-startup-message t)

;; Don't wrap lines
(set-default 'truncate-lines t)

;; Set the default tab width to 4 from the default 8. This is only
;; really used in go and makefiles and 8 if just silly
(set-default 'tab-width 4)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package flyspell-correct-ivy
  :bind ("C-M-;" . flyspell-correct-next)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  (setq ispell-dictionary "british"))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode)
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

;; Sort out line numbers
;; https://stackoverflow.com/a/23857738
(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "looks for filenames like file:line or file:line:position and reparses name in such manner that position in file"
  (ad-set-arg 0
	      (mapcar (lambda (fn)
			(let ((name (car fn)))
			  (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
			      (cons
			       (match-string 1 name)
			       (cons (string-to-number (match-string 2 name))
				     (string-to-number (or (match-string 3 name) "")))
			       )
			    fn))) files)))
