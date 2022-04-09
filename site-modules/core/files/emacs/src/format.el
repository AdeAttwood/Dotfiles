;;; format.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/
;;
;;; Commentary:
;; 
;;; Code:

;; Ensure the reformatter package is installed
(quelpa '(reformatter :fetcher git :url "https://github.com/purcell/emacs-reformatter"))
(require 'reformatter)
(require 'projectile)

(defun fmt--find-prettier ()
  "Find the 'prittier' executable.
This will be found from the project node_modules with a fall back to the
globally installed package"
  (let ((project-prittier (concat (projectile-project-root) "node_modules/.bin/prettier")))
	(message project-prittier)
	(if (file-exists-p project-prittier)
		project-prittier
	  "prettier")))

(defun fmt--find-phpcbf ()
  "Find the phpcbf executable.
It will look first in the locally installed version in your vendor folder, if
that cant be fond then it will try and use the globally installed executable"
  (let ((project-prittier (concat (projectile-project-root) "vendor/bin/phpcbf")))
	(message project-prittier)
	(if (file-exists-p project-prittier)
		project-prittier
	  "phpcbf")))

(defun fmt--find-php-ruleset ()
  "Find the phpcs ruleset for the current project.
If there is a 'ruleset.xml' file in your project root that will be used, if not
then the 'psr2' ruleset will be used"
  (let ((project-prittier (concat (projectile-project-root) "ruleset.xml")))
	(message project-prittier)
	(if (file-exists-p project-prittier)
		project-prittier
	  "psr2")))

(reformatter-define prettier-fmt
  :program (fmt--find-prettier)
  :args (list "--stdin-filepath" (buffer-file-name))
  :group 'fmt
  :lighter " PrettierFMT")

(reformatter-define phpcbf-fmt
  :program (fmt--find-phpcbf)
  :args (list (format "--standard=%s" (fmt--find-php-ruleset)) "-")
  :group 'fmt
  :exit-code-success-p (lambda (number) (= 1 number))
  :lighter " PhpCbfFMT")

(reformatter-define clang-fmt
  :program "clang-format"
  :args (list "--assume-filename" (buffer-file-name))
  :group 'fmt
  :lighter " ClangFMT")

;; Define our own jsonnet formatter. This is way faster then the
;; 'jsonnet-reformat-buffer command that comes with jsonnet-mode
(reformatter-define jsonnet-fmt
  :program "jsonnetfmt"
  :args (list "-")
  :group 'fmt
  :lighter " JsonnetFMT")

;; Set the gofmt command to be goimports we can use the built in functions in
;; go-mode they work just fine
(setq gofmt-command "goimports")

(defun fmt-buffer ()
  "Format the current buffer."
  (interactive)
  (cond
   ((eq major-mode 'c++-mode)            (clang-fmt))
   ((eq major-mode 'css-mode)            (prettier-fmt))
   ((eq major-mode 'go-mode)             (gofmt))
   ((eq major-mode 'js2-mode)            (prettier-fmt))
   ((eq major-mode 'jsonnet-mode)        (jsonnet-fmt))
   ((eq major-mode 'markdown-mode)       (prettier-fmt))
   ((eq major-mode 'php-mode)            (phpcbf-fmt))
   ((eq major-mode 'scss-mode)           (prettier-fmt))
   ((eq major-mode 'typescript-mode)     (prettier-fmt))
   ((eq major-mode 'typescript-tsx-mode) (prettier-fmt))
   ((eq major-mode 'web-mode)            (prettier-fmt))
   ((eq major-mode 'yaml-mode)           (prettier-fmt))
   ((eq 1 1)                             (message "No formatter found"))))

;;;###autoload
(define-minor-mode fmt-mode
  "Run fmt on file save when this mode is turned on."
  :lighter " fmt"
  :global nil
  (if fmt-mode
      (add-hook 'before-save-hook 'fmt-buffer nil 'local)
    (remove-hook 'before-save-hook 'fmt-buffer 'local)))

(define-globalized-minor-mode global-fmt-mode fmt-mode
  (lambda () (fmt-mode 1)))

(provide 'format)
;;; format.el ends here

