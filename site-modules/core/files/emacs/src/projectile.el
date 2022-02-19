;;; projectile.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(use-package projectile
  :quelpa t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Code/src")
    (setq projectile-project-search-path '("~/Code/src")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :quelpa t
  :config (counsel-projectile-mode))

(defun projectile--get-php-test-command ()
  "Get the correct test command for the PHP project.
This will find the test configuration file and work out the correct command to
run from the configuration file"
  (cond
   ((file-exists-p (concat (projectile-project-root) "vendor/bin/codecept")) "vendor/bin/codecept run --no-ansi")
   ((file-exists-p (concat (projectile-project-root) "vendor/bin/simple-phpunit")) "vendor/bin/simple-phpunit --colors=never")
   ((file-exists-p (concat (projectile-project-root) "vendor/bin/phpunit")) "vendor/bin/phpunit")))

(defun projectile--get-php-test-file-command (file)
  "Get the correct command for testing a PHP FILE.
For more details on getting the test command see
`projectile--get-php-test-command`"
  (cond
   ((file-exists-p (concat (projectile-project-root) "vendor/bin/codecept")) (format "vendor/bin/codecept run --no-ansi %s" file))
   ((file-exists-p (concat (projectile-project-root) "vendor/bin/simple-phpunit")) (format "vendor/bin/simple-phpunit --colors=never %s" file))
   ((file-exists-p (concat (projectile-project-root) "vendor/bin/phpunit")) (format "vendor/bin/phpunit %s" file))))

(defun projectile-test-php ()
  "Test the PHP project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
  (compile (projectile--get-php-test-command))))

(defun projectile-test-php-file ()
  "Test the current file with the PHP command.
If you are in a test file then the 'alternate' file will be run with the test
function for your project"
  (interactive)
  (let ((default-directory (projectile-project-root)))
  (compile (projectile--get-php-test-file-command (projectile-find-matching-test (buffer-file-name))))))

(defun projectile--get-js-test-command ()
  "Test the project with JS command.
The correct test command will be found baised on what configuration you have in
your project"
  (cond
   ((file-exists-p (concat (projectile-project-root) "jest.config.js")) "jest --no-colors")))

(defun projectile--get-js-test-file-command (file)
  "Test a FILE with the JS command."
  (cond
   ((file-exists-p (concat (projectile-project-root) "jest.config.js")) (format "jest --no-colors %s" file))))

(defun projectile-test-js ()
  "Test the current project with JS."
  (interactive)
  (let ((default-directory (projectile-project-root)))
  (compile (projectile--get-js-test-command))))

(defun projectile-test-js-file ()
  "Test the current file with the JS command.
If you are not on a test file then the 'alternate' file will be tested"
  (interactive)
  (let ((default-directory (projectile-project-root)))
  (compile (projectile--get-js-test-file-command (projectile-find-matching-test (buffer-file-name))))))

(defun projectile-test-file ()
  "Test the current file with the project test command.
The command will be found based on the current file extension and the
configuration in the project"
  (interactive)
  (let ((file-extention (file-name-extension (buffer-file-name))))
    (cond ((string= file-extention "js") (projectile-test-js-file))
          ((string= file-extention "jsx") (projectile-test-js-file))
          ((string= file-extention "tsx") (projectile-test-js-file))
          ((string= file-extention "ts") (projectile-test-js-file))
          ((string= file-extention "php") (projectile-test-php-file)))))

(setq practically-make/related-files
      (list
       (projectile-related-files-fn-test-with-suffix "js" ".test")
       (projectile-related-files-fn-test-with-suffix "jsx" ".test")
       (projectile-related-files-fn-test-with-suffix "ts" ".test")
       (projectile-related-files-fn-test-with-suffix "tsx" ".test")
       (projectile-related-files-fn-test-with-prefix "php" "Test")
       (projectile-related-files-fn-test-with-suffix "php" "Test")
       (projectile-related-files-fn-test-with-suffix "php" "Cest")))

(projectile-register-project-type 'practically-make '("Makefile" ".ctrc.yml")
                                  :project-file "Makefile"
                                  :configure "make init"
                                  :install "make install"
                                  :test "make test"
                                  :related-files-fn practically-make/related-files)

;; Add jest errors to the
(add-to-list 'compilation-error-regexp-alist 'jest-erorrs)
(add-to-list 'compilation-error-regexp-alist-alist
             '(jest-erorrs "^[ ]*at .* (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))" 1 2))

;; Add psalm errors so the compilation can pick them up
(add-to-list 'compilation-error-regexp-alist 'psalm-erorrs)
(add-to-list 'compilation-error-regexp-alist-alist
             '(psalm-erorrs "^ERROR: .* - \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2))
