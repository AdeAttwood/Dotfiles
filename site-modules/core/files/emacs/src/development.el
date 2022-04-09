;;; development.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/
;;
;; The main development configuration common development packages
;; like lsp and flycheck

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq-default
   lsp-intelephense-completion-trigger-parameter-hints nil
   lsp-clients-clangd-args '("--clang-tidy" "--header-insertion-decorators=0")
   lsp-auto-guess-root t
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-file-watchers nil
   lsp-keep-workspace-alive nil
   lsp-modeline-diagnostics-scope :project))

(advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy)

(defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

(defun aa/expand ()
  (interactive)
  (if (or (not yas/minor-mode)
		  (null (do-yas-expand)))
	  (if (string-empty-p (thing-at-point 'word 'no-properties))
		  (indent-for-tab-command)
		(if (fboundp 'emmet-expand-yas)
			(emmet-expand-yas)))))

(define-key evil-insert-state-map (kbd "C-e") 'aa/expand)

(use-package company
  :after lsp-mode
  :bind
  (:map company-active-map
		("RET" . company-complete)
		("C-l" . yas-next-field-or-maybe-expand)
		("<tab>" . aa/expand)
		("TAB" . aa/expand))
  :config
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-limit 15)
  ;; (setq company-dabbrev-downcase nil)
  ;; (setq-default company-backends
  ;; 		'((company-capf
  ;; 		   :with
  ;; 		   company-yasnippet
  ;; 		   company-files)))
  (global-company-mode)
  :hook (lsp-mode . company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq
   company-box-backends-colors nil
   company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-phpcs-standard "PSR2")
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Define local checkers so we can add next checkers per mode. LSP mode takes
;; priority and we can define the next checker in the mode hooks. Each language
;; will define there checkers if needed.
;;
;; Example usage for adding the php checker for php-mode
;; (setq flycheck-local-checkers '((lsp . ((next-checkers . (php))))))
(defvar-local flycheck-local-checkers nil)
(defun +flycheck-checker-get(fn checker property)
  (or (alist-get property (alist-get checker flycheck-local-checkers))
	  (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

;; Set the default column width to 80. The default in emacs is 70 that is too small for me
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-column 80)

(setq compilation-scroll-output 'first-error)

;; Set the default line number style to relative so when they are turned on I
;; dont need to worry about that
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package tree-sitter)
(use-package tree-sitter-langs)

(use-package magit
  :init
  ;; Force magit status to go full screen
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package magit-todos
  :init
  (magit-todos-mode 1)
  (global-hl-todo-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  :init
  (smartparens-global-mode))

;; To set different remote tyeps see https://github.com/rmuslimov/browse-at-remote#remote-types
(use-package browse-at-remote :defer t)

(use-package string-inflection :defer t)

(define-minor-mode aa-tab-mode
  "Tab mode by me"
  :lighter " aa-tab"
  :global nil
	(define-key prog-mode-map (kbd "<tab>") 'aa/expand)
	(define-key prog-mode-map (kbd "TAB") 'aa/expand))

(define-globalized-minor-mode global-aa-tab-mode aa-tab-mode
  (lambda () (aa-tab-mode 1)))

(global-aa-tab-mode)

;; https://people.gnome.org/~federico/blog/compilation-notifications-in-emacs.html
;; (setq compilation-finish-functions
;;       (append compilation-finish-functions
;;           '(fmq-compilation-finish)))

(setq compilation-finish-functions
          '(fmq-compilation-finish))

(defun fmq-compilation-finish (buffer status)
  (print status)
  (call-process "notify-send" nil nil nil
        "-t" "30000" ;; Display notification for 30 seconds
        "-i" (if (string-match "^finished" status) "dialog-ok" "dialog-no")
        "Compilation finished in Emacs"
        status))
