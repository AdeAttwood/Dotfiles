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

(defun efs/lsp-mode-setup ()
  "Set up the LSP mode."
  (setq lsp-enable-which-key-integration t
	lsp-enable-xref nil
	lsp-intelephense-completion-trigger-parameter-hints nil
	lsp-intelephense-multi-root nil
	lsp-enable-file-watchers nil
	lsp-enable-snippet t
	lsp-ui-doc-position 'top
	lsp-headerline-breadcrumb-enable nil))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq
	lsp-intelephense-completion-trigger-parameter-hints nil
	lsp-intelephense-multi-root nil
	lsp-enable-file-watchers nil
	lsp-enable-snippet t
	lsp-ui-doc-position 'top
	lsp-headerline-breadcrumb-enable nil))
  ;;:hook (lsp-mode . efs/lsp-mode-setup))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :bind
  (:map company-active-map
		("C-l" . yas-next-field-or-maybe-expand)
		("RET" . company-complete))
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  :hook (lsp-mode . company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Set the default column width to 80. The default in emacs is 70 that is too small for me
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-column 80)


;; Set the default line number style to relative so when they are turned on I
;; dont need to worry about that
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package magit
  :ensure t
  :init
  ;; Force magit status to go full screen
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package smartparens
  :config
  (require 'smartparens-config)
  :init
  (smartparens-global-mode))

;; To set different remote tyeps see https://github.com/rmuslimov/browse-at-remote#remote-types
(use-package browse-at-remote :defer t)

(use-package string-inflection :defer t)
