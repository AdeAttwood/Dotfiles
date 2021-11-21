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
   lsp-auto-guess-root t
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-file-watchers nil
   lsp-keep-workspace-alive nil
   lsp-ui-doc-position 'top
   lsp-modeline-diagnostics-scope :project
   lsp-completion-provider :none))

(advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

(defun aa/company-tab ()
    (interactive)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
		  (company-complete-common-or-cycle)))

(defun aa/company-sort (candidates)
  (let (case-fold-search
        (re "\\`[[:upper:]]*\\'"))
    (sort candidates
          (lambda (s1 s2)
            (and (string-match-p re s2)
                 (not (string-match-p re s1)))))))

(use-package company
  :after lsp-mode
  :bind
  (:map company-active-map
		("RET" . company-complete)
		("C-l" . yas-next-field-or-maybe-expand)
		("<tab>" . aa/company-tab)
		("TAB" . aa/company-tab))
  :config
  (global-company-mode)
  (push 'aa/company-sort company-transformers)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-limit 15)
  (setq company-dabbrev-downcase nil)
  (setq-default company-backends
		'((company-capf
		   :with
		   company-yasnippet
		   company-files)))
  :hook (lsp-mode . company-mode))

(use-package company-statistics
  :ensure t
  :after company
  :init
  (company-statistics-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq
   company-box-backends-colors nil
   company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(with-eval-after-load 'flycheck
  (setq flycheck-phpcs-standard "PSR2")
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

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

(defun aa/global-tab ()
    (interactive)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
		  (indent-for-tab-command)))

(define-minor-mode aa-tab-mode
  "Runs fmt on file save when this mode is turned on"
  :lighter " aa-tab"
  :global nil
	(global-set-key (kbd "<tab>") 'aa/global-tab)
	(global-set-key (kbd "TAB") 'aa/global-tab))

(define-globalized-minor-mode global-aa-tab-mode aa-tab-mode
  (lambda () (aa-tab-mode 1)))

(global-aa-tab-mode)
