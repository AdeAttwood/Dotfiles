;;; ts-js.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(use-package js2-mode
  :mode "\\.m?js\\'"
  :interpreter "node"
  :hook (js2-mode . lsp-deferred)
  :commands js2-line-break
  :config
  (setq js-chain-indent t
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t
        js2-idle-timer-delay 0.1))

(use-package rjsx-mode
  :mode
  (("components/.+\\.js$" . rjsx-mode))
  :hook (rjsx-mode . lsp-deferred)
  :init
  (defun +javascript-jsx-file-p ()
    "Detect React or preact imports early in the file."
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))

  (add-to-list 'magic-mode-alist '(+javascript-jsx-file-p . rjsx-mode))
  (setq-local emmet-expand-jsx-className? t))

(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :mode
  (("\\.ts\\'"      . typescript-mode)
   ("\\.tsx\\'"      . typescript-mode)))

(use-package prettier-js :commands prettier-js)
