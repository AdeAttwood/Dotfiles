;;; web.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(use-package web-mode
  :defer t
  :hook (web-mode . lsp-deferred)
  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.twig\\'"       . web-mode)
   ("\\.xml\\'"        . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode)
   ("\\.[gj]sp\\'"     . web-mode)
   ("\\.as[cp]x?\\'"   . web-mode)
   ("\\.eex\\'"        . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.handlebars\\'" . web-mode)
   ("\\.hbs\\'"        . web-mode)
   ("\\.eco\\'"        . web-mode)
   ("\\.ejs\\'"        . web-mode)
   ("\\.svelte\\'"     . web-mode)
   ("\\.ctp\\'"        . web-mode)
   ("\\.djhtml\\'"     . web-mode)))

(use-package sass-mode
  :defer t
  :hook (sass-mode . lsp-deferred)
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :defer t
  :hook (scss-mode . lsp-deferred)
  :mode ("\\.scss\\'" . scss-mode))

(use-package css-mode
  :defer t
  :hook (css-mode . lsp-deferred)
  :mode ("\\.css\\'" . css-mode))

(use-package less-css-mode
  :defer t
  :hook (less-css-mode . lsp-deferred)
  :mode ("\\.less\\'" . less-css-mode))

(use-package emmet-mode
  :defer t
  :hook ((web-mode	. emmet-mode)
	 (css-mode	. emmet-mode)
	 (sass-mode	. emmet-mode)
	 (js2-mode	. emmet-mode)
	 (rjsx-mode     . (lambda ()
			(emmet-mode)
			(setq-local emmet-expand-jsx-className? t)))
	 (typescript-tsx-mode . (lambda ()
			(emmet-mode)
			(setq-local emmet-expand-jsx-className? t)))))

