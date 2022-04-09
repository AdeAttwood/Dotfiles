;;; ui.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(tooltip-mode -1)       ; Disable tooltips
(menu-bar-mode -1)      ; Disable the menu bar
(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar

;; Display the line column in the modeline not just the line number
(column-number-mode)

;; Set up the visible bell
(setq visible-bell t)

(defun aa/set-font (a-font-size)
  "Sets the font size for the whole of emacs setting the font faces

This is better than the `C-x +` and C-x - because this is global to emacs not
just in in the current buffer."
  (interactive "nFont Size: ")
  (setq a-font "Rec Mono Casual")
  ;; (setq a-font "LigaFreeMono")

  (set-face-attribute 'default nil :font a-font :height a-font-size)
  (set-face-attribute 'fixed-pitch nil :font a-font :height a-font-size)
  (set-face-attribute 'variable-pitch nil :font a-font :height a-font-size :weight 'regular))

;; Set the default font size when emacs starts
(aa/set-font 120)

(use-package base16-theme
  :quelpa (aa-base16-theme :fetcher github :repo "AdeAttwood/base16-emacs")
  :init
  ;; Fix terminal color theme for base16
  ;; See: https://github.com/belak/base16-emacs#terminal-colors
  ;; See: https://github.com/belak/base16-emacs/issues/73#issuecomment-721942335
  (setq base16-theme-256-color-source 'base16-shell)
  (setq base16-distinct-fringe-background nil)
  (load-theme 'base16-tomorrow-night t t)
  (enable-theme 'base16-tomorrow-night))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package hide-mode-line)

(use-package ligature
  :quelpa (ligature :fetcher github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
				       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
				       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
				       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
				       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
				       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
				       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
				       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
				       ">=" ">>" ">-" "-~" "-|" "->" "-<" "<~" "<*" "<|" "<:" "<$"
				       "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!" "##"
				       "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:" "?="
				       "?." "??" ";;" "/*" "/**" "/=" "/>" "__" "~~" "(*" "*)"
				       "://" "true" "false"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
