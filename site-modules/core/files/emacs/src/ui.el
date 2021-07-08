;;; ui.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room
(menu-bar-mode -1)      ; Disable the menu bar

;; Display the line column in the modeline not just the line number
(column-number-mode)

;; Set up the visible bell
(setq visible-bell t)

(setq-default a-font-size 120)
(setq-default a-font "LigaFreeMono")

(set-face-attribute 'default nil :font a-font :height a-font-size)
(set-face-attribute 'fixed-pitch nil :font a-font :height a-font-size)
(set-face-attribute 'variable-pitch nil :font a-font :height a-font-size :weight 'regular)

(use-package doom-themes
  :init
  (load-theme 'doom-tomorrow-day t t)
  (when (display-graphic-p)
    (enable-theme 'doom-tomorrow-day)))

(use-package base16-theme
  :init
  ;; Fix terminal color theme for base16
  ;; See: https://github.com/belak/base16-emacs#terminal-colors
  ;; See: https://github.com/belak/base16-emacs/issues/73#issuecomment-721942335
  (setq base16-theme-256-color-source 'base16-shell)
  (setq base16-distinct-fringe-background nil)
  (load-theme 'base16-tomorrow t t)
  (unless (display-graphic-p)
    (enable-theme 'base16-tomorrow)))

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
