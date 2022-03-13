;;; language-tool.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(use-package languagetool
  :commands (languagetool-check languagetool-server-start)
  :init
  (setq languagetool-correction-language "en-GB")
  :config
  (setq languagetool-server-command "~/.local/share/LanguageTool/LanguageTool-5.6/languagetool-server.jar")
  (setq languagetool-console-command "~/.local/share/LanguageTool/LanguageTool-5.6/languagetool-commandline.jar"))
