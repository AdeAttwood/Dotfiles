;;; language-tool.el --- AMACS -*- lexical-binding: t; -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(use-package languagetool
  :quelpa t
  :config
  (setq languagetool-default-language "en-GB")
  (setq languagetool-language-tool-jar
		"/.local/share/LanguageTool/LanguageTool-5.6/languagetool-commandline.jar"))
