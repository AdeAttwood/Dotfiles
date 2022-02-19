;;; init.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/
;; ;;; Commentary:
;;
;;  The entrypoint to AMACS
;;
;;; Code:

;; (setq user-emacs-directory "~/development/emacs")
;; (customize-set-value 'custom-theme-directory user-emacs-directory)

;; Load package loading with `use-package` this must be the first
;; thing loaded so we can install the packages with `use-package`
(load-file (expand-file-name "src/packages.el" user-emacs-directory))

;; Load the ui tweeks and theme to limit the time we see a big white
;; screen
(load-file (expand-file-name "src/ui.el" user-emacs-directory))

;; Load the personal config if its available where all of the secrets
;; source is
(let ((personal-init (expand-file-name "init-personal.el" user-emacs-directory)))
 (when (file-exists-p personal-init)
   (load-file personal-init)))

;; Genral and main configuration
(load-file (expand-file-name "src/general.el" user-emacs-directory))
(load-file (expand-file-name "src/file-operations.el" user-emacs-directory))
(load-file (expand-file-name "src/evil.el" user-emacs-directory))
(load-file (expand-file-name "src/ivy.el" user-emacs-directory))
(load-file (expand-file-name "src/treemacs.el" user-emacs-directory))
(load-file (expand-file-name "src/term.el" user-emacs-directory))
(load-file (expand-file-name "src/language-tool.el" user-emacs-directory))

;; Set up puppet for editing the dotfiles config
(load-file (expand-file-name "src/puppet.el" user-emacs-directory))

;; Email configuration
(load-file (expand-file-name "src/notmuch.el" user-emacs-directory))

;; Org mode
(load-file (expand-file-name "src/org.el" user-emacs-directory))

;; Development configuration
(load-file (expand-file-name "src/format.el" user-emacs-directory))
(load-file (expand-file-name "src/development.el" user-emacs-directory))
(load-file (expand-file-name "src/zoekt.el" user-emacs-directory))
(load-file (expand-file-name "src/lang/php.el" user-emacs-directory))
(load-file (expand-file-name "src/lang/c-sharp.el" user-emacs-directory))
(load-file (expand-file-name "src/lang/js-ts.el" user-emacs-directory))
(load-file (expand-file-name "src/lang/yaml.el" user-emacs-directory))
(load-file (expand-file-name "src/lang/json.el" user-emacs-directory))
(load-file (expand-file-name "src/lang/web.el" user-emacs-directory))
(load-file (expand-file-name "src/lang/go.el" user-emacs-directory))
(load-file (expand-file-name "src/lang/docker.el" user-emacs-directory))
(load-file (expand-file-name "src/lang/shell.el" user-emacs-directory))
(load-file (expand-file-name "src/lang/c.el" user-emacs-directory))

;; Load the projectile module last so we can override compilation errors regexp
;; without other modules affecting it after
(load-file (expand-file-name "src/projectile.el" user-emacs-directory))

(use-package general
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
   ;; Global bindings
    "/"		'(counsel-projectile-ag :which-key "Search Project")
    "TAB"	'(evil-switch-to-windows-last-buffer :which-key "Last Buffer")
    "SPC"	'(counsel-M-x :which-key "M-x")
	"a"     '(projectile-toggle-between-implementation-and-test :which-key "Toggle test and implementation")
    ";"		'(evil-commentary-line :which-key "Comment")
	;; Docker
    "d"		'(:ignore t :which-key "Docker")
    "dd"	'docker
    "di"	'docker-images
    "dc"	'docker-containers
    ;; Jumping in a buffer
    "j"		'(:ignore t :which-key "Jumps")
    "jj"	'evil-avy-goto-char-timer
    "jl"	'evil-avy-goto-line
    ;; Searching
    "s"		'(:ignore t :which-key "Searching")
    "ss"	'swiper
    "sS"	'swiper-thing-at-point
    "sb"	'swiper-all
    "sB"	'swiper-all-thing-at-point
    ;; Windows. Just rebind all of the evil window bindings to "SPC w"
    ;; so we dont have to keep hitting "CTRL-w"
    "w"		'(evil-window-map :which-key "Windows")
    "wd"	'evil-window-delete
    ;; Org Mode
    "o"		'(evil-window-map :which-key "Org Mode")
    "oa"	'org-agenda
    "oc"    'org-capture
    ;; Git
    "g"		'(:ignore t :which-key "Git")
    "gs"    'magit-status
    "gd"	'magit-diff
    "gl"	'magit-log
	"gb"    'browse-at-remote
    ;; Email
    "e"		'(:ignore t :which-key "Email")
    "ei"    'aa/notmuch-search-inbox
    "eu"    'aa/notmuch-search-inbox-unread
    "es"	'counsel-notmuch
    "en"	'notmuch
    "ej"	'notmuch-jump-search
    ;; Files
    "f"		'(:ignore t :which-key "Files")
    "fs"	'save-buffer
    "ff"	'counsel-find-file
    ;; Buffers
    "b"		'(:ignore t :which-key "Buffers")
    "bd"	'kill-this-buffer
    "bb"	'counsel-switch-buffer
    ;; LSP actions
    "l"		'(:ignore t :which-key "LSP")
    "lr"	'lsp-workspace-restart
	"la"    'lsp-execute-code-action
    ;; Projects
    "p"		'(:ignore t :which-key "Projects")
    "p SPC"	'counsel-projectile
    "pb"	'counsel-projectile-switch-to-buffer
    "pd"	'counsel-projectile-find-dir
    "pp"	'counsel-projectile-switch-project
    "pf"	'counsel-projectile-find-file
    "pt"	'neotree-projectile-action
	"ps"    'projectile-run-vterm
    ;; Toggles
    "t"		'(:ignore t :which-key "Toggles")
    "tt"	'(counsel-load-theme :which-key "choose theme")))

(require 'server)
(unless (server-running-p)
        (message "Starting a server...")
        (server-start))
