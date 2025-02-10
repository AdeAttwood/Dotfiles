;;; init.el --- AMACS -*- lexical-binding: t -*-

;;; Package management
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; UI Setup

(tooltip-mode -1)     ; Disable tooltips
(menu-bar-mode -1)    ; Disable the menu bar
(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1)    ; Disable the toolbar

(defun aa/set-font (a-font-size)
  "Sets the font size for the whole of emacs setting the font faces

This is better than the `C-x +` and C-x - because this is global to emacs not
just in in the current buffer."
  (interactive "nFont Size: ")

  (setq aa-font "FiraCode Nerd Font Mono")
  (setq aa-v-font "Sans Serif")
  ;; (setq aa-v-font "Ubuntu")

  (set-face-attribute 'default nil :font aa-font :height a-font-size)
  (set-face-attribute 'fixed-pitch nil :font aa-font :height a-font-size)
  (set-face-attribute 'variable-pitch nil :font aa-v-font :height a-font-size :weight 'regular))

;; Set the default font size when emacs starts
(aa/set-font 100)

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; General Setup

;; Disable .# lock files. Then sends all directory watch tasks crazy. It also
;; make compiling applications fail when you have unsaved files
(setq create-lockfiles nil)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq browse-url-browser-function 'browse-url-chrome)

(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;;; Org

(straight-use-package 'org)

(require 'org-habit)
(require 'org-id)
(require 'org-protocol)
(require 'org-tempo)
(require 'ob)

(use-package ob-nushell
  :straight '(ob-nushell :type git :host github :repo "ln-nl/ob-nushell")
  :config
  (require 'ob-nushell)
  (setq org-babel-nushell-command "~/.cargo/bin/nu"
        ob-nushell-command-options "--error-style p")

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t) ; Enable Emacs Lisp
      (shell . t)
      (nushell . t))))

(defun aa/org-roam-update-tasklist-tag ()
  "Add or remove the 'tasklist' filetag based on the presence of TODO items in
  the current buffer. Ensure the filetag is inserted right under the title if
  not present."
  (when (org-roam-file-p)  ; Ensure this only runs on Org-roam files
    (save-excursion
      (goto-char (point-min))
      ;; Locate the title or the first heading
      (let ((has-title (re-search-forward "^#\\+title:" nil t))
            (has-todo nil))
        ;; Check for TODOs in the file
        (goto-char (point-min))
        (while (re-search-forward org-todo-regexp nil t)
               (setq has-todo t))

        ;; Determine the correct place for filetags (right below the title or
        ;; first heading)
        (goto-char (point-min))
        (when has-title
          (re-search-forward "^#\\+title:.*$" nil t))

        ;; Insert filetags if missing
        (unless (re-search-forward "^#\\+filetags:" (save-excursion (outline-next-heading) (point)) t)
          ;; Move under title or first heading
          (end-of-line)
          (insert "\n#+filetags:"))

        ;; Go to the filetags line and modify based on presence of TODOs
        (goto-char (point-min))
        (when (re-search-forward "^#\\+filetags:.*$" nil t)
          (let ((tags-line (match-string 0)))
            (if has-todo
              (unless (string-match-p ":tasklist:" tags-line)
                ;; Add the tasklist tag if not present and TODOs exist
                (end-of-line)
                (insert " :tasklist:"))
              (when (string-match-p ":tasklist:" tags-line)
                ;; Remove the tasklist tag if no TODOs exist
                (replace-match (replace-regexp-in-string ":tasklist:" "" tags-line) nil nil nil 0)))))))))

;; Add the function to before-save-hook so it runs on every save
(add-hook 'before-save-hook 'aa/org-roam-update-tasklist-tag)

(defun aa/org-roam-tasklist-files ()
  "Return a list of note files containing 'tasklist' tag."
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"tasklist\"%"))]))))

(defun aa/org-agenda-files (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files
    (append (aa/org-roam-tasklist-files) (list org-directory))))

(advice-add 'org-agenda :before #'aa/org-agenda-files)
(advice-add 'org-todo-list :before #'aa/org-agenda-files)

(setq org-directory "~/Org"
      org-todo-keywords '((sequence "TODO" "WAITING" "REVIEW" "|" "DONE" "ARCHIVED"))
      org-agenda-prefix-format '((agenda . " %?-12t% s")
                                (todo . "  ")
                                (tags . " %i %-12:c")
                                (search . " %i %-12:c"))
      org-hide-emphasis-markers t
      org-agenda-window-setup 'current-window
      org-export-with-broken-links "mark"
      org-export-with-section-numbers nil
      org-export-with-sub-superscripts nil
      org-export-with-toc nil
      org-html-head "<style>body { font-family: system-ui; }</style>")

(defun aa/search-org-roam-notes ()
  "Search in the Org-Roam folder using counsel-rg."
  (interactive)
  (counsel-rg nil org-roam-directory nil "Search Org Roam: "))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting on External")))
          (todo "TODO"
                ((org-agenda-overriding-header "Backlog")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
          (todo "TODO"
                   ((org-agenda-overriding-header "Scheduled After This Week")
                    (org-agenda-skip-function
                (lambda ()
                   (let* ((scheduled (org-get-scheduled-time (point)))
                          (end-of-week
                           (time-add (current-time)
                                     (days-to-time
                                      (- 7 (string-to-number (format-time-string "%u")))))))
                     (if (and scheduled
                              (time-less-p end-of-week scheduled))
                         nil
                       (org-end-of-subtree t)))))))

  ))))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "Todo.org" "Inbox")
         "* TODO %?\n\n  %a")))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
        ("t" "todo" entry
         "* TODO %?\n\n  %a"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(setq org-refile-targets
      `((,(concat (file-name-as-directory org-directory) "Todo.org") :maxlevel . 1)))

(add-hook 'org-mode-hook (lambda ()
    (electric-pair-mode)
    (org-indent-mode)
    (flyspell-mode)))

;; Open file links in current window, rather than new ones
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/config.el#L632
(setf (alist-get 'file org-link-frame-setup) #'find-file)

(use-package org-bullets
  :straight t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-roam
  :straight t
  :custom
  (setq org-roam-completion-everywhere t)
  (org-roam-directory (concat (file-name-as-directory org-directory) "Roam"))
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-mode)
  (aa/org-agenda-files)
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :straight t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-download
  :straight t
  :config
  (setq-default org-download-image-dir (concat (file-name-as-directory org-directory) "Attachments")
                org-download-heading-lvl nil))

(use-package languagetool
  :straight t
  :config
  (setq languagetool-correction-language "en-GB"
        languagetool-server-url "https://api.languagetool.org"
        languagetool-server-port 443))

;;; Completion

(use-package company
  :straight t
  :config
  (setq completion-ignore-case t)
  (setq company-idle-delay 0.4)
  (add-to-list 'company-backends 'company-capf)
  (global-company-mode 1))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package ivy
  :straight t
  :diminish
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :straight t
  :config
  (counsel-mode 1))

;;; Evil

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)

  ;; Set window movement to CTRL hjkl to emulate tmux vim interaction
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-j") 'evil-window-down)
  (global-set-key (kbd "C-l") 'evil-window-right)
  (global-set-key (kbd "C-k") 'evil-window-up)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :straight t
  :after evil org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme)
  (evil-org-agenda-set-keys))

;; Ensure org-agenda starts in evil-normal-state
(defun aa/org-agenda-setup ()
  "Start org-agenda in evil-normal-state."
  (evil-org-mode)
  (evil-set-initial-state 'org-agenda-mode 'normal))

(add-hook 'org-agenda-mode-hook 'aa/org-agenda-setup)

;;; Email

(use-package counsel-notmuch
  :straight t
  :defer t)

(use-package notmuch
  :straight t
  :config
  (setq notmuch-command "~/.local/bin/notmuch-remote")
  (setq notmuch-saved-searches '((:name "inbox"
                                  :key "i"
                                  :query "query:inbox"
                                  :sort-order oldest-first))))

(defun aa/notmuch-open-github-link ()
  "Open the first GitHub Pull Request or Issue link in the current notmuch message or thread.
Works in both `notmuch-show-mode` and `notmuch-search-mode`."
  (interactive)
  (let (url (original-buffer (current-buffer)))
    (cond
     ;; Handle notmuch-show-mode (email message view)
     ((eq major-mode 'notmuch-show-mode)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "https://github\\.com/[a-zA-Z0-9_.-]+/[a-zA-Z0-9_.-]+/\\(pull\\|issues\\)/[0-9]+" nil t)
            (setq url (match-string 0))
          (message "No GitHub link found in the current message."))))
     ;; Handle notmuch-search-mode (search list view)
     ((eq major-mode 'notmuch-search-mode)
      (let ((message-id (notmuch-search-find-thread-id)))
        (when message-id
          (save-window-excursion
            (with-temp-buffer
              (notmuch-show message-id nil nil)
              (goto-char (point-min))
              (if (re-search-forward "https://github\\.com/[a-zA-Z0-9_.-]+/[a-zA-Z0-9_.-]+/\\(pull\\|issues\\)/[0-9]+" nil t)
                  (setq url (match-string 0))
                (message "No GitHub link found in the thread."))))))))
    ;; Open the URL in the browser
    (if url
        (browse-url url)
      (message "No GitHub link found."))))


(evil-define-key 'normal notmuch-search-mode-map
  "d" 'aa/notmuch-search-delete
  "b" 'aa/notmuch-open-github-link)

(evil-define-key 'visual notmuch-search-mode-map
  "d" 'aa/notmuch-search-delete
  "b" 'aa/notmuch-open-github-link)

(evil-define-key 'normal notmuch-show-mode-map
  "b" 'aa/notmuch-open-github-link)

(defun aa/notmuch-search-inbox ()
  "Helper function to search for the inbox messages (oldest message first).
  This query is defined in the notmuch config to make it easier to search for
  inbox messages."
  (interactive)
  (notmuch-search "query:inbox" t))

(defun aa/notmuch-search-delete ()
  "When deleting messages we want to also make them unread. When a new message
  comes in the thread notmuch will show you all the messages, this makes it
  easier to see what is new."
  (interactive)
  (notmuch-search-tag (list "+deleted" "-inbox" "-unread"))
  (notmuch-tree-next-message))

(defun org-notmuch-open (id)
  "Visit the notmuch message or thread with id ID."
  (notmuch-show id))

(defun org-notmuch-store-link ()
  "Store a link to a notmuch mail message."
  (cl-case major-mode
    ('notmuch-show-mode
     ;; Store link to the current message
     (let* ((id (notmuch-show-get-message-id))
	    (link (concat "notmuch:" id))
	    (description (format "Mail: %s" (notmuch-show-get-subject))))
       (org-store-link-props
	:type "notmuch"
	:link link
	:description description)))
    ('notmuch-search-mode
     ;; Store link to the thread on the current line
     (let* ((id (notmuch-search-find-thread-id))
	    (link (concat "notmuch:" id))
	    (description (format "Mail: %s" (notmuch-search-find-subject))))
       (org-store-link-props
	:type "notmuch"
	:link link
	:description description)))))

(org-link-set-parameters
  "notmuch"
  :follow 'org-notmuch-open
  :store 'org-notmuch-store-link)

(defun aa/notmuch-show-view-as-patch ()
  "View the the current message as a patch.

   See: https://notmuchmail.org/emacstips"
  (interactive)
  (let* ((id (notmuch-show-get-message-id))
         (msg (notmuch-show-get-message-properties))
         (part (notmuch-show-get-part-properties))
         (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
         (diff-default-read-only t)
         (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
         (map (make-sparse-keymap)))
    (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert subject)
      (insert (notmuch-get-bodypart-text msg part nil)))
    (set-buffer-modified-p nil)
    (diff-mode)
    (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
                 (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
    (goto-char (point-min))))

(use-package mbwatch
  :load-path "~/.emacs.d/mbwatch"
  :after notmuch
  :config
  (mbwatch-mode 1)
  (add-hook 'mbwatch-output-hook
          (lambda (account mailbox)
            (message "[mbwatch] new messages for account %s in mailbox %s" account mailbox)
            (notmuch-command-to-string "new"))))

;;; Sending email
(use-package org-mime
  :straight t)

;; sendmail-program
(setq send-mail-function 'sendmail-send-it
      sendmail-program "mailsend")

;;; Keybindings

(use-package general
  :straight t
  :config
  (general-create-definer aa/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (aa/leader-keys
   ;; Global bindings
    "/"   '(aa/search-org-roam-notes :which-key "Search Org Roam")
    "TAB" '(evil-switch-to-windows-last-buffer :which-key "Last Buffer")
    "SPC" '(counsel-M-x :which-key "M-x")

    ;; Jumping in a buffer
    "j"  '(:ignore t :which-key "Jumps")
    "jj" 'evil-avy-goto-char-timer
    "jl" 'evil-avy-goto-line

    ;; Searching
    "s"  '(:ignore t :which-key "Searching")
    "ss" 'swiper
    "sS" 'swiper-thing-at-point
    "sb" 'swiper-all
    "sB" 'swiper-all-thing-at-point

    ;; Windows. Just rebind all of the evil window bindings to "SPC w"
    ;; so we dont have to keep hitting "CTRL-w"
    "w"  '(evil-window-map :which-key "Windows")
    "wd" 'evil-window-delete

    ;; Org Mode
    "o"  '(evil-window-map :which-key "Org Mode")
    "oa" 'org-agenda
    "oc" 'org-roam-dailies-capture-today
    "ot" 'org-roam-dailies-goto-today
    "of" 'org-roam-node-find
    "or" 'org-refile
    "ol" 'org-roam-link-replace-all

    ;; Files
    "f"  '(:ignore t :which-key "Files")
    "fs" 'save-buffer
    "ff" 'counsel-find-file

    ;; Buffers
    "b"  '(:ignore t :which-key "Buffers")
    "bd" 'kill-this-buffer
    "bb" 'counsel-switch-buffer

    "e"  '(:ignore t :which-key "Email")
    "ee" 'notmuch
    "ei" 'aa/notmuch-search-inbox
    "ej" 'notmuch-jump-search
    "es" 'counsel-notmuch

    ;; Toggles
    "t"  '(:ignore t :which-key "Toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))
