;; mbwatch-mode.el --- Run mbwatch and handle its output -*- lexical-binding: t; -*-

(defvar mbwatch-process nil
  "The process object for the mbwatch command.")

(defvar mbwatch-output-hook nil
  "Hook run when new output is received from the mbwatch process. Each function
  in this hook is called with a single argument, the new output.")

(defun mbwatch-start ()
  "Start the mbwatch process."
  (let ((process-buffer (get-buffer-create "*mbwatch*")))
    (with-current-buffer process-buffer
      (read-only-mode -1)
      (erase-buffer)
      (read-only-mode 1))

    (setq mbwatch-process
          (start-process-shell-command
           "mbwatch" process-buffer "mbwatch"))

    (set-process-filter
     mbwatch-process
     'mbwatch-process-filter)
    (message "mbwatch process started.")))

(defun mbwatch-stop ()
  "Stop the mbwatch process."
  (interactive)
  (when (and mbwatch-process (process-live-p mbwatch-process))
    (kill-process mbwatch-process)
    (setq mbwatch-process nil)))


(defun mbwatch-process-filter (proc output)
  "Filter function for mbwatch process output.
PROC is the process. OUTPUT is the new output."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert output))
    (mbwatch-handle-output output)))

(defun mbwatch-handle-output (output)
  "Handle new OUTPUT from the mbwatch process."
  (when (string-match "Synced changes for \\(.*?\\) in mailbox \\(.*?\\)$" output)
    (let ((account (match-string 1 output))
          (mailbox (match-string 2 output)))
      (run-hook-with-args 'mbwatch-output-hook account mailbox))))

(define-minor-mode mbwatch-mode
  "Minor mode to run mbwatch command and handle its output."
  :lighter " mbwatch"
  :global t
  (if mbwatch-mode
      (mbwatch-start)
    (mbwatch-stop)))

(provide 'mbwatch)

;;; mbwatch-mode.el ends here
