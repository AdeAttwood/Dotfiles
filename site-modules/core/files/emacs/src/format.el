;;; format.el --- AMACS -*- lexical-binding: t -*-
;;
;; Copyright 2021 Practically.io All rights reserved
;;
;; Use of this source is governed by a BSD-style
;; licence that can be found in the LICENCE file or at
;; https://www.practically.io/copyright/

(defun fmt--goto-line (line)
  "Move cursor to line LINE."
  (goto-char (point-min))
    (forward-line (1- line)))

(defcustom fmt-show-errors 'buffer
    "Where to display prettier error output.
It can either be displayed in its own buffer, in the echo area, or not at all.
Please note that Emacs outputs to the echo area when writing
files and will overwrite prettier's echo output if used from inside
a `before-save-hook'."
    :type '(choice
            (const :tag "Own buffer" buffer)
            (const :tag "Echo area" echo)
            (const :tag "None" nil))
      :group 'fmt)

(defun fmt--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in fmt--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (fmt--goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (let ((beg (point)))
                  (forward-line len)
                  (delete-region (point) beg))))
             (t
              (error "Invalid rcs patch or internal error in fmt--apply-rcs-patch")))))))))

(defun fmt--process-errors (filename errorfile errbuf)
  "Process errors for FILENAME, using an ERRORFILE and display the output in ERRBUF."
  (with-current-buffer errbuf
    (if (eq fmt-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (fmt--kill-error-buffer errbuf))
      (insert-file-contents errorfile nil nil nil)
      ;; Convert the prettier stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "fmt errors:\n")
      (while (search-forward-regexp "^stdin" nil t)
        (replace-match (file-name-nondirectory filename)))
      (compilation-mode)
      (display-buffer errbuf))))

(defun fmt--kill-error-buffer (errbuf)
  "Kill buffer ERRBUF."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (with-current-buffer errbuf
        (erase-buffer))
      (kill-buffer errbuf))))

(defun fmt-buffer ()
   "Format the current buffer according to the fmt tool."
   (interactive)
   (let* ((ext (file-name-extension buffer-file-name t))
          (bufferfile (make-temp-file "fmt" nil ext))
          (outputfile (make-temp-file "fmt" nil ext))
          (errorfile (make-temp-file "fmt" nil ext))
          (errbuf (if fmt-show-errors (get-buffer-create "*fmt errors*")))
          (patchbuf (get-buffer-create "*fmt patch*"))
          (coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))
     (unwind-protect
         (save-restriction
           (widen)
           (write-region nil nil bufferfile)
           (if errbuf
               (with-current-buffer errbuf
                 (setq buffer-read-only nil)
                 (erase-buffer)))
           (with-current-buffer patchbuf
             (erase-buffer))
           (if (zerop (apply 'call-process "fmtcli" bufferfile (list (list :file outputfile) errorfile) nil (list "-input" bufferfile "-formatting_file" buffer-file-name)))
               (progn
                 (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "--strip-trailing-cr" "-"
                                      outputfile)
                 (fmt--apply-rcs-patch patchbuf)
                 (if errbuf (fmt--kill-error-buffer errbuf)))
             (message "Could not apply fmt")
             (if errbuf
                 (fmt--process-errors (buffer-file-name) errorfile errbuf))))
       (kill-buffer patchbuf)
       (delete-file errorfile)
       (delete-file bufferfile)
       (delete-file outputfile))))

;;;###autoload
(define-minor-mode fmt-mode
  "Runs fmt on file save when this mode is turned on"
  :lighter " fmt"
  :global nil
  (if fmt-mode
      (add-hook 'before-save-hook 'fmt-buffer nil 'local)
    (remove-hook 'before-save-hook 'fmt-buffer 'local)))

(define-globalized-minor-mode global-fmt-mode fmt-mode
  (lambda () (fmt-mode 1)))
