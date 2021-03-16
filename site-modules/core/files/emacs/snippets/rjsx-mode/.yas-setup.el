
;; Capitalise the first letter in a string
(defun aa/yas/rjsx/capitalize-first-cha (&optional string)
  "Capitalise only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))
