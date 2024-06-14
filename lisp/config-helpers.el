;;; config-helpers -- Weird one-off helper functions

;;; Commentary:

;;; Code:

(cl-defun --completing-read (prompt collection
                                    &key predicate require-match
                                    initial-input history default-value
                                    inherit-input-method)
  "Wrapper around `completing-read' that allow the use of keywords."
  ;; if the collection is a list and there's only one element, just return that
  ;; and skip the fanfare.
  (if (and require-match
           (seqp collection)
           (length= collection 1))
    (car collection)
    (completing-read prompt collection
                     predicate
                     require-match
                     initial-input
                     history
                     default-value
                     inherit-input-method)))

(cl-defun --thing-at-point-or-region-or-user-input ()
  "Prompt the user for input, defaulting to symbol at point if none."
  (interactive)
  (require 'thingatpt)
  (read-string "String: "
               (cond ((use-region-p)
                      (buffer-substring-no-properties
                       (use-region-beginning)
                       (use-region-end)))
                     (t (thing-at-point 'symbol)))))

(cl-defun --convert-to-buffer (buf)
  "Normalizes BUF into a buffer, returning the buffer value."
  (let ((buffer (cond
                 ((stringp buf) (find-file-noselect buf))
                 ((or (symbol-function buf)
                      (functionp buf)) (funcall buf))
                 ((bufferp buf) buf)
                 (t (user-error "Buf is neither string, buffer nor fn! It is %s" buf)))))
    (cl-assert (bufferp buffer))
    buffer))

(cl-defun --read-word-list (&optional &key (prompt "Words")
                                      (func #'identity)
                                      (split-separator ",")
                                      (join-separator ", "))
  "Reads a comma-separted input list and normalizes it.

If FUNC is passed in then run func for each string in the list.

Finally, concatenate the results."
  (interactive)
  (require 's)
  (--> (read-string (format "%s [optional, \"%s\"-separated]: "
                            prompt split-separator))
      (s-split split-separator it :omit-nulls)
      (cl-remove-if #'s-blank-str-p it)
      (mapcar #'s-trim it)
      (mapcar (-partial #'s-replace " " "-") it)
      (mapcar #'s-downcase it)
      (mapcar func it)
      (s-join join-separator it)))

;; The best feature of notepad
(defun --make-iso-8601-timestamp (with-hours)
  (insert (format-time-string (if with-hours "%FT%H%M" "%F"))))

(with-eval-after-load 'general
  (general-define-key
   :states '(normal motion insert)
    "<f5>" #'(lambda () (interactive)
               (--make-iso-8601-timestamp nil))
    "S-<f5>" #'(lambda () (interactive)
                 (--make-iso-8601-timestamp t))
   (general-define-key
    :states '(normal)
    "gb" #'(lambda () (interactive)
             (--make-iso-8601-timestamp nil))
    "gB" #'(lambda () (interactive)
             (--make-iso-8601-timestamp t)))))


(provide 'config-helpers)

;;; config-helpers.el ends here
