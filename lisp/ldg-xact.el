;; A sample entry sorting function, which works if entry dates are of
;; the form YYYY/mm/dd.

(defun ledger-sort ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (sort-subr
     nil
     (function
      (lambda ()
        (if (re-search-forward
             (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
                     "\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)") nil t)
            (goto-char (match-beginning 0))
          (goto-char (point-max)))))
     (function
      (lambda ()
        (forward-paragraph))))))

