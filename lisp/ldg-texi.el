(defvar ledger-path "/Users/johnw/bin/ledger")
(defvar ledger-sample-doc-path "/Users/johnw/src/ledger/doc/sample.dat")
(defvar ledger-normalization-args "--args-only --columns 80")

(defun ledger-texi-expand-examples ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^@c \\(\\(?:small\\)?example\\): \\(.*\\)" nil t)
      (let ((section (match-string 1))
	    (command (match-string 2))
	    (data-file ledger-sample-doc-path))
	(goto-char (match-end 0))
	(forward-line)
	(when (looking-at "@\\(\\(?:small\\)?example\\)")
	  (let ((beg (point)))
	    (re-search-forward "^@end \\(\\(?:small\\)?example\\)")
	    (delete-region beg (1+ (point)))))

	(when (let ((case-fold-search nil))
		(string-match " -f \\$\\([-a-z]+\\)" command))
	  (let ((label (match-string 1 command)))
	    (setq command (replace-match "" t t command)
		  data-file (expand-file-name (format "%s.dat" label)
					      temporary-file-directory))
	    (save-excursion
	      (goto-char (point-min))
	      (search-forward (format "@c data: %s" label))
	      (re-search-forward "@\\(\\(?:small\\)?example\\)")
	      (forward-line)
	      (let ((beg (point))
		    content)
		(re-search-forward "@end \\(\\(?:small\\)?example\\)")
		(setq content (buffer-substring-no-properties
			       beg (match-beginning 0)))
		(with-current-buffer (find-file-noselect data-file)
		  (erase-buffer)
		  (insert content)
		  (save-buffer))))))

	(if (string-match "\\$LEDGER" command)
	    (setq command
		  (replace-match
		   (format "%s -f \"%s\" %s" ledger-path
			   data-file ledger-normalization-args)
		   t t command)))

	(save-restriction
	  (narrow-to-region (point) (point))
	  (shell-command command t (get-buffer-create " *ldg-texi*"))
	  (if (= (point-min) (point-max))
	      (progn
		(push-mark nil t)
		(message "Command '%s' yielded no result at %d"
			 command (point))
		(ding))
	    (goto-char (point-min))
	    (insert "@" section ?\n)
	    (goto-char (point-max))
	    (unless (eolp)
	      (insert ?\n))
	    (insert "@end " section ?\n)))))))
