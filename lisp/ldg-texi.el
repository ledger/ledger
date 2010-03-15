(defvar ledger-path "/Users/johnw/bin/ledger")
(defvar ledger-sample-doc-path "/Users/johnw/src/ledger/doc/sample.dat")
(defvar ledger-normalization-args "--args-only --columns 80")

(defun ledger-texi-update-examples ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^@c \\(\\(?:sm\\)?ex\\) \\(\\S-+\\): \\(.*\\)" nil t)
      (let ((section (match-string 1))
	    (example-name (match-string 2))
	    (command (match-string 3)) expanded-command
	    (data-file ledger-sample-doc-path)
	    input output)
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
	      (let ((beg (point)))
		(re-search-forward "@end \\(\\(?:small\\)?example\\)")
		(setq input (buffer-substring-no-properties
			       beg (match-beginning 0)))
		(with-current-buffer (find-file-noselect data-file)
		  (erase-buffer)
		  (insert input)
		  (save-buffer))))))

	(setq expanded-command command)
	(if (string-match "\\$LEDGER" expanded-command)
	    (setq expanded-command
		  (replace-match
		   (format "%s -f \"%s\" %s" ledger-path
			   data-file ledger-normalization-args)
		   t t expanded-command)))

	(save-restriction
	  (narrow-to-region (point) (point))
	  (shell-command expanded-command t (get-buffer-create " *ldg-texi*"))
	  (if (= (point-min) (point-max))
	      (progn
		(push-mark nil t)
		(message "Command '%s' yielded no result at %d"
			 expanded-command (point))
		(ding))
	    (setq output (buffer-string))
	    (goto-char (point-min))
	    (let ((section-name (if (string= section "smex")
				    "smallexample"
				  "example")))
	      (insert "@" section-name ?\n)
	      (goto-char (point-max))
	      (unless (eolp)
		(insert ?\n))
	      (insert "@end " section-name ?\n))))

	;; Update the regression test associated with this example
	
	(with-current-buffer
	    (find-file-noselect
	     (expand-file-name (concat example-name ".test")
			       "../test/manual"))
	  (erase-buffer)
	  (let ((case-fold-search nil))
	    (if (string-match "\\$LEDGER\\s-+" command)
		(setq command (replace-match "" t t command)))
	    (if (string-match " -f \\$\\([-a-z]+\\)" command)
		(setq command (replace-match "" t t command))))

	  (insert command ?\n)
	  (insert "<<<" ?\n)
	  (insert input)
	  (insert ">>>1" ?\n)
	  (insert output)
	  (insert ">>>2" ?\n)
	  (insert "=== 0" ?\n)
	  (save-buffer)
	  (kill-buffer (current-buffer)))))))

(provide 'ldg-texi)
