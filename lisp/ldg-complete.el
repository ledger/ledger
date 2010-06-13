;;(require 'esh-util)
;;(require 'esh-arg)
(require 'pcomplete)

;; In-place completion support

(defun ledger-thing-at-point ()
  (let ((here (point)))
    (goto-char (line-beginning-position))
    (cond ((looking-at "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.+?)\\)?\\s-+")
           (goto-char (match-end 0))
           'entry)
          ((looking-at "^\\s-+\\([*!]\\s-+\\)?[[(]?\\(.\\)")
           (goto-char (match-beginning 2))
           'transaction)
          ((looking-at "^\\(sun\\|mon\\|tue\\|wed\\|thu\\|fri\\|sat\\)\\s-+")
           (goto-char (match-end 0))
           'entry)
          (t
           (ignore (goto-char here))))))

(defun ledger-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  (let* ((info (save-excursion
                 (cons (ledger-thing-at-point) (point))))
         (begin (cdr info))
         (end (point))
         begins args)
    (save-excursion
      (goto-char begin)
      (when (< (point) end)
        (skip-chars-forward " \t\n")
        (setq begins (cons (point) begins))
        (setq args (cons (buffer-substring-no-properties
                          (car begins) end)
                         args)))
      (cons (reverse args) (reverse begins)))))

(defun ledger-entries ()
  (let ((origin (point))
        entries-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
                      "\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)") nil t)
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (setq entries-list (cons (match-string-no-properties 3)
                                   entries-list)))))
    (pcomplete-uniqify-list (nreverse entries-list))))

(defvar ledger-account-tree nil)

(defun ledger-find-accounts ()
  (let ((origin (point)) account-path elements)
    (save-excursion
      (setq ledger-account-tree (list t))
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]+\\([*!]\\s-+\\)?[[(]?\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)" nil t)
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (setq account-path (match-string-no-properties 2))
          (setq elements (split-string account-path ":"))
          (let ((root ledger-account-tree))
            (while elements
              (let ((entry (assoc (car elements) root)))
                (if entry
                    (setq root (cdr entry))
                  (setq entry (cons (car elements) (list t)))
                  (nconc root (list entry))
                  (setq root (cdr entry))))
              (setq elements (cdr elements)))))))))

(defun ledger-accounts ()
  (ledger-find-accounts)
  (let* ((current (caar (ledger-parse-arguments)))
         (elements (and current (split-string current ":")))
         (root ledger-account-tree)
         (prefix nil))
    (while (cdr elements)
      (let ((entry (assoc (car elements) root)))
        (if entry
            (setq prefix (concat prefix (and prefix ":")
                                 (car elements))
                  root (cdr entry))
          (setq root nil elements nil)))
      (setq elements (cdr elements)))
    (and root
         (sort
          (mapcar (function
                   (lambda (x)
                     (let ((term (if prefix
                                     (concat prefix ":" (car x))
                                   (car x))))
                       (if (> (length (cdr x)) 1)
                           (concat term ":")
                         term))))
                  (cdr root))
          'string-lessp))))

(defun ledger-complete-at-point ()
  "Do appropriate completion for the thing at point"
  (interactive)
  (while (pcomplete-here
          (if (eq (save-excursion
                    (ledger-thing-at-point)) 'entry)
              (if (null current-prefix-arg)
                  (ledger-entries)  ; this completes against entry names
                (progn
                  (let ((text (buffer-substring (line-beginning-position)
                                                (line-end-position))))
                    (delete-region (line-beginning-position)
                                   (line-end-position))
                    (condition-case err
                        (ledger-add-entry text t)
                      ((error)
                       (insert text))))
                  (forward-line)
                  (goto-char (line-end-position))
                  (search-backward ";" (line-beginning-position) t)
                  (skip-chars-backward " \t0123456789.,")
                  (throw 'pcompleted t)))
            (ledger-accounts)))))

(defun ledger-fully-complete-entry ()
  "Do appropriate completion for the thing at point"
  (interactive)
  (let ((name (caar (ledger-parse-arguments)))
        xacts)
    (save-excursion
      (when (eq 'entry (ledger-thing-at-point))
        (when (re-search-backward
               (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
                       (regexp-quote name) "\\(\t\\|\n\\| [ \t]\\)") nil t)
          (forward-line)
          (while (looking-at "^\\s-+")
            (setq xacts (cons (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))
                              xacts))
            (forward-line))
          (setq xacts (nreverse xacts)))))
    (when xacts
      (save-excursion
        (insert ?\n)
        (while xacts
          (insert (car xacts) ?\n)
          (setq xacts (cdr xacts))))
      (forward-line)
      (goto-char (line-end-position))
      (if (re-search-backward "\\(\t\\| [ \t]\\)" nil t)
          (goto-char (match-end 0))))))

(provide 'ldg-complete)
