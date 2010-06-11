(require 'ldg-post)
(require 'ldg-state)

(defgroup ledger-register nil
  ""
  :group 'ledger)

(defcustom ledger-register-date-format "%m/%d/%y"
  "*The date format used for ledger register reports."
  :type 'string
  :group 'ledger-register)

(defcustom ledger-register-line-format "%s %-30.30s %-25.25s %15s\n"
  "*The date format used for ledger register reports."
  :type 'string
  :group 'ledger-register)

(defface ledger-register-pending-face
  '((((background light)) (:weight bold))
    (((background dark)) (:weight bold)))
  "Face used to highlight pending entries in a register report."
  :group 'ledger-register)

(defun ledger-register-render (data-buffer posts)
  (dolist (post posts)
    (let ((index 1))
      (dolist (xact (nthcdr 5 post))
        (let ((beg (point))
              (where
               (with-current-buffer data-buffer
                 (cons
                  (nth 0 post)
                  (if ledger-clear-whole-entries
                      (save-excursion
                        (goto-line (nth 1 post))
                        (point-marker))
                    (save-excursion
                      (goto-line (nth 0 xact))
                      (point-marker)))))))
          (insert (format ledger-register-line-format
                          (format-time-string ledger-register-date-format
                                              (nth 2 post))
                          (nth 4 post) (nth 1 xact) (nth 2 xact)))
          (if (nth 3 xact)
              (set-text-properties beg (1- (point))
                                   (list 'face 'ledger-register-pending-face
                                         'where where))
            (set-text-properties beg (1- (point))
                                 (list 'where where))))
        (setq index (1+ index)))))
  (goto-char (point-min))
  )

(defun ledger-register-generate (&optional data-buffer &rest args)
  (let ((buf (or data-buffer (current-buffer))))
    (with-current-buffer (get-buffer-create "*ledger-register*")
      (let ((pos (point))
            (inhibit-read-only t))
        (erase-buffer)
        (ledger-register-render buf (apply #'ledger-exec-read buf args))
        (goto-char pos))
      (set-buffer-modified-p nil)
      (toggle-read-only t)
      (display-buffer (current-buffer) t))))

(provide 'ldg-register)
