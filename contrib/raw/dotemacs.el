;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ledger

;; Maybe later add this to the expense repo once it settles
(add-to-list 'load-path "/home/adamsrl/.emacs.d/addons/ledger")

(add-to-list 'load-path "/home/adamsrl/AdamsInfoServ/BusinessDocuments/Ledger/AdamsRussell/bin")
(autoload 'ledger-mode "ldg-new" nil t)
(add-to-list 'auto-mode-alist '("\\.dat$" . ledger-mode))

(add-hook 'ledger-mode-hook
          (lambda ()
            (setq truncate-lines 1)
            (url-handler-mode 1)        ; Enable hyperlinks
            (require 'ledger-matching)  ; Requires ldg-report anyway
            (load-file "/home/adamsrl/.emacs.d/addons/ledger/ldg-xact.el")
            (let ((map (current-local-map)))
              (define-key map (kbd "\C-c o") 'find-file-at-point) ; Open images
              (define-key map (kbd "<f8>")   'ledger-expense-shortcut)
              (define-key map (kbd "M-i")    'ledger-expense-internal)
              (define-key map (kbd "M-o")    'ledger-expense-personal)
              (define-key map (kbd "M-'")    'ledger-expense-split)
              (define-key map (kbd "M-n")    '(lambda ()
                                                (interactive)
                                                (ledger-post-next-xact)
                                                (recenter)
                                                (when (get-buffer "*Receipt*")
                                                  (ledger-expense-show-receipt))))
              (define-key map (kbd "M-p")    '(lambda () (interactive)
                                                (ledger-post-prev-xact)
                                                (recenter)
                                                (when (get-buffer "*Receipt*")
                                                  (ledger-expense-show-receipt))))
              (local-unset-key [tab]) ; Ideally this turns off pcomplete
              (local-unset-key [(control ?i)]) ; Ideally this turns off pcomplete
              )

                                        ;(defface ledger-report-face-account-ok  '((t (:foreground "Cyan"))) "Derp")
                                        ;(defface ledger-report-face-account-bad '((t (:foreground "Red")))  "Derp")

            (font-lock-add-keywords
             'ledger-mode
             '(("Unassigned\\|Unknown\\|; RECEIPT:$" 0 'highlight prepend))) ))

;; My customizations to make receipt image matching work with ledger-report mode
(add-hook 'ledger-report-mode-hook
          (lambda ()
            (hl-line-mode 1)
            (local-set-key (kbd "<RET>") 'ledger-report-visit-source) ; Make return jump to the right txn
            (local-set-key (kbd "<tab>") 'ledger-report-visit-source) ; Make tab jump to the right txn
            (local-set-key (kbd "n") '(lambda ()
                                        (interactive)
                                        (save-selected-window
                                          (next-line)
                                          (ledger-report-visit-source)))) ; Update a txn window but keep focus
            (local-set-key (kbd "p") '(lambda ()
                                        (interactive)
                                        (save-selected-window
                                          (previous-line)
                                          (ledger-report-visit-source)))) ; Update a txn window but keep focus


            (local-set-key (kbd "M-r") 'ledger-receipt-matching) ; Link receipt to current item
            (local-set-key (kbd "M-l") 'ledger-matching-tie-receipt-to-txn) ; Link receipt to current item
            (local-set-key (kbd "M-n") '(lambda ()
                                          (interactive)
                                          (ledger-matching-image-offset-adjust  1))) ; Next receipt image
            (local-set-key (kbd "M-p") '(lambda ()
                                          (interactive)
                                          (ledger-matching-image-offset-adjust -1))) ; prev receipt image
            (local-set-key (kbd "M-s") '(lambda ()
                                          (interactive)
                                          (ledger-receipt-skip))) ; Skip receipt image
            (local-set-key (kbd "C-c C-e")  '(lambda () (interactive)
                                               (save-selected-window
                                                 (ledger-report-visit-source)
                                                 (ledger-toggle-current-entry) ))) ; Toggle entry
            ))

(defvar *ledger-expense-shortcut-ER*
  "Current expense report number, just last four digits (ie: 1234 results in AISER1234).")

(defvar *ledger-expense-shortcut-split-ER*
  "Split (ie: internal) expense report number, just last four digits (ie: 1234 results in AISER1234).")

(defvar *ledger-expense-shortcut-Proj* ""
  "Current export report project code (ie: AGIL1292)")

(defun ledger-expense-shortcut-ER-format-specifier () *ledger-expense-shortcut-ER*)

(defun ledger-expense-shortcut-setup (ER Split Proj)
  "Sets the variables expanded into the transaction."
  (interactive "MER Number (4 digit number only): \nMSplit ER Number (4 digit number only): \nMProject: ")
  (setq *ledger-expense-shortcut-ER*
        (concatenate 'string "AISER" ER))
  (setq *ledger-expense-shortcut-split-ER*
        (concatenate 'string "AISER" Split))
  (setq *ledger-expense-shortcut-Proj* Proj)
  (setq ledger-matching-project Proj)
  (message "Set Proj to %s and ER to %s, split to %s"
           *ledger-expense-shortcut-Proj*
           *ledger-expense-shortcut-ER*
           *ledger-expense-shortcut-split-ER*))

(defun ledger-expense-shortcut ()
  "Updates the ER and Project metadata with the current values of the shortcut variables."
 (interactive)
 (when (eq major-mode 'ledger-mode)
   (if (or (eql *ledger-expense-shortcut-ER* "")
           (eql *ledger-expense-shortcut-Proj* ""))
         (message "Run ledger-expense-shortcut-setup first.")
     (save-excursion
       (search-forward "; ER:")
       (kill-line nil)
       (insert " " *ledger-expense-shortcut-ER*))
     (save-excursion
       (search-forward "; PROJECT:")
       (kill-line nil)
       (insert " " *ledger-expense-shortcut-Proj*)))))

(defun ledger-expense-split ()
  "Splits the current transaction between internal and projects."
  (interactive)
  (when (eq major-mode 'ledger-mode) ; I made this local now, should only trigger in ldg-mode
    (save-excursion
      (end-of-line)
      (re-search-backward "^[0-9]\\{4\\}/")
      (re-search-forward "^ +Dest:Projects")
      (move-beginning-of-line nil)
      (let ((begin (point))
            (end (re-search-forward "^$")))
        (goto-char end)
        (insert (buffer-substring begin end))
        (goto-char end)
        (re-search-forward "^    Dest:Projects")
        (replace-match "    Dest:Internal")
        (re-search-forward "; ER: +[A-Za-z0-9]+")
        (replace-match (concat "; ER: " *ledger-expense-shortcut-split-ER*)  t)
        (when (re-search-forward "; CATEGORY: Meals" (save-excursion (re-search-forward "^$")) t)
          (replace-match "; CATEGORY: Travel" t))))
    (re-search-backward "^[0-9]\\{4\\}/")
    (re-search-forward "^ +Dest:Projects")
    (insert-string "                                     $") ))

(defun ledger-expense-internal ()
  "Makes the expense an internal one."
  (interactive)
  (when (eq major-mode 'ledger-mode) ; I made this local now, should only trigger in ldg-mode
    (save-excursion
      (end-of-line)
      (re-search-backward "^[0-9]\\{4\\}/")
      (let ((begin (point))
            (end (save-excursion (re-search-forward "^$"))))
        (when (re-search-forward "^    Dest:Projects" end t)
          (replace-match "    Dest:Internal") )
        (when (re-search-forward "; CATEGORY: Meals" (save-excursion (re-search-forward "^$")) t)
          (replace-match "; CATEGORY: Travel" t))))))

(defun ledger-expense-personal ()
  "Makes the expense an personal one, eliminating metadata and receipts."
 (interactive)
 (when (eq major-mode 'ledger-mode) ; I made this local now, should only trigger in ldg-mode
   (save-excursion
     (end-of-line)
     (re-search-backward "^[0-9]\\{4\\}/")
     (let ((begin (point))
           (end (save-excursion (re-search-forward "^$"))))
       (when (re-search-forward "^    Dest:Projects" end t)
         (replace-match "    Other:Personal"))
       (goto-char begin)
       (save-excursion
         (when (re-search-forward "^ +; ER:" end t)
         (beginning-of-line)
         (kill-line 1)))
       (save-excursion
         (when (re-search-forward "^ +; PROJECT:" end t)
         (beginning-of-line)
         (kill-line 1)))
       (save-excursion
         (when (re-search-forward "^ +; CATEGORY:" end t)
         (beginning-of-line)
         (kill-line 1)))
       (save-excursion
         (when (re-search-forward "^ +; RECEIPT:" end t)
         (beginning-of-line)
         (kill-line 1)))
       (ledger-toggle-current-entry)))))

(defun ledger-expense-show-receipt ()
  "Uses the Receipt buffer to show the receipt of the txn we're on."
  (when (eq major-mode 'ledger-mode) ; I made this local now, should only trigger in ldg-mode
    (save-excursion
      (end-of-line)
      (re-search-backward "^[0-9]\\{4\\}/")
      (let ((begin (point))
            (end (save-excursion (re-search-forward "^$"))))
        (save-excursion
          (when (re-search-forward "^\\( +; RECEIPT: +\\)\\([^,]+?.jpg\\).*$" end t)
            (ledger-matching-display-image
             (concat "/home/adamsrl/AdamsInfoServ/BusinessDocuments/Ledger/AdamsRussell/"
                     (match-string 2))) ))))))
