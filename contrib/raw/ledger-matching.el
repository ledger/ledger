;; This library is intended to allow me to view a receipt on one panel, and tie it to ledger transactions in another

(require 'ldg-report)

(defgroup ledger-matching nil
  "Ledger image matching")

(defcustom ledger-matching-sourcedir "~/AdamsInfoServ/BusinessDocuments/Ledger/Incoming"
  "Source directory for images to process, ie: the incoming queue of images."
  :group 'ledger-matching)

(defcustom ledger-matching-destdir "~/AdamsInfoServ/BusinessDocuments/Ledger/AdamsRussell/Receipts"
  "Destination directory for images when matched, will still have a project directory appended to it."
  :group 'ledger-matching)

(defcustom ledger-matching-relative-receipt-dir "Receipts"
  "Relative directory root for destination images used in Ledger entries, will have the project directory appended and receipt filename."
  :group 'ledger-matching)

(defcustom ledger-matching-convert-binary "/usr/bin/convert"
  "Path to the Imagemagick convert command."
  :group 'ledger-matching)

(defcustom ledger-matching-scale 50
  "Scaling parameter to Imagemagick's convert to resize an image for viewing."
  :group 'ledger-matching)

(defcustom ledger-matching-rotation 0
  "Rotation parameter to Imagemagick's convert to rotate an image for viewing. Images on disk should always be upright for reading."
  :group 'ledger-matching)


(defconst ledger-matching-image-buffer "*Receipt*"
  "Buffer name we load images into. Created if it doesn't exist, and persists across image loads.")


(defvar ledger-matching-project "Internal"
  "The directory appended to the destination for the project code where receipts will be stored.")

(defvar ledger-matching-image-offset 0
  "The index of the current file from the SORTED source directory contents.")

(defvar ledger-matching-image-name nil
  "The filename only of the current image.")


(defun ledger-matching-display-image (image-filename)
  "Resize the image and load it into our viewing buffer."

  ;; Create our viewing buffer if needed, and set it. Do NOT switch,
  ;; this buffer isn't the primary. Let the user leave it where they
  ;; place it.
  (unless (get-buffer ledger-matching-image-buffer)
    (get-buffer-create ledger-matching-image-buffer))
  (set-buffer ledger-matching-image-buffer)
  (erase-buffer)
  (goto-char (point-min))
  (insert-string image-filename "\n")

  ;; Convert the source to the temporary dest applying resizing and rotation
  (let* ((source (expand-file-name image-filename ledger-matching-sourcedir))
         (dest (make-temp-file "ledger-matching-" nil ".jpg"))
         (result (call-process ledger-matching-convert-binary nil (get-buffer "*Messages*") nil
                               source
                               "-scale" (concat (number-to-string ledger-matching-scale) "%")
                               "-rotate" (number-to-string ledger-matching-rotation)
                               dest)))

    (if (/= 0 result)

        ;; Bomb out if the convert fails
        (message "Error running convert, see *Messages* buffer for details.")

      ;; Insert scaled image into the viewing buffer, replacing
      ;; current contents Temp buffer is to force sync reading into
      ;; memory of the jpeg due to async race condition with display
      ;; and file deletion
      (let ((image (create-image (with-temp-buffer
                                   (insert-file-contents-literally dest)
                                   (string-as-unibyte (buffer-string)))
                                 'jpeg t)))
        (insert-image image)
        (goto-char (point-min))

        ;; Redisplay is required to prevent a race condition between displaying the image and the deletion. Apparently its async.
        ;; Either redisplay or the above string method work, both together can't hurt.
        (redisplay)
        ))

    ;; Delete our temporary file
    (delete-file dest)))



(defun ledger-matching-update-current-image ()
  "Grab the image from the source directory by offset and display"

  (let* ((file-listing (directory-files ledger-matching-sourcedir nil "\.jpg$" nil))
         (len (safe-length file-listing)))

    ;; Ensure our offset doesn't exceed the file list
    (cond ((= len 0)
           (message "No files found in source directory."))

          ((< len 0)
           (message "Error, list of files should never be negative. Epic fail."))

          ((>= ledger-matching-image-offset len)
           (message "Hit end of list. Last image.")
           (setq ledger-matching-image-offset (1- len)))

          ((< ledger-matching-image-offset 0)
           (message "Beginning of list. First image.")
           (setq ledger-matching-image-offset 0)))

    ;; Get the name for the offset
    (setq ledger-matching-image-name (nth ledger-matching-image-offset file-listing))

    (ledger-matching-display-image ledger-matching-image-name)))



(defun ledger-matching-image-offset-adjust (amount)
  "Incr/decr the offset and update the receipt buffer."

  (setq ledger-matching-image-offset (+ ledger-matching-image-offset amount))
  (ledger-matching-update-current-image))



(defun ledger-receipt-matching ()
  "Open the receipt buffer and start with the first image."
  (interactive)
  (setq ledger-matching-image-offset 0)
  (ledger-matching-update-current-image))



(defun ledger-matching-tie-receipt-to-txn ()
  (interactive)
  (save-selected-window
    (ledger-report-visit-source)

    ;; Assumes we're in a narrowed buffer with ONLY this txn
    (backward-paragraph)
    (beginning-of-line)

    ;; ;; Update the ER and Project while I'm there
    ;; (save-excursion
    ;;   (search-forward "; ER:")
    ;;   (kill-line nil)
    ;;   (insert " " *ledger-expense-shortcut-ER*))
    ;; Just do the project for now.
    (save-excursion
      (search-forward "; PROJECT:")
      (kill-line nil)
      (insert " " *ledger-expense-shortcut-Proj*))

    ;; Goto the receipt line, unless their isn't one then add one
    (unless (search-forward "RECEIPT:" nil t)

      ;; Still at date line if that failed
      (next-line)
      (newline)
      (insert-string "    ; RECEIPT:"))

    ;; Point immediately after : on tag

    ;; Check for existing jpg file
    (if (search-forward ".jpg" (line-end-position) t)

        ;; if present make it a comma delimited list
        (insert-string ",")

      ;; otherwise just add a space to pad
      (insert-string " "))

    ;; Add our relative filename as the value of the RECEIPT tag
    (insert-string (concat ledger-matching-relative-receipt-dir "/"
                           ledger-matching-project "/"
                           ledger-matching-image-name))

    ;; Create the destination project dir if it doesn't exist.
    (let ((full-destination (concat ledger-matching-destdir "/" ledger-matching-project )))
      (unless (file-accessible-directory-p full-destination)
        (make-directory full-destination t)))

    ;; Rename the file from the source directory to its permanent home
    (rename-file (concat ledger-matching-sourcedir "/"
                         ledger-matching-image-name)
                 (concat ledger-matching-destdir "/"
                         ledger-matching-project "/"
                         ledger-matching-image-name))

    ;; Update the receipt screen
    (ledger-matching-update-current-image)

    (message "Filed %s to project %s" ledger-matching-image-name ledger-matching-project)))



(defun ledger-receipt-skip ()
  "Move the current image to the Skip directory because its not relevant."

      (rename-file (concat ledger-matching-sourcedir "/"
                         ledger-matching-image-name)
                   (concat ledger-matching-sourcedir "/Skip/"
                           ledger-matching-image-name))

    ;; Update the receipt screen at the same offset
    (ledger-matching-update-current-image))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items below are speed entry macros, and should eventually migrate to their own file.

(defvar *ledger-expense-shortcut-ER*
  "Current expense report number, just last four digits (ie: 1234 results in AISER1234).")

(defvar *ledger-expense-shortcut-split-ER*
  "Split (ie: internal) expense report number, just last four digits (ie: 1234 results in AISER1234).")

(defvar *ledger-expense-shortcut-Proj* ""
  "Current export report project code (ie: AGIL1292)")

(defun ledger-expense-shortcut-ER-format-specifier () *ledger-expense-shortcut-ER*)

(defun ledger-expense-shortcut-Project-format-specifier () *ledger-expense-shortcut-Proj*)

(defun ledger-expense-shortcut-setup (ER Split Proj)
  "Sets the variables expanded into the transaction."
  (interactive "MER Number (ER or IN and 4 digit number only): \nMSplit ER Number (ER or IN and 4 digit number only): \nMProject: ")
  (setq *ledger-expense-shortcut-ER*
        (concatenate 'string "AIS" ER))
  (setq *ledger-expense-shortcut-split-ER*
        (concatenate 'string "AIS" Split))
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


(provide 'ledger-matching)
