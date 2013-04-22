;;; ldg-mode.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2013 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;; Provide buffer narrowing to ledger mode.  Adapted from original loccur
;; mode by Alexey Veretennikov <alexey dot veretennikov at gmail dot
;; com>
;;
;; Adapted to ledger mode by Craig Earls <enderww at gmail dot
;; com>

;;; Code:

(defconst ledger-occur-overlay-property-name 'ledger-occur-custom-buffer-grep)

(defcustom ledger-occur-use-face-shown t
  "If non-nil, use a custom face for xacts shown in `ledger-occur' mode using ledger-occur-xact-face."
  :type 'boolean
  :group 'ledger)
(make-variable-buffer-local 'ledger-occur-use-face-shown)


(defvar ledger-occur-mode nil 
"name of the minor mode, shown in the mode-line")

(make-variable-buffer-local 'ledger-occur-mode)

(or (assq 'ledger-occur-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list '(ledger-occur-mode ledger-occur-mode))))

(defvar ledger-occur-history nil
  "History of previously searched expressions for the prompt.")
;;(make-variable-buffer-local 'ledger-occur-history)

(defvar ledger-occur-last-match nil
  "Last match found.")
(make-variable-buffer-local 'ledger-occur-last-match)

(defvar ledger-occur-overlay-list nil
  "A list of currently active overlays to the ledger buffer.")
(make-variable-buffer-local 'ledger-occur-overlay-list)

(defun ledger-occur-remove-all-overlays ()
  "Remove all overlays from the ledger buffer."
  (interactive)
  (remove-overlays))

(defun ledger-occur-mode (regex buffer)
  "Highlight transactions that match REGEX in BUFFER, hiding others.

When REGEX is nil, unhide everything, and remove higlight"
  (set-buffer buffer)
  (setq ledger-occur-mode
	(if (or (null regex)
		(zerop (length regex)))
	    nil
	    (concat " Ledger-Narrowed: " regex)))
  (force-mode-line-update)
  (ledger-occur-remove-overlays)
  (if ledger-occur-mode
      (let* ((buffer-matches (ledger-occur-find-matches regex))
	     (ovl-bounds (ledger-occur-create-xact-overlay-bounds buffer-matches)))
	(setq ledger-occur-overlay-list
	      (append (ledger-occur-create-xact-overlays ovl-bounds)
		      (ledger-occur-create-narrowed-overlays buffer-matches)))
	(setq ledger-occur-last-match regex)
	(if (get-buffer-window buffer)
	    (select-window (get-buffer-window buffer)))))
  (recenter))

(defun ledger-occur (regex)
  "Perform a simple grep in current buffer for the regular expression REGEX.

   This command hides all xact from the current buffer except
   those containing the regular expression REGEX.  A second call
   of the function unhides lines again"
  (interactive
   (if ledger-occur-mode
       (list nil)
       (list (read-string (concat "Regexp<" (ledger-occur-prompt) ">: ") 
			  nil 'ledger-occur-history (ledger-occur-prompt)))))
  (ledger-occur-mode regex (current-buffer)))

(defun ledger-occur-prompt ()
  "Return the default value of the prompt.

   Default value for prompt is a current word or active
   region(selection), if its size is 1 line"
  (let ((prompt
         (if (and transient-mark-mode
                  mark-active)
             (let ((pos1 (region-beginning))
                   (pos2 (region-end)))
               ;; Check if the start and the of an active region is on
               ;; the same line
               (if (= (line-number-at-pos pos1)
                      (line-number-at-pos pos2))
                   (buffer-substring-no-properties pos1 pos2)))
	     (current-word))))
    prompt))

(defun ledger-occur-create-narrowed-overlays(buffer-matches)
  (if buffer-matches
      (let ((overlays
             (let ((prev-end (point-min)))
               (mapcar (lambda (match)
                         (prog1
			     (make-overlay prev-end (car match)
					   (current-buffer) t nil)
                           (setq prev-end (1+ (cadr match)))))
                       buffer-matches))))
        (mapcar (lambda (ovl)
                  (overlay-put ovl ledger-occur-overlay-property-name t)
                  (overlay-put ovl 'invisible t))
                (push  (make-overlay (cadr (car(last buffer-matches)))
                                     (point-max)
                                     (current-buffer) t nil) overlays)))))


(defun ledger-occur-create-xact-overlays (ovl-bounds)
  "Create the overlay for the visible transactions.
Argument OVL-BOUNDS contains bounds for the transactions to be left visible."
  (let ((overlays
         (mapcar (lambda (bnd)
                   (make-overlay (car bnd)
				 (cadr bnd)
				 (current-buffer) t nil))
                 ovl-bounds)))
    (mapcar (lambda (ovl)
	      (overlay-put ovl ledger-occur-overlay-property-name t)
	      (if ledger-occur-use-face-shown
		  (overlay-put ovl 'face 'ledger-occur-xact-face )))
            overlays)))

(defun ledger-occur-quit-buffer (buffer)
  "Quits hidings transaction in the given BUFFER.
Used for coordinating `ledger-occur' with other buffers, like reconcile."
  (set-buffer buffer)
  (setq ledger-occur-mode nil)
  (force-mode-line-update)
  (ledger-occur-remove-overlays)
  (recenter))

(defun ledger-occur-remove-overlays ()
  "Remove the transaction hiding overlays."
  (interactive)
  (remove-overlays (point-min)
		   (point-max) ledger-occur-overlay-property-name t)
  (setq ledger-occur-overlay-list nil))


(defun ledger-occur-create-xact-overlay-bounds (buffer-matches)
  "Use BUFFER-MATCHES to produce the overlay for the visible transactions."
  (let ((prev-end (point-min))
        (overlays (list)))
    (when buffer-matches
      (mapc (lambda (line)
	      (push (list (car line) (cadr line)) overlays)
	      (setq prev-end (cadr line)))
	    buffer-matches)
      (setq overlays (nreverse overlays)))))


(defun ledger-occur-find-matches (regex)
  "Return a list of 2-number tuples describing the beginning and start of transactions meeting REGEX."
  (save-excursion
    (goto-char (point-min))
    ;; Set initial values for variables
    (let (curpoint 
	  endpoint
	  (lines (list)))
      ;; Search loop
      (while (not (eobp))
        (setq curpoint (point))
        ;; if something found
        (when (setq endpoint (re-search-forward regex nil 'end))
          (save-excursion
	    (let ((bounds (ledger-find-xact-extents (match-beginning 0))))
	      (push bounds lines)
	      (setq curpoint (cadr bounds)))) ;; move to the end of
	                                      ;; the xact, no need to
	                                      ;; search inside it more
          (goto-char curpoint))
        (forward-line 1))
      (setq lines (nreverse lines)))))


(provide 'ldg-occur)

;;; ldg-occur.el ends here
