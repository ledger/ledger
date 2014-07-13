;;; ledger-mode.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2014 John Wiegley (johnw AT gnu DOT org)

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

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

(defvar ledger-occur-last-match nil
  "Last match found.")
(make-variable-buffer-local 'ledger-occur-last-match)

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
  (when ledger-occur-mode
    (ledger-occur-create-overlays
     (ledger-occur-compress-matches
      (ledger-occur-find-matches regex)))
    (setq ledger-occur-last-match regex)
    (if (get-buffer-window buffer)
        (select-window (get-buffer-window buffer))))
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


(defun ledger-occur-make-visible-overlay (beg end)
  (let ((ovl (make-overlay beg end (current-buffer))))
    (overlay-put ovl ledger-occur-overlay-property-name t)
    (overlay-put ovl 'face 'ledger-occur-xact-face)))

(defun ledger-occur-make-invisible-overlay (beg end)
  (let ((ovl (make-overlay beg end (current-buffer))))
    (overlay-put ovl ledger-occur-overlay-property-name t)
    (overlay-put ovl 'invisible t)))

(defun ledger-occur-create-overlays (ovl-bounds)
  "Create the overlays for the visible transactions.
Argument OVL-BOUNDS contains bounds for the transactions to be left visible."
  (let* ((beg (caar ovl-bounds))
         (end (cadar ovl-bounds)))
    (ledger-occur-make-invisible-overlay (point-min) (1- beg))
    (dolist (visible (cdr ovl-bounds))
      (ledger-occur-make-visible-overlay beg end)
      (ledger-occur-make-invisible-overlay (1+ end) (1- (car visible)))
      (setq beg (car visible))
      (setq end (cadr visible)))
    (ledger-occur-make-invisible-overlay (1+ end) (point-max))))

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
                   (point-max) ledger-occur-overlay-property-name t))

(defun ledger-occur-find-matches (regex)
  "Return a list of 2-number tuples describing the beginning and end of transactions meeting REGEX."
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

(defun ledger-occur-compress-matches (buffer-matches)
  "identify sequential xacts to reduce number of overlays required"
  (let ((points (list))
        (current-beginning (caar buffer-matches))
        (current-end (cadar buffer-matches)))
    (dolist (match (cdr buffer-matches))
      (if (< (- (car match) current-end) 2)
          (setq current-end (cadr match))
        (push (list current-beginning current-end) points)
        (setq current-beginning (car match))
        (setq current-end (cadr match))))
    (nreverse (push (list current-beginning current-end) points))))

(provide 'ledger-occur)

;;; ledger-occur.el ends here
