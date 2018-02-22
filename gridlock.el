;;; gridlock.el --- navigate among delimited fields
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, January 26, 2018
;; Version: 0.1
;; Modified Time-stamp: <2018-02-22 17:39:15 dharms>
;; Modified by: Dan Harms
;; Keywords: tools
;; URL: https://github.com/articuluxe/gridlock.git
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Navigate between delimited fields, and explicate them if possible.
;;

;;; Code:
(require 'subr-x)
(require 'ht)

(defgroup gridlock nil
  "Helper to navigate between fields, and explicate them."
  :group 'tools
  :prefix "gridlock")

(defvar-local gridlock-buffer-points
  (ht-create 'eq)
  "List of gridlock points in the current buffer.")

(defvar gridlock-anchor-regex "^"
  "Regexp identifying lines of interest.")
(defvar gridlock-field-delimiter ","
  "Regexp splitting fields per line.")
(defvar gridlock-field-regex-begin nil
  "Regexp to delimit the start of the fields to be scanned per line.
If nil, the entire string from the anchor point will be used.")
(defvar gridlock-field-regex-end nil
  "Regexp to delimit the end of the fields to be scanned per line.
If nil, the entire string to the end of line will be used.")

(defun gridlock--find-anchor-on-line (pt)
  "Find location, if any, of anchor point on line containing PT."
  (let (anchor fields)
    (save-excursion
      (goto-char pt)
      (goto-char (line-beginning-position))
      (and (setq anchor (search-forward-regexp gridlock-anchor-regex (line-end-position) t))
           (setq fields (gridlock--check-anchor anchor))
           (gridlock--on-anchor-found anchor fields))
      anchor)))

(defun gridlock-reset ()
  "Reset gridlock state in current buffer."
  (interactive)
  (ht-clear! gridlock-buffer-points))

;;;###autoload
(defun gridlock-goto-next-line ()
  "Advance point to next anchor point."
  (interactive)
  (let ((pt (gridlock--find-next-line)))
    (if pt (goto-char pt)
      (message "No further lines."))))

(defun gridlock--find-next-line ()
  "Return location, if any, of next anchor point."
  (let* ((pt (point))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor))
         (idx (gridlock--lookup-field-at-pos fields pt))
         beg)
    (save-excursion
      (setq beg (search-forward-regexp gridlock-anchor-regex nil t))
      (when (eq beg pt)
        (goto-char (1+ (point)))
        (setq beg (search-forward-regexp gridlock-anchor-regex nil t)))
      (if beg
          (progn
            (and
             (setq fields (gridlock--check-anchor beg))
             (gridlock--on-anchor-found beg fields))
            (if (and idx (< idx (length fields)))
                (car (gridlock-field-get-bounds (aref fields idx)))
              beg))
        nil))))

;;;###autoload
(defun gridlock-goto-prev-line ()
  "Advance point to prior anchor point."
  (interactive)
  (let ((pt (gridlock--find-prev-line)))
    (if pt (goto-char pt)
      (message "No prior lines."))))

(defun gridlock--find-prev-line ()
  "Return location, if any, of prior anchor point."
  (let* ((pt (point))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor))
         (idx (gridlock--lookup-field-at-pos fields pt))
         beg)
    ;; (message "drh prev pt %d anchor %d idx %d" pt anchor (if idx idx -34))
    (when anchor
      (setq pt anchor))
    (save-excursion
      (goto-char pt)
      (setq beg (search-backward-regexp gridlock-anchor-regex nil t))
      (when (eq beg pt)
        (goto-char (1- (point)))
        (setq beg (search-backward-regexp gridlock-anchor-regex nil t)))
      (if beg
          (progn
            (when (setq fields (gridlock--check-anchor beg))
                 (gridlock--on-anchor-found beg fields))
            (if (and idx (< idx (length fields)))
                (car (gridlock-field-get-bounds (aref fields idx)))
              beg))
        nil))))

;;;###autoload
(defun gridlock-goto-line-start ()
  "Move point to the beginning of the first field, if any, on the current line."
  (interactive)
  (let ((pt (gridlock--find-first-field)))
    (if pt (goto-char pt)
      (message "No fields on current line."))))

(defun gridlock--find-first-field ()
  "Return location, if any, of first field on current line."
  (let* ((pt (point))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor)))
    (when (> (length fields) 0)
      (car (gridlock-field-get-bounds
            (aref fields 0))))))

;;;###autoload
(defun gridlock-goto-line-end ()
  "Move point to the beginning of the last field, if any, on the current line."
  (interactive)
  (let ((pt (gridlock--find-last-field)))
    (if pt (goto-char pt)
      (message "No fields on current line."))))

(defun gridlock--find-last-field ()
  "Return location, if any, of last field on current line."
  (let* ((pt (point))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor))
         (len (length fields)))
    (when (> len 0)
      (car (gridlock-field-get-bounds
            (aref fields (1- len)))))))

;;;###autoload
(defun gridlock-goto-next-field ()
  "Move point to the next field, if any, on current line."
  (interactive)
  (let ((fld (gridlock-get-next-field (point))))
    (if fld
        (goto-char (car (gridlock-field-get-bounds fld)))
      ;; todo allow wrap-around to next line
      (message "No further fields on this line."))))

;;;###autoload
(defun gridlock-goto-previous-field ()
  "Move point to the previous field, if any, on current line."
  (interactive)
  (let ((fld (gridlock-get-previous-field (point))))
    (if fld
        (goto-char (car (gridlock-field-get-bounds fld)))
      (message "No further fields on this line."))))

(defun gridlock--check-anchor (anchor)
  "Validate what fields can be parsed from anchor point ANCHOR.
If none, return nil.  If some, return those fields."
  (let ((fields (gridlock--parse-line anchor)))
    (if (> (length fields) 0) fields nil)))

(defun gridlock--on-anchor-found (anchor fields)
  "Mark that anchor point ANCHOR is associated with FIELDS."
  (unless (ht-contains? gridlock-buffer-points anchor)
    (ht-set! gridlock-buffer-points anchor fields))
  ;; todo metadata
  )

(defun gridlock--parse-line (beg)
  "Parse the format of the line beginning at BEG."
  (save-excursion
    (save-match-data
      (let ((pt beg)
            (end (line-end-position))
            (idx 0)
            lst str vec)
        (goto-char beg)
        (and gridlock-field-regex-begin
             (not (string-empty-p gridlock-field-regex-begin))
             (search-forward-regexp gridlock-field-regex-begin end t)
             (setq beg (or (match-end 0) beg))
             (setq pt beg)
             (goto-char beg)
             gridlock-field-regex-end
             (not (string-empty-p gridlock-field-regex-end))
             (search-forward-regexp gridlock-field-regex-end end t)
             (setq end (or (match-beginning 0) end))
             ;; (message "pre: beg %d end %d" beg end)
             (goto-char beg)
             )
        (while (search-forward-regexp gridlock-field-delimiter end t)
          (setq str (buffer-substring-no-properties
                     pt (match-beginning 0)))
          ;; (message "during: bounds: (%d,%d) end is %d, str is %s" pt (match-beginning 0) end str)
          (push (list (cons pt (match-beginning 0)) idx str) lst)
          (setq idx (1+ idx))
          (setq pt (point)))
        (setq str (string-trim (buffer-substring-no-properties pt end)))
        ;; (message "post: end pt is %d, end is %d, str is %s" pt end str)
        (unless (string-empty-p str)
          (push (list (cons pt (+ pt (length str))) idx str) lst))
        (setq vec (make-vector (length lst) nil))
        (dolist (elt (nreverse lst) vec)
          (aset vec (gridlock-field-get-index elt) elt)
          )))))

(defun gridlock-field-get-bounds (field)
  "Given a record FIELD, return its bounds.
This is a cons cell (BEG . END) of the field's bounds."
  (nth 0 field))

(defun gridlock-field-get-index (field)
  "Given a record FIELD, return its index."
  (nth 1 field))

(defun gridlock-field-get-str (field)
  "Given a record FIELD, return its string value."
  (nth 2 field))

(defvar gridlock-field-get-title-func nil
  "Given a record FIELD, return its grid's title.
Function takes one parameter, field.")

(defun gridlock-field-get-title (field)
  "Given a record FIELD, return its grid's title."
  (funcall gridlock-field-get-title-func field))

(defun gridlock-get-field-at (point)
  "Return the field, if any, that POINT lies on."
  (let* ((anchor (gridlock--find-anchor-on-line point))
         (fields (gridlock-get-fields-at anchor))
         (idx (gridlock--lookup-field-at-pos fields point)))
    (and idx (aref fields idx))))

(defun gridlock-get-fields-at (anchor)
  "Return the fields, if any, existing on line associated with ANCHOR.."
  (ht-get gridlock-buffer-points anchor))

(defun gridlock--lookup-field-at-pos (fields pt)
  "Lookup in field list FIELDS what field, if any, PT is within."
  (let ((len (length fields))
        (i 0)
        elt bounds)
    (catch 'found
      (progn
        (while (< i len)
          (setq elt (aref fields i))
          (setq bounds (gridlock-field-get-bounds elt))
          (when (and (>= pt (car bounds))
                     (< pt (cdr bounds)))
            (throw 'found i))
          (setq i (1+ i)))
        nil))))

(defun gridlock-get-previous-field (point)
  "Return the field, if any, that precedes that at POINT."
  (let* ((anchor (gridlock--find-anchor-on-line point))
         (fields (gridlock-get-fields-at anchor))
         (idx (gridlock--lookup-field-at-pos fields point))
         prev)
    (and idx
         (setq prev (gridlock--get-previous-field fields idx))
         (aref fields prev))))

(defun gridlock-get-next-field (point)
  "Return the field, if any, following that at POINT."
  (let* ((anchor (gridlock--find-anchor-on-line point))
         (fields (gridlock-get-fields-at anchor))
         (idx (gridlock--lookup-field-at-pos fields point))
         next)
    (and idx
         (setq next (gridlock--get-next-field fields idx))
         (aref fields next))))

(defun gridlock--get-next-field (fields idx)
  "Return the next field from FIELDS after IDX, if it exists."
  (let ((i (1+ idx)))
    (and (< i (length fields)) i)))

(defun gridlock--get-previous-field (fields idx)
  "Return the previous field from FIELDS before IDX, if it exists."
  (when (and (<= idx (length fields))
             (> idx 0))
    (1- idx)))

(defun gridlock-define-keys (map)
  "Define in keymap MAP bindings for `gridlock-mode'."
  (define-key map (kbd "<down>") 'gridlock-goto-next-line)
  (define-key map (kbd "<up>") 'gridlock-goto-prev-line)
  (define-key map (kbd "<left>") 'gridlock-goto-previous-field)
  (define-key map (kbd "<right>") 'gridlock-goto-next-field)
  (define-key map "a" 'gridlock-goto-line-start)
  (define-key map "e" 'gridlock-goto-line-end)
  )

(defvar gridlock-mode-map
  (let ((map (make-sparse-keymap)))
    (gridlock-define-keys map)
    map)
  "Keymap for `gridlock-mode'.")

(provide 'gridlock)
;;; gridlock.el ends here
