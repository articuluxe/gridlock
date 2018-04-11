;;; gridlock-fix.el --- gridlock for fix
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, March 22, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-04-12 06:02:41 dharms>
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
;; Defines a minor mode for gridlock with fix messages.
;;

;;; Code:
(require 'gridlock)

(defvar gridlock-fix-init-hook '()
  "Hooks run after setting up `gridlock-fix-mode' but before inspecting buffer.")

(defvar gridlock-fix-metadata nil
  "The metadata for this fix buffer.")

(defcustom gridlock-fix-preferred-display-schemes
  '("popup" "pos-tip" "quick-peek" "echo")
  "Preferred display schemes for `gridlock-fix-mode'.
Display schemes will be loaded in this order."
  :group 'gridlock-group)

(defvar gridlock-fix-metadata-file "fix4.2.hash"
  "Data file to persist tag metadata.")

(defun gridlock-fix--reset-metadata ()
  "Reset the metadata associated with the current buffer in `gridlock-fix-mode'."
  ;(clrhash gridlock-fix-metadata)
  )

(defun gridlock-fix--get-buffer-metadata ()
  "Initialize the buffer's metadata for `gridlock-fix-mode'."
  (if (featurep 'hashtable-print-readable)
      (let ((file (locate-file gridlock-fix-metadata-file load-path)))
        (if (and file (file-exists-p file))
            (gridlock-fix-metadata-read-file file)
          (error "No metadata! (%s does not exist)"
                 (or file gridlock-fix-metadata-file))))
    (error
     "Gridlock Fix Metadata cannot be read in (hash tables are not serializable in this version of Emacs)")))

(defun gridlock-fix-metadata-read-file (file)
  "Read the value of `gridlock-fix-metadata' from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (setq gridlock-fix-metadata (read (current-buffer)))))

;; field class override for fix
(defclass gridlock-fix-field (gridlock-field)
  ((tag :initarg :tag
        :initform 0
        :type (integer 0 *)
        :documentation "The fix tag's numeric value."))
  "A fix field of interest.")

;; silence compiler warning
(eval-when-compile (defvar gridlock-fix-field))

(defmethod gridlock-fix-get-tag ((field gridlock-fix-field))
  "Get the fix tag number associated with FIELD."
  (oref field tag))

(defmacro gridlock-fix-create-field-macro (&rest args)
  "Create a `gridlock-fix-field' with ARGS, parameterized on Emacs version.
This is necessary as constructor arguments changed in Emacs 25."
  (if (version< emacs-version "25")
      `(apply gridlock-fix-field "gridlock-fix" ,@args)
    `(apply gridlock-fix-field ,@args)))

(defun gridlock-fix-create-field-compat (args)
  "Create a `gridlock-fix-field' according to ARGS."
  (gridlock-fix-create-field-macro args))

(defun gridlock-fix-create-field (&rest args)
  "Create a gridlock-fix-field instance with associated values ARGS."
  (let* ((field (gridlock-fix-create-field-compat args))
         (str (gridlock-get-str field))
         (values (split-string str "="))
        tag value)
    (setq tag (string-to-number (car values)))
    (when (> tag 0)
      (oset field tag tag))
    (setq value (cadr values))
    (when value
      (oset field string value))
    field))

(defun gridlock-fix-get-title (field)
  "Return the title associated with FIELD."
  (let* ((tag (gridlock-fix-get-tag field))
         (str (gethash tag gridlock-fix-metadata)))
    (if (and str (not (string-empty-p str)))
        str
      "")))

(defun gridlock-fix-gather-titles (fields)
  "Return a list of all field headings in field list FIELDS."
  (mapcar (lambda (field)
            (gridlock-fix-get-title field)) fields))

(defvar gridlock-fix-mode-map
  (let ((map gridlock-mode-map))
    (define-key map [t] 'gridlock-fix-turn-off)
    map)
  "Keymap for `gridlock-fix-mode'.")

(defun gridlock-fix-reset()
  "Reset `gridlock-fix' state."
  (gridlock-reset)
  (gridlock-fix--reset-metadata)
  )

(defun gridlock-fix-turn-off ()
  "Turn off `gridlock-fix-mode'."
  (interactive)
  (gridlock-fix-mode -1))

(define-minor-mode gridlock-fix-mode
  "Navigate between and explicate FIX fields."
  :init-value nil
  :lighter " GRID"
  :keymap gridlock-fix-mode-map
  (if gridlock-fix-mode
      (progn
        (setq gridlock-anchor-regex "FIX\\.[45]\\.")
        (setq gridlock-field-delimiter "\x01\\||")
        (setq gridlock-field-regex-begin "\\(8=\\)")
        (setq gridlock-field-regex-end "\\(10=[:digit:]\\{3\\}\\)")
        (setq gridlock-metadata-get-title-func #'gridlock-fix-get-title)
        (setq gridlock-metadata-gather-titles-func #'gridlock-fix-gather-titles)
        (setq gridlock-create-field-func #'gridlock-fix-create-field)
        (run-hooks 'gridlock-fix-init-hook)
        (gridlock-fix-reset)
        (gridlock-fix--get-buffer-metadata)
        (gridlock-activate-one-of-display-schemes
         gridlock-fix-preferred-display-schemes)
        (gridlock-show-title))
    (gridlock-fix-reset)))

(provide 'gridlock-fix)
;;; gridlock-fix.el ends here
