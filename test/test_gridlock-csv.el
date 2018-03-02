#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_gridlock-csv.el --- gridlock-csv test
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, February 22, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-03-02 08:42:37 dharms>
;; Modified by: Dan Harms
;; Keywords: test gridlock

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
;; Test gridlock-csv.
;;

;;; Code:
(load-file "test/gridlock-test-common.el")
(require 'gridlock-csv)

(defun test-gridlock-csv-metadata-helper (buffer vec)
  "Helper function to test metadata parsing."
  (let (anchor fields)
    (cl-letf (((symbol-function 'gridlock--show-title-helper)
               (lambda(_)))
              ((symbol-function 'gridlock--hide-title-helper)
               (lambda(_))))
      (with-temp-buffer
        (insert buffer)
        (gridlock-csv-mode)
        (search-forward-regexp "\n" nil t)
        (search-forward-regexp "\n" nil t)
        ;; move to the second line, before the end
        (goto-char (1- (match-beginning 0)))
        (setq anchor (gridlock--find-anchor-on-line (point)))
        (setq fields (gridlock-get-fields-at anchor))
        (should (eq (length fields) 3))
        (should (string= (gridlock-field-get-title (aref fields 0))
                         (aref vec 0)))
        (should (string= (gridlock-field-get-title (aref fields 1))
                         (aref vec 1)))
        (should (string= (gridlock-field-get-title (aref fields 2))
                         (aref vec 2)))
        ))))


(ert-deftest gridlock-csv-test-metadata ()
  "Test gridlock-csv metadata."
  (test-gridlock-csv-metadata-helper "One,Two,Three\n1,2,3\n4,5,6" ["One" "Two" "Three"])
  (test-gridlock-csv-metadata-helper "#One,Two,Three\n1,2,3\n4,5,6" ["One" "Two" "Three"])
  )

(ert-run-tests-batch-and-exit (car argv))
;;; test_gridlock-csv.el ends here
