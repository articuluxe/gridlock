#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_gridlock-csv.el --- gridlock-csv test
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, February 22, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-02-22 17:36:34 dharms>
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

(ert-deftest gridlock-csv-test-metadata ()
  "Test gridlock-csv metadata."
  (let (anchor fields)
    (with-temp-buffer
      (insert "One,Two,Three\n1,2,3\n4,5,6")
      (gridlock-csv-mode)
      (goto-char 17)
      (should (looking-at "2"))
      (setq anchor (gridlock--find-anchor-on-line (point)))
      (setq fields (gridlock-get-fields-at anchor))
      (should (eq (length fields) 3))
      ;; field 1
      (should (string= (gridlock-field-get-title (aref fields 0))
                       "One"))
      (should (string= (gridlock-field-get-title (aref fields 1))
                       "Two"))
      (should (string= (gridlock-field-get-title (aref fields 2))
                       "Three"))
      )))

(ert-run-tests-batch-and-exit (car argv))
;;; test_gridlock-csv.el ends here
