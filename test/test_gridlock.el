#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_gridlock.el --- gridlock test
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, February  2, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-02-07 12:27:47 dharms>
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
;; Tests gridlock parsing.

;;

;;; Code:
(load-file "test/gridlock-test-common.el")
(require 'gridlock)

(ert-deftest gridlock-test-search-forward ()
  "Test basic parsing and searching forward."
  (with-temp-buffer
    (insert "One,Two,Three,\n1,2,3\n4,5,6")
    (gridlock-mode 1)
    ;; search forward from beginning
    (goto-char 1)
    (should (looking-at "One"))
    (should (eq (point) 1))
    (gridlock--find-next)
    (should (looking-at "1"))
    (should (equal (ht-get gridlock-buffer-points (point))
                   `((16 0 "1")
                     (18 1 "2")
                     (20 2 "3"))))
    ;; search forward from middle
    (goto-char 5)
    (should (looking-at "Two"))
    (gridlock--find-next)
    (should (looking-at "1"))
    ))

(ert-deftest gridlock-test-parse-delimited-fields ()
  "Test parsing delimited fields."
  (with-temp-buffer
    (setq-local gridlock-field-regex-begin "#")
    (setq-local gridlock-field-regex-end "@")
    (insert "One,Two,Three\n,,#1,2,3,\n#4,5,6\n,,,#7,8,9@,,,")
    (gridlock-mode 1)
    (goto-char 1)
    (should (looking-at "One"))
    (gridlock--find-next)
    (should (equal (ht-get gridlock-buffer-points (point))
                   '((18 0 "1")
                     (20 1 "2")
                     (22 2 "3"))))
    (gridlock--find-next)
    (should (equal (ht-get gridlock-buffer-points (point))
                   '((26 0 "4")
                     (28 1 "5")
                     (30 2 "6"))))
    (gridlock--find-next)
    (should (equal (ht-get gridlock-buffer-points (point))
                   '((36 0 "7")
                     (38 1 "8")
                     (40 2 "9"))))
    ))

(ert-run-tests-batch-and-exit (car argv))

;;; test_gridlock.el ends here
