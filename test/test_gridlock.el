#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_gridlock.el --- gridlock test
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, February  2, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-02-06 08:50:09 dharms>
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
  "Test movement."
  (with-temp-buffer
    (insert "One,Two,Three\n1,2,3\n4,5,6")
    (gridlock-mode 1)
    ;; search forward from beginning
    (goto-char 1)
    (should (looking-at "One"))
    (should (eq (point) 1))
    (gridlock--find-next)
    (should (eq (point) 15))
    (should (looking-at "1"))
    (should (equal (ht-get gridlock-buffer-points 15)
                   `((15 0 "1")
                     (17 1 "2")
                     (19 2 "3"))))
    ;; search forward from middle
    (goto-char 5)
    (should (looking-at "Two"))
    (gridlock--find-next)
    (should (eq (point) 15))
    (should (looking-at "1"))
  ))

(ert-run-tests-batch-and-exit (car argv))

;;; test_gridlock.el ends here
