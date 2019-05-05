;;; magit-diff-flycheck.el --- Report errors in diffs -*- lexical-binding: t; -*-

;; Author: Alex Ragone <ragonedk@gmail.com.com>
;; Created: 05 May 2019
;; Homepage: https://github.com/ragone/magit-diff-flycheck
;; Keywords: convenience, matching
;; Package-Version: 0.1.0
;; Package-Requires: ((magit "2") (flycheck "31") (seq "2") (emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run M-x magit-diff-flycheck in a magit-diff buffer to display a
;; filtered list of errors for the added/modified lines only.
;;
;; This is primarily meant to be used on legacy projects where you do
;; not want to see errors for the whole file but only for the added/modified
;; lines.

;;; Code:

(require 'magit-diff)
(require 'flycheck)
(require 'seq)

(defvar magit-diff-flycheck-current-errors nil
  "List of `flycheck-error' for all the buffers.")

(defvar magit-diff-flycheck-inhibit-message t
  "If non-nil, disable message output while running.")

;;;###autoload
(defun magit-diff-flycheck ()
  "Run flycheck on all added lines in `magit-diff-mode'."
  (interactive)
  (unless (derived-mode-p 'magit-diff-mode)
    (user-error "Not a Magit Diff buffer"))
  (magit-diff-flycheck--setup)
  (magit-diff-flycheck--run)
  (magit-diff-flycheck--teardown))

(defun magit-diff-flycheck--setup ()
  "Setup before running the program."
  (magit-diff-set-context (lambda (_num) 0))
  (if magit-diff-flycheck-inhibit-message
    (setq inhibit-message t)))

(defun magit-diff-flycheck--teardown ()
  "Teardown after running the program."
  (magit-diff-default-context)
  (if magit-diff-flycheck-inhibit-message
    (setq inhibit-message nil)))

(defun magit-diff-flycheck--run ()
  "Run the checkers on the files in the diff buffer."
  (let ((file-sections (seq-filter #'magit-file-section-p
                                   (oref magit-root-section children))))
    (magit-diff-flycheck-clear-errors)
    (seq-do #'magit-diff-flycheck-file-section file-sections)
    (magit-diff-flycheck-list-errors)))

(defun magit-diff-flycheck-file-section (file-section)
  "Run flycheck on FILE-SECTION."
  (let* ((filename (oref file-section value))
         (buffer (first (magit-diff-visit-file--noselect filename))))
    (with-current-buffer buffer
      (add-hook 'flycheck-after-syntax-check-hook
                (apply-partially #'magit-diff-flycheck--flycheck-collect-errors file-section)
                nil
                t)
      ;; Disable threshold to get all errors
      (setq-local flycheck-checker-error-threshold nil)
      (condition-case nil
          (flycheck-buffer)
        (user-error (kill-buffer))))))

(defun magit-diff-flycheck-clear-errors ()
  "Clear the displayed errors."
  (setq magit-diff-flycheck-current-errors nil))

(defun magit-diff-flycheck--remove-filename (fn err)
  "Remove the filename from ERR, run FN and revert the filename."
  (let ((mode-active (derived-mode-p 'magit-diff-flycheck-error-list-mode))
        (file (flycheck-error-filename err)))
    (if mode-active
      (setf (flycheck-error-filename err) nil))
    (apply fn (list err))
    (if mode-active
      (setf (flycheck-error-filename err) file))))

(defun magit-diff-flycheck--contained-in-diff-p (err hunk-sections)
  "Return non-nil if ERR is contained in any of the HUNK-SECTIONS."
  (seq-some (lambda (hunk)
              (let* ((to-range (oref hunk to-range))
                     (start (first to-range))
                     (len (second to-range))
                     (end (+ start (if len (1- len) 0)))
                     (err-line (flycheck-error-line err)))
                (<= start err-line end)))
            hunk-sections))

(defun magit-diff-flycheck--filter-errors (errors file-section)
  "Filter ERRORS for FILE-SECTION."
  (seq-filter (lambda (err)
                (magit-diff-flycheck--contained-in-diff-p err (oref file-section children)))
              errors))

(defun magit-diff-flycheck--flycheck-collect-errors (file-section)
  "Collect errors for FILE-SECTION."
  (let* ((filename (oref file-section value))
         (errors (seq-map (lambda (err)
                            (unless (flycheck-error-filename err)
                              (setf (flycheck-error-filename err) filename))
                            err)
                          flycheck-current-errors))
         (filtered (magit-diff-flycheck--filter-errors errors file-section)))
    (setq magit-diff-flycheck-current-errors (append magit-diff-flycheck-current-errors filtered))
    (remove-hook 'flycheck-after-syntax-check-hook
                 (apply-partially #'magit-diff-flycheck--flycheck-collect-errors file-section)
                 t)
    (flycheck-error-list-refresh)))

(defun magit-diff-flycheck-list-errors ()
  "Show the error list."
  (interactive)
  (unless (get-buffer flycheck-error-list-buffer)
    (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
      (magit-diff-flycheck-error-list-mode)))
  (display-buffer flycheck-error-list-buffer)
  (flycheck-error-list-refresh))

(defun magit-diff-flycheck--error-list-entries ()
  "Create the entries for the error list."
  (let ((filtered (flycheck-error-list-apply-filter magit-diff-flycheck-current-errors)))
    (seq-map #'flycheck-error-list-make-entry filtered)))

(define-derived-mode magit-diff-flycheck-error-list-mode flycheck-error-list-mode
  "Flycheck errors"
  "Major mode for listing Flycheck errors.

\\{flycheck-error-list-mode-map}"
  (setq tabulated-list-format flycheck-error-list-format
        tabulated-list-sort-key (cons "File" nil)
        tabulated-list-padding flycheck-error-list-padding
        tabulated-list-entries #'magit-diff-flycheck--error-list-entries
        mode-line-buffer-identification flycheck-error-list-mode-line)
  (advice-add #'flycheck-jump-to-error :around #'magit-diff-flycheck--remove-filename)
  (tabulated-list-init-header))

(provide 'magit-diff-flycheck)

;;; magit-diff-flycheck.el ends here
