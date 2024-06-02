;;; dired-open-with.el --- And "Open with" dialog for Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jakub Kadlčík

;; Author: Jakub Kadlčík <frostyx@email.cz>
;; URL: https://github.com/FrostyX/dired-open-with
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (s "1.13.0"))
;; Keywords: files, dired, xdg, open-with

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; An 'Open with' dialog for opening files in external applications from Dired.
;;
;; This package is built upon freedesktop.org features and therefore works
;; only on operating systems and desktop environments that comply with the
;; XDG specifications. That should be true for the majority of GNU/Linux
;; distributions and BSD variants. I don't know what is the situation on
;; MS Windows, macOS, or and mobile systems.
;;
;; https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html


;;; Code:

;;;; Requirements

(require 's)
(require 'xdg)
(require 'mailcap)
(require 'dired)

;;;; Commands

;;;###autoload
(defun dired-open-with ()
  "An 'Open with' dialog for opening files in external applications from Dired.
Such dialogs are known from GUI file managers, when right-clicking a file."
  (interactive)

  (when (xdg-runtime-dir)
    (error (concat
            "You are running an unsupported operating system or desktop "
            "environment. It doesn't comply with the XDG specification.")))

  (let* ((path (dired-get-file-for-visit))
         (apps (dired-open-with--applications-for-file path))
         (app (dired-open-with--completing-read apps))
         (cmd (dired-open-with--xdg-format-exec (gethash "Exec" app) path)))
    (dired-open-with--start-process cmd)))

;;;; Functions

;;;;; Public

;;;;; Private

(defun dired-open-with--completing-read (apps)
  "A convenience wrapper around `completing-read' for this package.
It takes a list of APPS (represented as Hash Tables) and returns the
selected application."
  (let* ((items (mapcar (lambda (app) (cons (gethash "Name" app) app)) apps))
         (max-length (apply #'max (mapcar (lambda (x) (length (car x))) items)))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (name)
               (let ((annotation (gethash "Comment" (cdr (assoc name items)))))
                 (concat
                  (make-string (- (+ max-length 2) (length name)) ?\s)
                  annotation)))))
         (value (completing-read
                 "Open with: "
                 (mapcar (lambda (item) (gethash "Name" (cdr item))) items))))
    (cdr (assoc value items))))

(defun dired-open-with--applications-for-file (path)
  "Return a list of applications that can open a given PATH.
Every application is represented as a Hash Table."
  (let* ((extension (file-name-extension path))
         (mimetype (mailcap-extension-to-mime extension))
         (applications (xdg-mime-apps mimetype)))
    (if applications
        (mapcar #'xdg-desktop-read-file applications)
      (error "No XDG appliations found for MIME type: %s" mimetype))))

(defun dired-open-with--xdg-format-exec (exec path)
  "Format XDG application EXEC string with PATH and return an executable command.
For the list of keys and their meaning, please see
https://specifications.freedesktop.org/desktop-entry-spec/latest/ar01s07.html

Oh cmon ... this must be already implemented somewhere."
  (let* ((url path)
         (cmd exec)
         (cmd (s-replace "%f" path cmd))
         (cmd (s-replace "%F" path cmd))
         (cmd (s-replace "%u" url cmd))
         (cmd (s-replace "%U" url cmd))
         (cmd (s-replace "%i" "" cmd))
         (cmd (s-replace "%c" "" cmd))
         (cmd (s-replace "%k" "" cmd)))
    cmd))

(defun dired-open-with--start-process (cmd)
  "Start a process for this CMD.
The functions for running processes implemented in `dired-open' doesn't
support inputting only a command (already containing the file) but always
operate with an executable and then concatenating a file at the end of the
line.  That is not suitable for XDG applications that contain formatting in
their Exec and expect us to inject the filename into a specific part of the
string."
  (apply 'start-process
         (append '("dired-open-with" nil)
                 (split-string cmd))))

;;;; Footer

(provide 'dired-open-with)

;;; dired-open-with.el ends here
