;;; counsel-git-grep-current-mode.el --- wrapper arround counsel-git-grep -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>
;; URL: https://github.com/juergenhoetzel/counsel-git-grep-current-mode
;; Package-Requires: ((emacs "25.1") (counsel "0.13.0"))
;; Keywords: convenience, matching, tools

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

;; Like `counsel-git-grep', but limit search to current file extension."

;;; Code:

(require 'counsel)

;;;###autoload
(defun counsel-git-grep-current-mode ()
  "Like `counsel-git-grep', but limit to current file extension."
  (interactive)
  (pcase-let ((`(_ . ,cmd) (counsel--git-grep-cmd-and-proj nil))
              (ext (file-name-extension (buffer-file-name))))
    (counsel-git-grep nil nil (format "%s -- '*.%s'" cmd (shell-quote-argument ext)))))

(provide 'counsel-git-grep-current-mode)
(global-set-key (kbd "C-c k") 'counsel-git-grep-current-mode)
;;; test.el ends here
