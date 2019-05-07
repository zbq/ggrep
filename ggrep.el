;;; ggrep.el --- grep with Graphical User Interface, output result in org-mode format.

;; Copyright (C) 2019 zbq

;; Repository: https://github.com/zbq/ggrep

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

(require 'widget)
(require 'rx)
(eval-when-compile (require 'wid-edit))

(defvar-local ggrep-path-widget nil)
(defvar-local ggrep-mask-widget nil)
(defvar-local ggrep-search-for-widget nil)

(defun ggrep-find-dir ()
  (let ((dir (read-directory-name "Directory:" nil "" t)))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(or (file-directory-p dir)
		(error "Not a directory: %s" dir))
	dir))

(defun ggrep-grep (path-list mask-list search-for-list)
  (let* ((path-list (remove "..." path-list))
		 (mask-list (mapcar #'string-trim mask-list))
		 (mask-list (mapcar #'wildcard-to-regexp (remove-if #'string-empty-p mask-list)))
		 (mask-rx (rx-to-string `(or ,@mask-list)))
		 (search-for-list (mapcar #'string-trim search-for-list))
		 (search-for-list (remove-if #'string-empty-p search-for-list)))
	(dolist (path path-list)
	  (message mask-rx)
	  (dolist (file (directory-files-recursively path mask-rx))
		(message file)))))

(defun ggrep-create-form ()
  "Create graphical form to get grep parameters."
  (interactive)
  (let ((buffer (generate-new-buffer "*ggrep*")))
	(with-current-buffer buffer
	  (widget-insert "\n\ngrep with Graphical User Interface, output result in org-mode format\n\n")
	  (widget-insert "Path:\n")
	  (setq ggrep-path-widget
			(widget-create 'editable-list
						   :entry-format "%i %d %v\n"
						   '(link :value "..."
								  :notify (lambda (widget &rest ignore)
											(widget-value-set widget (ggrep-find-dir))
											(widget-setup)))))
	  (widget-insert "\nFile Mask (support shell wildcard):\n")
	  (setq ggrep-mask-widget
			(widget-create 'editable-list
						   :entry-format "%i %d %v"
						   '(editable-field :value "")))
	  (widget-insert "\nSearch for (support regexp):\n")
	  (setq ggrep-search-for-widget
			(widget-create 'editable-list
						   :entry-format "%i %d %v"
						   '(editable-field :value "")))
	  (widget-insert "\n\n")
	  (widget-create 'push-button
					 :notify (lambda (&rest ignore)
							   (ggrep-grep (widget-value ggrep-path-widget)
										   (widget-value ggrep-mask-widget)
										   (widget-value ggrep-search-for-widget)))
					 "Search")
	  (widget-insert "\n")
	  (use-local-map widget-keymap)
	  (widget-setup))
  (switch-to-buffer buffer)))

(provide 'ggrep)

