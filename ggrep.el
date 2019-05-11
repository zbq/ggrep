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
(defvar-local ggrep-result-buffer nil)

(defun ggrep-find-dir ()
  (let ((dir (read-directory-name "Directory:" nil "" t)))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(or (file-directory-p dir)
		(error "Not a directory: %s" dir))
	dir))

(defun ggrep-grep (buffer path-list mask-list search-for-list)
  (let* ((path-list (remove "..." path-list))
		 (mask-list (mapcar #'string-trim mask-list))
		 (mask-list (remove-if #'string-empty-p mask-list))
		 (mask-list (mapcar (lambda (wildcard) `(regexp ,(wildcard-to-regexp wildcard))) mask-list))
		 (mask-rx (rx-to-string `(or ,@mask-list)))
		 (search-for-list (mapcar #'string-trim search-for-list))
		 (search-for-list (remove-if #'string-empty-p search-for-list))
		 (grep-func (lambda (file) (list
									(list
									 :line-num "5"
									 :head "hello, "
									 :match "world"
									 :tail ".")
									(list
									 :line-num "15"
									 :head "hello, "
									 :match "jj world"
									 :tail " again."))))
		 (output-func (lambda (last-dir shift-of-last-dir this-file matches)
						"output:
+ dir
 + file
  - [[.../dir/file::5][5:]] xxxx *sdfsdf* yyy
return shift of this-dir (directory of this-file)."
						(let ((segs (split-string (file-relative-name this-file last-dir) "[/\\]"))
							  (shift shift-of-last-dir))
						  ;; output file hierarchy
						  (dolist (seg segs)
							(if (string-equal seg "..")
								(decf shift)
							  (progn
								(dotimes (i shift)
								  (insert " "))
								(insert "+ " seg "\n")
								(incf shift))))
						  ;; output matches
						  (dolist (match matches)
							(dotimes (i (1+ shift))
							  (insert " "))
							(insert "- [[" this-file "::" (getf match :line-num) "][" (getf match :line-num) ":]] "
									(getf match :head) "*" (getf match :match) "*" (getf match :tail) "\n"))
						  (1- shift)))))
	(when (not (get-buffer buffer))
	  (setq buffer (get-buffer-create buffer))
	  (with-current-buffer buffer
		(org-mode)
		(buffer-disable-undo)
		(setq buffer-read-only t)))
	(with-current-buffer buffer
	  (let ((inhibit-read-only t))
		;; clear buffer
		(erase-buffer)
		(insert "-*- mode: org; buffer-read-only: t -*-\n")
		;; fill in grep result
		(dolist (path path-list)
		  (let ((last-dir path)
				(shift-of-last-dir 1))
			(insert "* " path "\n")
			(dolist (file (directory-files-recursively path mask-rx))
			  (when-let ((matches (funcall grep-func file)))
				(setq shift-of-last-dir (funcall output-func last-dir shift-of-last-dir file matches)
					  last-dir (file-name-directory file))))))
		(set-buffer-modified-p nil)))
	(switch-to-buffer-other-window buffer)))

(defun ggrep-create-form ()
  "Create graphical form to get grep parameters."
  (interactive)
  (let ((buffer (generate-new-buffer "*ggrep*")))
	(with-current-buffer buffer
	  (setq ggrep-result-buffer (concat (buffer-name buffer) "-result"))
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
							   (ggrep-grep ggrep-result-buffer
										   (widget-value ggrep-path-widget)
										   (widget-value ggrep-mask-widget)
										   (widget-value ggrep-search-for-widget)))
					 "Search")
	  (widget-insert "\n")
	  (use-local-map widget-keymap)
	  (widget-setup))
  (switch-to-buffer buffer)))

(provide 'ggrep)

