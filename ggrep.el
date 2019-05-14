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

(defun ggrep-find-dir (initial)
  (let ((dir (read-directory-name "Directory:" (if (string-equal initial "...") nil initial) "" t)))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(or (file-directory-p dir)
		(error "Not a directory: %s" dir))
	dir))

(defun ggrep-grep (buffer path-list mask-list search-for-list ignore-case)
  (let* ((path-list (or (remove "..." path-list)
						(error "Need valid 'Path'")))
		 (mask-list (mapcar #'string-trim mask-list))
		 (mask-list (or (remove-if #'string-empty-p mask-list)
						(error "Need valid 'File Mask'")))
		 (mask-list (mapcar (lambda (wildcard) `(regexp ,(wildcard-to-regexp wildcard))) mask-list))
		 (mask-rx (rx-to-string `(or ,@mask-list)))
		 (search-for-list (mapcar #'string-trim search-for-list))
		 (search-for-list (or (remove-if #'string-empty-p search-for-list)
							  (error "Need valid 'Search for'")))
		 (search-for-bak search-for-list)
		 (search-for-list (mapcar (lambda (s) `(regexp ,s)) search-for-list))
		 (search-for-rx (rx-to-string `(or ,@search-for-list)))
		 (grep-func (lambda (fname search-for-rx)
					  (with-temp-buffer
						(insert-file-contents fname)
						(setq case-fold-search ignore-case)
						(goto-char (point-min))
						(do* ((complete nil)
							  (matches (cons '(:line-num -1) nil))
							  (tail matches))
							(complete (cdr matches))
						  (if (re-search-forward search-for-rx nil t)
							  (progn
								(goto-char (match-beginning 0))
								(let* ((line-num (line-number-at-pos))
									   (line-b (line-beginning-position))
									   (line-e (min (+ 180 line-b) (line-end-position))))
								  (setf (cdr tail) (cons (list :line-num line-num
															   :line (buffer-substring-no-properties line-b line-e))
														 nil))
								  (setf tail (cdr tail)))
								(forward-line 1))
							(setf complete t))))))
		 (output-func (lambda (last-dir shift-of-last-dir this-file matches)
						"
insert buffer:
+ dir
 + file
  - [[.../dir/file::5][5:]] xxxx yyy
return shift of this-dir (directory of this-file)."
						(let ((segs (split-string (file-relative-name this-file last-dir) "[/\\]"))
							  (shift shift-of-last-dir))
						  ;; output file hierarchy
						  (dolist (seg segs)
							(if (string-equal seg "..")
								(decf shift)
							  (progn
								(insert (format (format "%%%ds" shift) "")
										(format "+ %s\n" seg))
								(incf shift))))
						  ;; output matches
						  (dolist (match matches)
							(insert (format (format "%%%ds" shift) "")
									(format "- [[%s::%d][%d:]] %s\n"
											this-file
											(getf match :line-num)
											(getf match :line-num)
											(getf match :line))))
						  (1- shift)))))
	(when (not (get-buffer buffer))
	  (setq buffer (get-buffer-create buffer)))
	(with-current-buffer buffer
	  (setq buffer-read-only nil)
	  (kill-all-local-variables)
	  (buffer-disable-undo)
	  ;; clear buffer
	  (erase-buffer)
	  (insert "-*- mode: org; buffer-read-only: t -*-
* Search for\n")
	  (dolist (s search-for-bak)
		(insert "  " s "\n"))
	  ;; fill in grep result
	  (dolist (path path-list)
		(let ((last-dir path)
			  (shift-of-last-dir 1))
		  (insert "* " path "\n")
		  (message "%s" path)
		  (dolist (file (directory-files-recursively path mask-rx))
			(message "%s" file)
			(when-let ((matches (funcall grep-func file search-for-rx)))
			  (setq shift-of-last-dir (funcall output-func last-dir shift-of-last-dir file matches)
					last-dir (file-name-directory file))))))
	  (set-buffer-modified-p nil)
	  (org-mode)
	  (outline-show-all)
	  (setq buffer-read-only t))
	(ding)
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
											(widget-value-set widget (ggrep-find-dir (widget-value widget)))
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
										   (widget-value ggrep-search-for-widget)
										   nil))
					 "Case Sensitive Search")
	  (widget-insert "    ")
	  (widget-create 'push-button
					 :notify (lambda (&rest ignore)
							   (ggrep-grep ggrep-result-buffer
										   (widget-value ggrep-path-widget)
										   (widget-value ggrep-mask-widget)
										   (widget-value ggrep-search-for-widget)
										   t))
					 "Search")
	  (widget-insert "\n")
	  (use-local-map widget-keymap)
	  (widget-setup))
  (switch-to-buffer buffer)))

(provide 'ggrep)

