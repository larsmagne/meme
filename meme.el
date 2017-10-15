;;; meme.el --- meme interface -*- lexical-binding: t -*-
;; Copyright (C) 2016 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: fun

;; meme.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; meme.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Add the following to your .emacs to be able to use this package:

;; (setq load-path (cons (expand-file-name "~/src/meme") load-path))
;; (autoload 'meme "meme.el" "Create a meme from a collection" t)
;; (autoload 'meme-file "meme.el" "Create a meme from a file" t)

;; (meme-gif "/var/tmp/screenshots/Witness for the Prosecution/" "comp02")

;;; Code:

(require 'cl)
(require 'eww)
(require 'svg)
(require 'imgur)
(require 'giffy)

(defvar meme-width 400
  "The width of the meme images that are generated.")

(defvar meme-svg)
(defvar meme-animation)
(defvar meme-column)
(defvar meme-font "impact")

(defun meme ()
  "Create a meme image interactively in Emacs."
  (interactive)
  (switch-to-buffer (get-buffer-create "*meme*"))
  (meme-mode)
  (meme--insert-thumbnails)
  (setq-local post-command-hook nil)
  (setq-local meme-column 0)
  (add-hook 'post-command-hook 'meme--fix-point))

(defun meme-file (file)
  "Create a meme from FILE."
  (interactive "fImage to memefy: ")
  (switch-to-buffer (get-buffer-create "*meme*"))
  (meme-mode)
  (meme--setup-image file))

(defun meme-gif (directory match)
  "Create a animate meme GIF interactively in Emacs."
  (interactive "DSource directory: \nsMatching files (regexp): ")
  (switch-to-buffer (get-buffer-create "*meme*"))
  (meme-mode)
  (when giffy-timer
    (cancel-timer giffy-timer)
    (setq giffy-timer nil))
  (let* ((inhibit-read-only t)
	 (files (directory-files directory nil match))
	 (meme-data (meme--setup-image
		     (expand-file-name (car files) directory))))
    (setq-local after-change-functions nil)
    (setq-local post-command-hook nil)
    (goto-char (point-min))
    (forward-line 4)
    (insert "GIF    ")
    (let ((elem (list :start (meme--text-input "start" 4 "0"))))
      (plist-put elem :files (mapcar (lambda (f)
				       (svg--image-data 
					(expand-file-name f directory)
					"image/png"
					nil))
				     files))
      (plist-put elem :directory directory)
      (plist-put elem :size (image-size (create-image
					 (expand-file-name (car files)
							   directory))
					t))
      (plist-put elem :buffer (current-buffer))
      (plist-put elem :timestamp (float-time))
      (plist-put elem :index 0)
      (plist-put elem :direction 'forward)
      (plist-put elem :end (meme--text-input "end" 4
					     (format "%d" (length files))))
      (plist-put elem :skip (meme--text-input "skip" 4 "0"))
      (plist-put elem :rate (meme--text-input "rate" 4 "40"))
      (plist-put elem :mode (meme--text-input "mode" 8 "restart"))
      (insert "\n")
      (meme--animate meme-data elem)
      (setq-local meme-animation (list meme-data elem)))
    (eww-size-text-inputs)
    (add-hook 'after-change-functions #'eww-process-text-input nil t)
    nil))

(defun meme--fix-point ()
  (let ((column (current-column)))
    (when (get-text-property (point) 'meme-intangible)
      (if (> column meme-column)
	  (forward-char 1)
	(backward-char 1))
      (setq column (current-column)))
    (setq meme-column column)))

(defvar meme-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "\r" 'meme-save-or-upload)
    map))

(define-derived-mode meme-mode special-mode "Meme"
  "Major mode for creating meme images.

\\{meme-mode-map}"
  ;;(setq-local meme-font "impact")
  (setq buffer-read-only t))

(defvar meme--select-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'meme-select-image)
    map))

(defvar meme--submit-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'meme-save-or-upload)
    map))

(defun meme--insert-thumbnails ()
  (let* ((dir (expand-file-name
	       "images" (file-name-directory (locate-library "meme"))))
	 (pixels 180)
	 (width (/ (- (nth 2 (window-pixel-edges))
		      (nth 0 (window-pixel-edges))
		      20)
		   pixels))
	 (inhibit-read-only t)
	 (i 0))
    (erase-buffer)
    (dolist (file (directory-files dir t ".jpg\\'"))
      (when (and (not (bolp))
		 (not (= image-width pixels)))
	(insert (propertize " " 'display
			    `(space :align-to (,(* (mod i width) pixels)))
			    'meme-intangible t)))
      (setq image (create-image file 'imagemagick nil
				:max-width pixels
				:max-height pixels))
      (setq image-width (car (image-size image t)))
      (insert-image image " ")
      (add-text-properties (1- (point)) (point)
			   (list 'keymap meme--select-map
				 'file file))
      (when (zerop (mod (incf i) width))
	(insert "\n")))
    (goto-char (point-min))))

(defun meme-select-image ()
  "Start creating a meme from the image under point."
  (interactive)
  (let ((file (get-text-property (point) 'file)))
    (unless file
      (error "No file under point"))
    (meme--setup-image file)))

(defun meme--text-input (name size &optional value)
  (let ((start (point)))
    (eww-form-text (dom-node 'input `((size . ,(format "%s" size))
				      (name . ,name)
				      (value . ,(or value "")))))
    (put-text-property start (point) 'keymap meme--submit-map)
    (get-text-property start 'eww-form)))

(defun meme--insert-inputs (name)
  (insert (format "%-7s" (capitalize name)))
  (let ((elem (list :text (meme--text-input (format "%s-text" name) 60))))
    (insert "\n       ")
    (plist-put elem :margin (meme--text-input
			     (format "%s-margin" name) 4 "20"))
    (plist-put elem :size (meme--text-input
			   (format "%s-size" name) 4 "40"))
    (plist-put elem :color (meme--text-input
			    (format "%s-color" name) 8 "white"))
    (plist-put elem :align (meme--text-input
			    (format "%s-align" name) 8 "middle"))
    (plist-put elem :family (meme--text-input
			     (format "%s-family" name) 10 "impact"))
    (insert "\n")
    elem))

(defun meme--setup-image (file)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let* ((image (create-image file 'imagemagick))
	 (image-size (image-size image t))
	 (width meme-width)
	 (height (* (cdr image-size) (/ width (float (car image-size)))))
	 (svg (svg-create width height
			  :xmlns:xlink "http://www.w3.org/1999/xlink"))
	 (inhibit-read-only t)
	 (top (meme--insert-inputs "top"))
	 (bottom (meme--insert-inputs "bottom")))
    (insert "\n")
    (svg-insert-image svg)
    (insert (make-string 20 ? ))
    (insert-image (create-image
		   (expand-file-name
		    "manual.png"
		    (file-name-directory (locate-library "meme")))
		   'imagemagick
		   nil :max-width 120)
		  " ")
    (svg-embed svg file "image/jpeg" nil
	       :width width
	       :height height)
    (eww-size-text-inputs)
    (goto-char (next-single-property-change (point-min) 'eww-form))
    (setq-local meme-svg svg)
    (setq-local meme-animation nil)
    (setq after-change-functions nil)
    (add-hook 'after-change-functions #'eww-process-text-input nil t)
    (setq-local post-command-hook nil)
    (setq buffer-read-only t)
    (add-hook 'post-command-hook
	      (lambda ()
		(meme--update-meme svg height top bottom)))
    (set-buffer-modified-p nil)
    (list :svg svg :height height :top top :bottom bottom)))

(defun meme--value (elem name &optional number)
  (let ((value (plist-get (plist-get elem name) :value)))
    (if number
	(string-to-number value)
      value)))

(defun meme--update-meme (svg height top bottom &optional force)
  (when (or force
	    (buffer-modified-p))
    (let* ((inhibit-read-only t))
      (meme--update-text svg top
			 (+ (meme--value top :margin t)
			    (meme--value top :size t)
			    -10)
			 nil)
      (meme--update-text svg bottom
			 (- height (meme--value bottom :margin t))
			 t)
      (set-buffer-modified-p nil))))

(defun meme--update-text (svg data y-offset bottom)
  (let* ((elem (plist-get data :text))
	 (string (upcase (string-trim (meme--value data :text))))
	 (font-size (meme--value data :size t))
	 (family (meme--value data :family))
	 (bits (meme--fold-string string font-size family))
	 (align (meme--value data :align))
	 (i 0))
    (when (and bottom
	       (> (length bits) 1))
      (decf y-offset (* font-size (1- (length bits)))))
    (dotimes (i 10)
      (dolist (version '("a" "b"))
	(svg-remove svg (format "%s-%d-%s" (plist-get elem :name) i version))))
    (dolist (bit bits)
      ;; The idea here is that we render the text twice: First with a
      ;; black stroke, and then with a totally see-through stroke.
      ;; This should remove strokes-on-top-of-fills on overlapping
      ;; characters.
      (dolist (type '(("a" 4) ("b" 1)))
	(svg-text svg bit
		  :font-size font-size
		  :font-weight "bold"
		  :stroke "black"
		  :fill (meme--value data :color)
		  :font-family (meme--value data :family)
		  :letter-spacing (format "-%spt" (* font-size 0.07))
		  :font-stretch 'condensed
		  :stroke-width (cadr type)
		  :text-anchor (cond
				((or (equal align "left")
				     (string-match "^[0-9]+$" align))
				 "start")
				((equal align "right")
				 "end")
				(t
				 "middle"))
		  :x (cond
		      ((or (equal align "left")
			   (string-match "^[0-9]+$" align))
		       (if (equal align "left")
			   10
			 (string-to-number align)))
		      ((equal align "right")
		       390)
		      (t
		       (/ meme-width 2)))
		  :y (+ y-offset (* i font-size))
		  :id (format "%s-%d-%s" (plist-get elem :name) i (car type))))
      (incf i))))

(defun meme--fold-string (string font-size family)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((prev (point)))
      (while (meme--next-point)
	(when (> (meme--text-width (buffer-substring (line-beginning-position)
						     (point))
				   font-size
				   family)
		 (* meme-width 0.9))
	  (unless (= prev (line-beginning-position))
	    (goto-char prev))
	  (delete-char -1)
	  (insert "\n"))
	(setq prev (point)))
      (split-string (buffer-string) "\n"))))

(defun meme--next-point ()
  (and (not (eobp))
       (or (search-forward " " (point-max) 'bound)
	   (eobp))))

(defun meme--text-width (string font-size family)
  (let ((fallback meme-font))
    (prog1
	(with-temp-buffer
	  (let ((face (make-face 'meme-face)))
	    (condition-case _error
		(progn
		  (set-face-font
		   face
		   (format "-*-%s-normal-normal-condensed-*-%s-*-*-*-*-0-iso10646-1"
			   (capitalize family)
			   font-size))
		  (setq fallback family))
	      (error
	       (set-face-font
		face
		(format "-*-%s-normal-normal-condensed-*-%s-*-*-*-*-0-iso10646-1"
			(capitalize fallback)
			font-size))))
	    (insert (propertize string 'face 'meme-face))
	    (let ((shr-use-fonts t))
	      (shr-pixel-column))))
      (setq meme-font fallback))))

(defun meme-save-or-upload ()
  "Save the meme or upload it to imgur."
  (interactive)
  (cond
   (meme-animation
    (meme--save-animation
     (equal
      (car
       (read-multiple-choice
	"GIF or MP4?"
	'((?g "gif")
	  (?m "mp4"))))
      ?m)
     (read-file-name "Save to file: ")))
   ((eq (car (read-multiple-choice
	      "Save or upload?"
	      '((?s "save" "Save the meme in the format of your choice")
		(?u "upload" "Upload the meme to imgur"))))
	?s)
    (call-interactively 'meme-create-image))
   (t
    (let ((svg meme-svg))
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(svg-print svg)
	(call-process-region (point-min) (point-max) "convert"
			     t (list (current-buffer) nil) nil
			     "svg:-" "png:-")
	(let ((url (imgur-upload-image (buffer-string) t)))
	  (if (not url)
	      (message "Upload failed")
	    (message "Copied '%s' to the kill ring" url)
	    (with-temp-buffer
	      (insert (url-encode-url url))
	      (copy-region-as-kill (point-min) (point-max))))))))))

(defun meme-create-image (file)
  "Write the meme in the current buffer to a file."
  (interactive "FFile name to write to: ")
  (let ((svg meme-svg))
    (with-temp-buffer
      (svg-print svg)
      (if (string-match "\\.svg\\'" file)
	  (write-region (point-min) (point-max) file)
	(call-process-region (point-min) (point-max) "convert"
			     nil nil nil "svg:-"
			     (file-truename file))))))

(defun meme--animate (meme-data data)
  (when (buffer-live-p (plist-get data :buffer))
    (with-current-buffer (plist-get data :buffer)
      (save-excursion
	(let* ((inhibit-read-only t)
	       (delay (- (float-time) (plist-get data :timestamp)))
	       (at-time (max 0.001 (- (/ (meme--value data :rate t) 1000.0)
				      delay))))
	  (goto-char (point-min))
	  (when (re-search-forward "^Length:" nil t)
	    (delete-region (match-beginning 0)
			   (progn (forward-line 1) (point))))
	  (goto-char (point-min))
	  (forward-line 5)
	  (insert (format
		   "Length: %d  Index: %d  Real-Delay: %.5f\n"
		   (length (plist-get data :files))
		   (plist-get data :index)
		   at-time))
	  (if (eq (plist-get data :direction) 'forward)
	      (progn
		(incf (getf data :index) (1+ (meme--value data :skip t)))
		(when (>= (plist-get data :index) (meme--value data :end t))
		  (if (not (equal (meme--value data :mode) "restart"))
		      (setf (getf data :direction) 'backward
			    (getf data :index) (1- (meme--value data :end t)))
		    (setf (getf data :index) (meme--value data :start t)))))
	    (decf (getf data :index) (1+ (meme--value data :skip t)))
	    (when (<= (plist-get data :index) (meme--value data :start t))
	      (setf (getf data :direction) 'forward
		    (getf data :index) (1+ (meme--value data :start t)))))
	  (setf (getf data :timestamp) (float-time))
	  (meme--update-image meme-data
			      (elt (getf data :files) (getf data :index))
			      (getf data :size))
	  (setq giffy-timer
		(run-at-time
		 at-time
		 nil
		 (lambda ()
		   (meme--animate meme-data data)))))
	(goto-char (point))))))

(defun meme--update-image (meme-data base64 size)
  (let ((inhibit-read-only t)
	(image (meme--make-animated-image meme-data base64 size)))
    (goto-char (point-min))
    (forward-line 6)
    (delete-region (point) (point-max))
    (insert "\n")
    (insert-image (create-image image 'svg t))
    (set-buffer-modified-p nil)))

(defun meme--make-animated-image (meme-data base64 size)
  (let* ((width meme-width)
	 (height (* (cdr size) (/ width (float (car size)))))
	 (svg (svg-create width height
			  :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (meme--update-meme svg
		       (plist-get meme-data :height)
		       (plist-get meme-data :top)
		       (plist-get meme-data :bottom)
		       t)
    (with-temp-buffer
      (svg-print svg)
      (goto-char (point-min))
      (setq a (list (current-buffer) (buffer-string)))
      (search-forward "/xlink\">")
      (insert "<image xlink:href=\"")
      (insert base64)
      (insert (format "\" height=\"%s\" width=\"%s\"></image> "
		      height
		      width))
      (buffer-string))))

(defun meme--save-animation (make-mp4 file)
  (let* ((files-name (make-temp-file "giffy"))
	 (temp-files (list files-name))
	 (prefix (concat (file-name-nondirectory files-name) "-"))
	 (meme-data (car meme-animation))
	 (findex 0)
	 (data (cadr meme-animation)))
    (with-temp-buffer
      (loop for index from (meme--value data :start t) upto
	    (meme--value data :end t) by (1+ (meme--value data :skip t))
	    do (push (meme--write-animated-image prefix (incf findex)
						 meme-data data index)
		     temp-files))
      (unless (equal (meme--value data :mode) "restart")
	(loop for index from (1- (meme--value data :end t))
	      downto (1+ (meme--value data :start t))
	      by (1+ (meme--value data :skip t))
	      do (push (meme--write-animated-image prefix (incf findex)
						   meme-data data index)
		       temp-files)))
      (write-region (point-min) (point-max) files-name nil 'silent))
    (if make-mp4
	(setq file (format "%s.mp4" file))
      (setq file (format "%s.gif" file)))
    (when (file-exists-p file)
      (delete-file file))
    (if make-mp4
	(call-process "ffmpeg" files-name (get-buffer-create "*convert*") nil
		      "-r" "60"
		      "-f" "image2"
		      "-s" "1920x1080"
		      "-i" (concat "/tmp/" prefix "%04d.png")
		      "-vcodec" "libx264"
		      "-crf" "25"
		      "-pix_fmt" "yuv420p" file)
      (call-process "convert" nil (get-buffer-create "*convert*") nil
		    "-dispose" "none"
		    ;; Our delay is in ms, but "convert"s is in 100ths
		    ;; of a second.
		    "-delay" (format "%d" (/ (meme--value data :rate t) 10))
		    (format "@%s" files-name)
		    "-coalesce"
		    "-loop" "0" file))
    (mapc #'delete-file temp-files)
    (message "%s created" file)))

(defun meme--write-animated-image (prefix findex meme-data data index)
  (let ((file (format "/tmp/%s%04d.png" prefix findex)))
    (insert (format "'%s'\n" file))
    (with-temp-buffer
      (insert (meme--make-animated-image
	       meme-data (elt (getf data :files) index)
	       (getf data :size)))
      (call-process-region (point-min) (point-max) "convert"
			   nil nil nil "svg:-"
			   file))
    file))

(provide 'meme)

;;; meme.el ends here
