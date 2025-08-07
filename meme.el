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

(require 'cl-lib)
(require 'eww)
(require 'svg)
(require 'imgur)

(defgroup meme nil
	"Meme-related options."
	:group 'multimedia)

(defcustom meme-dir (expand-file-name "images" (file-name-directory (locate-library "meme")))
	"The directory with meme files."
	:group 'meme
	:type 'directory)

(defcustom meme-width 1200
  "The width of the meme images that are generated."
	:group 'meme
	:type 'natnum)

(defcustom meme-mp4-output-width 1200
  "Resulting width of mp4 files."
	:group 'meme
	:type 'natnum)

(defcustom meme-font "impact"
	"Font to use."
	:group 'meme
	:type 'string)

(defvar meme-svg)
(defvar meme-animation)
(defvar meme-column)
(defvar meme--timer nil)

(defvar meme-trim-gif t
  "If non-nil, trim images before making gifs.")

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

(defun meme-gif (directory match &optional extra-overlay)
  "Create a animated meme GIF interactively in Emacs."
  (interactive "DSource directory: \nsMatching files (regexp): ")
  (switch-to-buffer (get-buffer-create "*meme*"))
  (meme-mode)
  (when meme--timer
    (cancel-timer meme--timer)
    (setq meme--timer nil))
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
      (plist-put elem :crop (meme--find-crop
			     (mapcar (lambda (f)
				       (expand-file-name f directory))
				     files)))
      (plist-put elem :files (mapcar (lambda (f)
				       (meme--image-data 
					(expand-file-name f directory)
					"image/jpeg"
					(plist-get elem :crop)))
				     files))
      (plist-put elem :file-names (mapcar (lambda (f)
					    (expand-file-name f directory))
					  files))
      (plist-put elem :directory directory)
      (plist-put elem :extra-overlay extra-overlay)
      (plist-put elem :size (image-size (create-image
					 (with-temp-buffer
					   (meme--insert-and-trim
					    (expand-file-name (car files)
							      directory)
					    (plist-get elem :crop))
					   (buffer-string))
					 nil t
					 :scale 1)
					t))
      (plist-put elem :buffer (current-buffer))
      (plist-put elem :timestamp (float-time))
      (plist-put elem :index 0)
      (plist-put elem :direction 'forward)
      (plist-put elem :end (meme--text-input "end" 4
					     (format "%d" (length files))))
      (plist-put elem :skip (meme--text-input "skip" 4 "0"))
      (plist-put elem :rate (meme--text-input "rate" 4 "25"))
      (plist-put elem :mode (meme--text-input "mode" 8 "restart"))
      (insert "\n")
      (meme--animate meme-data elem)
      (setq-local meme-animation (list meme-data elem)))
    (eww-size-text-inputs)
    (add-hook 'after-change-functions #'eww-process-text-input nil t)
    nil))

(defun meme--find-crop (files)
  (let ((crops nil))
    (dolist (file files)
      (with-temp-buffer
	(call-process
	 "convert" nil '(t nil) nil
	 "-trim" "-fuzz" watch-directory-trim-fuzz
	 file "info:-")
	(let* ((data (seq-take
		      (nreverse
		       (seq-take (nreverse
				  (split-string (string-trim
						 (buffer-string))))
				 7))
		      2))
	       (left (string-to-number (nth 1 (split-string (cadr data)
							    "[+-]"))))
	       (top (string-to-number (nth 2 (split-string (cadr data)
							   "[+-]"))))
	       (width (string-to-number
		       (car (split-string (car data) "x"))))

	       (height (string-to-number
			(cadr (split-string (car data) "x")))))
	  (push (list width height left top) crops))))
    ;; We now have a number of crops -- pick the most likely
    ;; one.  Sort by width first.
    (setq crops (sort crops
		      (lambda (c1 c2)
			(< (car c1) (car c2)))))
    ;; Pick the ones with the median size.
    (let* ((m (nth (/ (length crops) 2) crops))
	   (median (* (car m) (cadr m))))
      (setq crops (seq-filter (lambda (c)
				(= (* (car c) (cadr c)) median))
			      crops)))
    ;; Pick the median offset.
    (setq crops (sort crops
		      (lambda (c1 c2)
			(< (nth 2 c1) (nth 2 c2)))))
    (nth (/ (length crops) 2) crops)))

(defun meme--insert-and-trim (file crop)
  (set-buffer-multibyte nil)
  (insert-file-contents file)
  (sleep-for 0.01)
  (when meme-trim-gif
    (call-process-region (point-min) (point-max) "convert" t (current-buffer)
			 nil
			 "-crop" (apply #'format "%dx%d+%d+%d" crop)
			 "jpg:-" "jpg:-")))

(defun meme--image-data (image itype crop)
  (with-temp-buffer
    (meme--insert-and-trim image crop)
    (base64-encode-region (point-min) (point-max) t)
    (goto-char (point-min))
    (insert "data:" itype ";base64,")
    (buffer-string)))

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
  (let* ((dir meme-dir)
	 (pixels (truncate (* 180 (image-compute-scaling-factor))))
	 (width (/ (- (nth 2 (window-pixel-edges))
		      (nth 0 (window-pixel-edges))
		      20)
		   pixels))
	 (inhibit-read-only t)
	 (i 0)
	 image-width image)
    (erase-buffer)
    (dolist (file (directory-files dir t ".jpg\\'"))
      (when (and (not (bolp))
		 (not (= image-width pixels)))
	(insert (propertize " " 'display
			    `(space :align-to (,(* (mod i width) pixels)))
			    'meme-intangible t)))
      (setq image (create-image file (meme--image-type) nil
				:max-width pixels
				:max-height pixels))
      (setq image-width (car (image-size image t)))
      (insert-image image " ")
      (add-text-properties (1- (point)) (point)
			   (list 'keymap meme--select-map
				 'file file))
      (when (zerop (mod (cl-incf i) width))
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
			   (format "%s-size" name) 4 "60"))
    (plist-put elem :color (meme--text-input
			    (format "%s-color" name) 8 "white"))
    (plist-put elem :align (meme--text-input
			    (format "%s-align" name) 8 "middle"))
    (plist-put elem :family (meme--text-input
			     (format "%s-family" name) 10 meme-font))
    (insert "\n")
    elem))

(defun meme--image-type-for-svg (file)
  (concat
   "image/"
   (downcase
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (call-process "identify" nil t nil "-format" "%m" (expand-file-name file))
      (buffer-string)))))

(defun meme--setup-image (file)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let* ((image-scaling-factor 1)
	 (image (create-image file (meme--image-type)))
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
		   (meme--image-type)
		   nil :max-width 120)
		  " ")
    (svg-embed svg file (meme--image-type-for-svg file) nil
	       :width width
	       :height (round height))
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
    (let* ((inhibit-read-only t)
	   (image-scaling-factor 1))
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
      (cl-decf y-offset (* font-size (1- (length bits)))))
    (dotimes (i 10)
      (dolist (version '("a" "b"))
	(svg-remove svg (format "%s-%d-%s" (plist-get elem :name) i version))))
    (dolist (bit bits)
      ;; The idea here is that we render the text twice: First with a
      ;; black stroke, and then with a totally see-through stroke.
      ;; This should remove strokes-on-top-of-fills on overlapping
      ;; characters.
      (dolist (type '(("a" 8 1) ("b" 1 0)))
	(svg-text svg bit
		  :font-size font-size
		  :stroke "black"
		  :stroke-opacity (nth 2 type)
		  :fill (meme--value data :color)
		  :font-family (meme--value data :family)
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
      (cl-incf i))))

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

(defun meme-save-or-upload (&optional exit)
  "Save the meme or upload it to imgur."
  (interactive "P")
  (cond
   (meme-animation
    (let ((out
	   (meme--save-animation
	    (if exit
		'mp4
	      (intern
	       (cadr
		(read-multiple-choice
		 "Format?"
		 '((?g "gif")
		   (?m "mp4")
		   (?w "webp"))))))
	    (read-file-name "Save to file: "))))
      (when exit
	(kill-buffer (current-buffer))
	(insert
	 (format "<video autoplay loop muted poster=%S><source src=%S type=\"video/mp4\"></video>\n\n"
		 (cdr out)
		 (car out))))))
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
	       (comp-delay (/ 1.0 (max (meme--value data :rate t) 1)))
	       (at-time (max 0.001 (- comp-delay (- delay comp-delay)))))
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
		(cl-incf (cl-getf data :index) (1+ (meme--value data :skip t)))
		(when (>= (plist-get data :index) (meme--value data :end t))
		  (if (not (equal (meme--value data :mode) "restart"))
		      (setf (cl-getf data :direction) 'backward
			    (cl-getf data :index) (1- (meme--value data :end t)))
		    (setf (cl-getf data :index) (meme--value data :start t)))))
	    (cl-decf (cl-getf data :index) (1+ (meme--value data :skip t)))
	    (when (<= (plist-get data :index) (meme--value data :start t))
	      (setf (cl-getf data :direction) 'forward
		    (cl-getf data :index) (1+ (meme--value data :start t)))))
	  (setf (cl-getf data :timestamp) (float-time))
	  (meme--update-image meme-data
			      (elt (cl-getf data :files) (cl-getf data :index))
			      (cl-getf data :size)
			      data)
	  (setq meme--timer
		(run-at-time
		 at-time
		 nil
		 (lambda ()
		   (meme--animate meme-data data)))))
	(goto-char (point))))))

(defun meme--update-image (meme-data base64 size data)
  (let ((inhibit-read-only t)
	(image (meme--make-animated-image meme-data base64 size
					  nil data)))
    (goto-char (point-min))
    (forward-line 6)
    (delete-region (point) (point-max))
    (insert "\n")
    (insert-image (create-image image 'svg t :scale 1))
    (set-buffer-modified-p nil)))

(defun meme--make-animated-image (meme-data base64 size &optional width data)
  (let* ((width (or width meme-width))
	 (height (* (cdr size) (/ width (float (car size)))))
	 (svg (svg-create width height)))
    (meme--update-meme svg
		       (plist-get meme-data :height)
		       (plist-get meme-data :top)
		       (plist-get meme-data :bottom)
		       t)
    (when-let ((extra (plist-get data :extra-overlay)))
      (funcall extra svg width height))
    (with-temp-buffer
      (svg-print svg)
      (goto-char (point-min))
      (search-forward "/xlink\">")
      ;; Use this instead of `svg-embed' for the image, because it's
      ;; much faster.
      (insert "<image xlink:href=\"")
      (insert base64)
      (insert (format "\" height=\"%s\" width=\"%s\"></image> "
		      height
		      width))
      (buffer-string))))

(defun meme--save-animation (format file)
  (let* ((files-name (make-temp-file "anim-"))
	 (temp-files (list files-name))
	 (img-files nil)
	 (prefix (concat (file-name-nondirectory files-name) "-"))
	 (meme-data (car meme-animation))
	 (findex 0)
	 (data (cadr meme-animation)))
    (with-temp-buffer
      (cl-loop for index from (meme--value data :start t) upto
	       (1- (meme--value data :end t)) by (1+ (meme--value data :skip t))
	       do (push (meme--write-animated-image
		         prefix (cl-incf findex)
		         meme-data data index
		         meme-mp4-output-width)
		        img-files))
      (unless (equal (meme--value data :mode) "restart")
	(cl-loop for index from (- (meme--value data :end t) 2)
	         downto (1+ (meme--value data :start t))
	         by (1+ (meme--value data :skip t))
	         do (push (meme--write-animated-image
			   prefix (cl-incf findex)
			   meme-data data index
			   meme-mp4-output-width)
		          img-files)))
      (write-region (point-min) (point-max) files-name nil 'silent))
    (setq file (format "%s.%s" file format))
    (when (file-exists-p file)
      (delete-file file))
    (cond
     ((eq format 'mp4)
      (call-process "ffmpeg" files-name (get-buffer-create "*convert*") nil
		    "-r" (meme--value data :rate)
		    "-f" "image2"
		    "-s" "1920x1080"
		    "-i" (concat (temporary-file-directory) prefix "%04d.png")
		    "-vcodec" "libx264"
		    "-crf" "25"
		    "-vf" "pad=ceil(iw/2)*2:ceil(ih/2)*2"
		    "-pix_fmt" "yuv420p"
		    (expand-file-name file)))
     ((or (eq format 'gif) (eq format 'webp))
      (call-process "convert" nil (get-buffer-create "*convert*") nil
		    "-dispose" "none"
		    ;; Our delay is in frames per second, but
		    ;; "convert"s is in 100ths of a second.
		    "-delay"
		    (format "%d" (* (/ 1.0 (max (meme--value data :rate t) 1))
				    100))
		    (format "@%s" files-name)
		    "-coalesce"
		    "-loop" "0"
		    (expand-file-name file))))
    (setq img-files (nreverse img-files))
    (mapc #'delete-file (append (cdr img-files) temp-files))
    (message "%s created" file)
    ;; Return a cons of the anim file and the first image (to be used
    ;; as a poster).
    (cons file (car img-files))))

(defun meme--write-animated-image (prefix findex meme-data data index
					  width)
  (let ((file (format "%s%s%04d.png" (temporary-file-directory) prefix findex)))
    (insert (format "'%s'\n" file))
    (sleep-for 0.01)
    (if (and (zerop (length (meme--value (plist-get meme-data :top) :text)))
	     (zerop (length (meme--value (plist-get meme-data :bottom) :text)))
	     (null (plist-get data :extra-overlay)))
	(if meme-trim-gif
	    (call-process "convert" nil nil nil
			  "-crop" (apply #'format "%dx%d+%d+%d"
					 (cl-getf data :crop))
			  (elt (cl-getf data :file-names) index) file)
	  (call-process "convert" nil nil nil
			(elt (cl-getf data :file-names) index) file))
      (with-temp-buffer
	(insert (meme--make-animated-image
		 meme-data (elt (cl-getf data :files) index)
		 (cl-getf data :size)
		 width data))
	(call-process-region (point-min) (point-max) "convert"
			     nil nil nil "svg:-"
			     file)))
    file))

(defun meme--image-type ()
  (if (or (and (fboundp 'image-transforms-p)
	       (image-transforms-p))
	  (not (fboundp 'imagemagick-types)))
      nil
    'imagemagick))

(provide 'meme)

;;; meme.el ends here
