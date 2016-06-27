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

;;; Code:

(require 'cl)
(require 'eww)

(defun meme ()
  "Create a meme image interactively in Emacs."
  (interactive)
  (pop-to-buffer (get-buffer-create "*meme*"))
  (meme-mode)
  (meme--insert-thumbnails))

(defvar meme-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    ;;(define-key map " " 'archive-next-line)
    map))

(define-derived-mode meme-mode meme-mode "Meme"
  "Major mode for creating meme images.

\\{meme-mode-map}"
  
  )

(defvar meme--select-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'meme-select-image)
    map))

(defun meme--insert-thumbnails ()
  (let* ((dir (expand-file-name
	       "images" (file-name-directory (locate-library "meme"))))
	 (pixels 180)
	 (width (/ (- (nth 2 (window-pixel-edges))
		      (nth 0 (window-pixel-edges))
		      20)
		   pixels))
	 (i 0))
    (erase-buffer)
    (dolist (file (directory-files dir t ".jpg\\'"))
      (unless (bolp)
	(insert (propertize " " 'display
			    `(space :align-to (,(* (mod i width) pixels)))
			    'intangible t)))
      (insert-image (create-image file 'imagemagick nil
				  :max-width pixels
				  :max-height pixels)
		    " ")
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
    (meme--setup-image file)))

(defun meme--setup-image (file)
  (let* ((image (create-image file 'imagemagick))
	 (image-size (image-size image t))
	 (width 400)
	 (height (* (cdr image-size) (/ width (float (car image-size)))))
	 (svg (svg-create width height
			  :xmlns:xlink "http://www.w3.org/1999/xlink"))
	 (inhibit-read-only t)
	 top bottom data)
    (erase-buffer)
    (insert "Top    ")
    (setq top (point))
    (eww-form-text (dom-node 'input '((size . "60") (name . "top"))))
    (insert "\n")
    (insert "Bottom ")
    (setq bottom (point))
    (eww-form-text (dom-node 'input '((size . "60") (name . "bottom"))))
    (insert "\n")
    (svg-insert-image svg)
    (svg-embed svg file "image/jpeg" nil
	       :width width
	       :height height)
    (eww-size-text-inputs)
    (goto-char top)
    (setq after-change-functions nil)
    (add-hook 'after-change-functions #'eww-process-text-input nil t)
    (setq-local post-command-hook nil)
    (setq buffer-read-only t)
    (add-hook 'post-command-hook
	      (lambda ()
		(meme--update-meme svg
				   (get-text-property top 'eww-form)
				   (get-text-property bottom 'eww-form))))
    nil))

(defun meme--update-meme (svg top bottom)
  (let* ((string (upcase (string-trim (plist-get top :value))))
	 (inhibit-read-only t)
	 (bits (meme--fold-string string))
	 (i 0))
    (dotimes (i 10)
      (svg-remove svg (format "%s-%d" (plist-get top :name) i)))
    (dolist (bit bits)
      (svg-text svg bit
		:font-size "40"
		:font-weight "bold"
		:stroke "black"
		:fill "white"
		:font-family "impact"
		:letter-spacing "-3pt"
		:x (- (/ 400 2) (/ (meme--text-width bit) 2))
		:y (+ 50 (* i 40))
		:stroke-width 1
		:id (format "%s-%d" (plist-get top :name) i))
      (incf i))))

(defun meme--fold-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((prev (point)))
      (while (meme--next-point)
	(when (> (meme--text-width (buffer-substring (line-beginning-position)
						     (point)))
		 380)
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

(defun meme--text-width (string)
  (let ((ratio (/ (float (meme--text-pixels "x")) 17)))
    (/ (meme--text-pixels string) ratio)))

(defun meme--text-pixels (string)
  (with-temp-buffer
    (insert (propertize string 'face '(:family "impact" :height 400)))
    (let ((shr-use-fonts t))
      (shr-pixel-column))))
    
(provide 'meme)

;;; meme.el ends here
