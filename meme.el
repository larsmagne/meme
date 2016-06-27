;;; meme.el --- meme interface
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

\\{meme-mode-map}")

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
			    `(space :align-to (,(* (mod i width) pixels))))))
      (insert-image (create-image file 'imagemagick nil
				  :max-width pixels
				  :max-height pixels)
		    " ")
      (when (zerop (mod (incf i) width))
	(insert "\n")))
    (goto-char (point-min))))

(provide 'meme)

;;; meme.el ends here
