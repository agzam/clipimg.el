;;; clipimg-save.el --- Put a clipboard image on disk -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: September 15, 2026
;; Keywords: multimedia tools
;; URL: https://github.com/agzam/clipimg.el
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Saving for `clipimg'.  One prompt, starting in `clipimg-save-directory'
;; under a name stamped with the time the clip was taken.  The extension
;; typed at that prompt decides the format: the one the clip is already in
;; costs nothing, any other one goes through ImageMagick or GraphicsMagick.
;;
;; Nothing lands on disk before `image-type-from-data' has called it the
;; format that was asked for.  A converter that answers with anything else
;; is an error, never a file.

;;; Code:

(require 'image)
(require 'seq)
(require 'subr-x)
(require 'clipimg)

(defcustom clipimg-save-directory "~/Pictures/"
  "Directory the save prompt starts in."
  :type 'directory
  :group 'clipimg)

(defcustom clipimg-save-default-format 'png
  "Format the name at the save prompt carries, or nil for the clip's own.
Whatever a user types over that name still decides the format.  A
clipboard is not always asked what it holds: a Qt program on macOS
publishes every capture as TIFF, several times the size of the same
image as PNG, and this is what keeps a save out of it."
  :type '(choice (const :tag "The format of the clip" nil) symbol)
  :group 'clipimg)


;;;; The format a name asks for

(defun clipimg-save--format (name default)
  "Return the image format the extension of NAME asks for, or DEFAULT.
Emacs names an image by its type, so the extensions people type for two
of them arrive under another name."
  (if-let* ((extension (file-name-extension name)))
      (let ((format (downcase extension)))
        (intern (cond ((equal format "jpg") "jpeg")
                      ((equal format "tif") "tiff")
                      (t format))))
    default))



;;;; What the prompt says while a name is typed

(defun clipimg-save--known-formats ()
  "Return every format Emacs can name by looking at an image's bytes.
This is the set a conversion is checked against, and it says nothing
about what a build can display, so a headless Emacs answers the same as
a windowed one."
  (delete-dups (mapcar #'cdr image-type-header-regexps)))

(defun clipimg-save-format-problem (name clip)
  "Return why a save of CLIP under NAME cannot happen, or nil when it can.
An extension Emacs does not know as an image is half typed rather than
wrong, so it passes without a word."
  (let ((format (clipimg-save--format name nil)))
    (when (and format
               (memq format (clipimg-save--known-formats))
               (not (eq format (clipimg-clip-format clip format))))
      (format "%s needs ImageMagick or GraphicsMagick"
              (upcase (symbol-name format))))))

(defun clipimg-save--name-watcher (clip)
  "Return a function saying when the name being typed cannot hold CLIP.
The minibuffer is the only place to say it: by the time the prompt is
answered the user has typed the extension and moved on."
  (let ((said 'none))
    (lambda (&rest _)
      (let ((problem (clipimg-save-format-problem (minibuffer-contents) clip)))
        (unless (equal problem said)
          (setq said problem)
          (when problem
            (minibuffer-message "%s" problem)))))))


;;;; Saving

(defun clipimg-save-write (clip file)
  "Write CLIP to FILE in the format the name of FILE asks for.
Nothing is asked here, so a command calling this should have asked
already.  Return the name written to."
  (let* ((file (expand-file-name file))
         (format (clipimg-save--format
                  file (or (clipimg-clip-type clip) 'png)))
         (data (clipimg-clip-bytes clip format))
         (coding-system-for-write 'binary))
    (make-directory (file-name-directory file) t)
    (write-region data nil file nil 'silent)
    file))

(defun clipimg-save--read-file-name (clip)
  "Ask for a name to write CLIP under, stamped with the time it was taken."
  (minibuffer-with-setup-hook
      (lambda ()
        (add-hook 'after-change-functions (clipimg-save--name-watcher clip) nil t))
    (read-file-name "Save image as: "
                    (file-name-as-directory
                     (expand-file-name clipimg-save-directory))
                    nil nil
                    (clipimg-clip-filename
                     clip (clipimg-clip-format clip clipimg-save-default-format)))))

(defun clipimg-save-file (clip &optional file)
  "Write CLIP to FILE and return the name written to.
Ask for a name when FILE is nil, and ask before replacing a file that is
already there."
  (let ((file (or file (clipimg-save--read-file-name clip))))
    (unless (or (not (file-exists-p file))
                (y-or-n-p (format "Overwrite %s? " (abbreviate-file-name file))))
      (user-error "Nothing saved"))
    (clipimg-save-write clip file)
    (message "clipimg: saved %s" (abbreviate-file-name file))
    file))

;;;###autoload
(defun clipimg-save ()
  "Put the image on the clipboard in a file."
  (interactive)
  (clipimg-save-file (clipimg-clipboard-clip-or-error)))

(provide 'clipimg-save)
;;; clipimg-save.el ends here
