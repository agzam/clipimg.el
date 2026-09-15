;;; clipimg.el --- Act on the image in the clipboard -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: September 14, 2026
;; Version: 0.1.0
;; Keywords: multimedia tools
;; Homepage: https://github.com/agzam/clipimg.el
;; Package-Requires: ((emacs "29.1") (transient "0.7.0"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; `clipimg' reads the image on the system clipboard and opens a menu of
;; things to do with it.  Text recognition is what the menu offers today.
;;
;; This file holds the clip, the clipboard readers and the file the
;; readers' bytes are written to.  A clip is read once and every command
;; works on that snapshot, so changing the clipboard afterwards breaks
;; nothing.

;;; Code:

(require 'cl-lib)
(require 'seq)
;; if-let* and when-let* live here until Emacs 30 moves them into subr.
(require 'subr-x)

;; An Emacs built without window-system support carries no `image-size'.
(declare-function image-size "image.c" (spec &optional pixels frame))

(defgroup clipimg nil
  "Act on the image in the system clipboard."
  :group 'multimedia
  :prefix "clipimg-")

(defcustom clipimg-clipboard-types
  '(image/png image/jpeg image/tiff image/gif image/webp)
  "Clipboard media types to read, the most wanted one first.
Window systems differ in what they offer: X and Wayland usually list
`image/png', a macOS pasteboard may carry only `image/tiff'."
  :type '(repeat symbol)
  :group 'clipimg)

(cl-defstruct (clipimg-clip (:constructor clipimg-clip-create)
                            (:copier nil))
  "An image taken off the clipboard, with what is known about it."
  data type width height time path)

(defvar clipimg--temp-files nil
  "Files written for clips in this session, newest first.")


;;;; Reading the clipboard

(defun clipimg--image-bytes (data)
  "Return DATA when it is a string holding an image Emacs recognizes."
  (and (stringp data)
       (< 0 (length data))
       (image-type-from-data data)
       data))

(defun clipimg--selection-bytes ()
  "Return image bytes from the window system clipboard, or nil.
A terminal frame owns no selection, so nothing is read there."
  (when (display-graphic-p)
    (let ((targets (ignore-errors (gui-get-selection 'CLIPBOARD 'TARGETS))))
      (seq-some (lambda (type)
                  (and (seq-contains-p targets type)
                       (clipimg--image-bytes
                        (ignore-errors (gui-get-selection 'CLIPBOARD type)))))
                clipimg-clipboard-types))))

(defun clipimg--command-bytes (program &rest args)
  "Return what PROGRAM run with ARGS writes to standard output, as raw bytes.
Return nil when PROGRAM is absent, fails, or writes nothing."
  (when-let* ((executable (executable-find program)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary))
        (and (zerop (apply #'call-process executable nil '(t nil) nil args))
             (< 0 (buffer-size))
             (buffer-string))))))

(defun clipimg--file-bytes (file)
  "Return the contents of FILE as raw bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defconst clipimg--pasteboard-script "\
ObjC.import('AppKit');
const path = ObjC.unwrap($.NSProcessInfo.processInfo.environment.objectForKey('CLIPIMG_FILE'));
const items = $.NSPasteboard.generalPasteboard.pasteboardItems;
if (items.count > 0) {
  const item = items.objectAtIndex(0);
  const types = item.types;
  for (let i = 0; i < types.count; i++) {
    const name = ObjC.unwrap(types.objectAtIndex(i));
    if (name.indexOf('image') === -1 &&
        name.indexOf('tiff') === -1 &&
        name.indexOf('png') === -1) continue;
    const image = $.NSImage.alloc.initWithData(item.dataForType(types.objectAtIndex(i)));
    if (!image.isValid) continue;
    const rep = $.NSBitmapImageRep.imageRepWithData(image.TIFFRepresentation);
    rep.representationUsingTypeProperties(4, $()).writeToFileAtomically(path, true);
    break;
  }
}"
  "JavaScript that writes the macOS pasteboard image to CLIPIMG_FILE as PNG.
The path arrives in the environment so that no file name is ever pasted
into the script.  The 4 is NSBitmapImageFileTypePNG.")

(defun clipimg--pasteboard-bytes ()
  "Return the macOS pasteboard image as PNG bytes, or nil."
  (when (executable-find "osascript")
    (let ((file (make-temp-file "clipimg-pasteboard-" nil ".png")))
      (unwind-protect
          (let ((process-environment
                 (cons (concat "CLIPIMG_FILE=" file) process-environment)))
            (call-process "osascript" nil nil nil
                          "-l" "JavaScript" "-e" clipimg--pasteboard-script)
            (and (< 0 (or (file-attribute-size (file-attributes file)) 0))
                 (clipimg--file-bytes file)))
        (delete-file file)))))

(defun clipimg--helper-bytes ()
  "Return image bytes read by a clipboard helper program, or nil.
This is the path a terminal frame takes, where Emacs owns no selection."
  (clipimg--image-bytes
   (cond ((eq system-type 'darwin)
          (clipimg--pasteboard-bytes))
         ((getenv "WAYLAND_DISPLAY")
          (clipimg--command-bytes "wl-paste" "--no-newline" "--type" "image/png"))
         ((getenv "DISPLAY")
          (clipimg--command-bytes "xclip" "-selection" "clipboard"
                                  "-t" "image/png" "-o")))))

(defun clipimg--dimensions (data)
  "Return (WIDTH . HEIGHT) in pixels for image DATA, or nil.
Without `:scale 1' a HiDPI frame reports the scaled size, and a terminal
frame cannot measure an image at all."
  (ignore-errors
    (let ((size (image-size (create-image data nil t :scale 1) t)))
      (cons (truncate (car size)) (truncate (cdr size))))))

;;;###autoload
(defun clipimg-clipboard-clip ()
  "Return a clip for the image on the clipboard, or nil when there is none."
  (when-let* ((data (or (clipimg--selection-bytes) (clipimg--helper-bytes))))
    (let ((size (clipimg--dimensions data)))
      (clipimg-clip-create :data data
                           :type (image-type-from-data data)
                           :width (car size)
                           :height (cdr size)
                           :time (current-time)))))

(defun clipimg-clipboard-clip-or-error ()
  "Return a clip for the image on the clipboard, or signal a `user-error'."
  (or (clipimg-clipboard-clip)
      (user-error "No image on the clipboard")))


;;;; Clips on disk

(defun clipimg--delete-temp-files ()
  "Delete the files written for clips in this session."
  (dolist (file clipimg--temp-files)
    (when (file-exists-p file)
      (ignore-errors (delete-file file))))
  (setq clipimg--temp-files nil))

(defun clipimg--write-temp (data type)
  "Write image DATA of image TYPE to a temporary file and return its name."
  (let ((file (make-temp-file "clipimg-" nil (format ".%s" (or type "img"))))
        (coding-system-for-write 'binary))
    (write-region data nil file nil 'silent)
    (unless clipimg--temp-files
      (add-hook 'kill-emacs-hook #'clipimg--delete-temp-files))
    (push file clipimg--temp-files)
    file))

(defun clipimg-clip-file (clip)
  "Return the file holding CLIP's bytes, writing it on first use."
  (or (clipimg-clip-path clip)
      (setf (clipimg-clip-path clip)
            (clipimg--write-temp (clipimg-clip-data clip)
                                 (clipimg-clip-type clip)))))

(defun clipimg-clip-filename (clip &optional format)
  "Return a file name for CLIP, stamped with the time it was taken.
FORMAT names the extension; without one, the format CLIP is in.  Every
command that gives the image a name of its own uses this one, so a file
saved and a file uploaded a moment apart are named alike."
  (concat (format-time-string "clipimg-%Y%m%d-%H%M%S" (clipimg-clip-time clip))
          "." (symbol-name (or format (clipimg-clip-type clip) 'png))))


;;;; Converting

(defconst clipimg--converters
  '(("magick") ("gm" "convert") ("convert"))
  "Command lines able to turn one image format into another, best first.
Each takes two further arguments: the format going in and the format
coming out, both as FORMAT:- for standard input and standard output.")

(defun clipimg-converter ()
  "Return the command line of the first converter on PATH, or nil."
  (seq-find (lambda (command) (executable-find (car command)))
            clipimg--converters))

(defun clipimg-convert (data from format)
  "Return image DATA of format FROM as FORMAT.
Emacs has `image-convert' for this, and it cannot be trusted with a file
a user keeps: it asks ffmpeg first, which knows no format named jpeg, and
its ImageMagick path lets the converter's standard error into the image
data, where ImageMagick 7 puts its warning about the name `convert'."
  (let ((command (or (clipimg-converter)
                     (user-error "%s needs ImageMagick or GraphicsMagick"
                                 (upcase (symbol-name format))))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary))
        (unless (zerop (apply #'call-process-region (point-min) (point-max)
                              (car command) t '(t nil) nil
                              (append (cdr command)
                                      (list (format "%s:-" from)
                                            (format "%s:-" format)))))
          (user-error "%s made no %s of the image" (car command) format)))
      (let ((converted (buffer-string)))
        (unless (eq (image-type-from-data converted) format)
          (user-error "%s answered with something that is not a %s image"
                      (car command) format))
        converted))))

(defun clipimg-clip-format (clip &optional format)
  "Return FORMAT when CLIP can be had in it, or the format CLIP is in.
A format nothing on PATH can make is no use to a caller, so the clip's
own stands in for it."
  (let ((type (or (clipimg-clip-type clip) 'png)))
    (if (and format (or (eq format type) (clipimg-converter)))
        format
      type)))

(defun clipimg-clip-bytes (clip &optional format)
  "Return the bytes of CLIP in FORMAT, converting only when it must."
  (let ((type (or (clipimg-clip-type clip) 'png)))
    (if (or (null format) (eq format type))
        (clipimg-clip-data clip)
      (clipimg-convert (clipimg-clip-data clip) type format))))


;;;; Describing a clip

(defun clipimg-clip-summary (clip)
  "Return a one line description of CLIP."
  (let ((width (clipimg-clip-width clip))
        (height (clipimg-clip-height clip)))
    (concat (upcase (symbol-name (or (clipimg-clip-type clip) 'image)))
            (if (and width height) (format " %dx%d" width height) "")
            ", "
            (file-size-human-readable (length (clipimg-clip-data clip))))))

(defcustom clipimg-preview-max-width 600
  "Width in pixels a clip preview is shrunk to."
  :type 'natnum
  :group 'clipimg)

(defun clipimg-clip-preview (clip)
  "Return CLIP's summary carrying the image as a display property.
A graphical frame draws the image, a terminal frame draws the summary."
  (let ((summary (clipimg-clip-summary clip)))
    (if-let* ((image (ignore-errors
                       (create-image (clipimg-clip-data clip) nil t
                                     :scale 1
                                     :max-width clipimg-preview-max-width))))
        (propertize summary 'display image)
      summary)))

(provide 'clipimg)
;;; clipimg.el ends here
