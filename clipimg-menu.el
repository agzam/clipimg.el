;;; clipimg-menu.el --- The clipimg menu -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: September 14, 2026
;; Keywords: multimedia tools
;; URL: https://github.com/agzam/clipimg.el
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; `clipimg' is the entry point: it reads the image on the clipboard and
;; opens a transient on that one snapshot.  Changing the clipboard while
;; the menu is open changes nothing; `r' takes a fresh snapshot.

;;; Code:

(require 'subr-x)
(require 'transient)
(require 'clipimg)
(require 'clipimg-ocr)
(require 'clipimg-upload)

(defvar clipimg-menu--clip nil
  "The clip the open menu acts on.")


;;;; Reading the menu

(defun clipimg-menu--args ()
  "Return the current argument strings, during setup or later."
  (cond ((and transient--prefix (slot-boundp transient--prefix 'value))
         (oref transient--prefix value))
        (transient--suffixes (transient-get-value))
        (t (transient-args 'clipimg))))

(defun clipimg-menu--backend ()
  "Return the backend the menu names, or the one in effect."
  (if-let* ((value (transient-arg-value "--backend=" (clipimg-menu--args))))
      (intern value)
    (clipimg-ocr-backend)))

(defun clipimg-menu--language ()
  "Return the language the menu names, or nil."
  (transient-arg-value "--language=" (clipimg-menu--args)))

(defun clipimg-menu--layout ()
  "Return the layout the menu names, or the one in effect."
  (if-let* ((value (transient-arg-value "--layout=" (clipimg-menu--args))))
      (intern value)
    clipimg-ocr-layout))

(defun clipimg-menu--service ()
  "Return the upload service the menu names, or the one in effect."
  (if-let* ((value (transient-arg-value "--service=" (clipimg-menu--args))))
      (intern value)
    clipimg-upload-service))

(defun clipimg-menu--origin-buffer ()
  "Return the buffer the menu was opened from, or the current one."
  (if (buffer-live-p transient--original-buffer)
      transient--original-buffer
    (current-buffer)))

(defun clipimg-menu--origin-read-only-p ()
  "Return non-nil when the buffer the menu was opened from takes no insert."
  (buffer-local-value 'buffer-read-only (clipimg-menu--origin-buffer)))

(defun clipimg-menu--ocr-inapt-p ()
  "Return non-nil when the chosen backend cannot run."
  (and (clipimg-ocr-problem (clipimg-menu--backend)) t))


;;;; Header

(defun clipimg-menu--clipboard-line ()
  "Return the line naming the image the menu acts on."
  (concat (propertize "Clipboard: " 'face 'transient-heading)
          (if clipimg-menu--clip
              (clipimg-clip-summary clipimg-menu--clip)
            (propertize "empty" 'face 'transient-inactive-value))))

(defun clipimg-menu--problems ()
  "Return what stands in the way of a command, one string each."
  (delq nil (list (clipimg-ocr-problem (clipimg-menu--backend))
                  (clipimg-upload-problem (clipimg-menu--service)))))

(defun clipimg-menu--header (_children)
  "Return the header: what is on the clipboard, then anything in the way."
  (transient-parse-suffixes
   'clipimg
   (cons (list :info* (clipimg-menu--clipboard-line) :format "%d")
         (mapcar (lambda (problem)
                   (list :info* (propertize (concat "! " problem) 'face 'error)))
                 (clipimg-menu--problems)))))


;;;; Infixes

(defun clipimg-menu--label (label value default)
  "Render LABEL with VALUE, or with DEFAULT greyed out when VALUE is nil."
  (concat label ": "
          (if value
              (propertize value 'face 'transient-value)
            (propertize default 'face 'transient-inactive-value))))

(transient-define-infix clipimg-menu--backend-infix ()
  :class 'transient-option
  :key "-b"
  :argument "--backend="
  :prompt "Recognize text with: "
  :always-read t
  :choices (lambda () (mapcar #'symbol-name (clipimg-ocr-backend-names)))
  :format " %k %d"
  :description (lambda (object)
                 (clipimg-menu--label
                  "backend" (oref object value)
                  (clipimg-ocr-label (clipimg-ocr-backend)))))

(transient-define-infix clipimg-menu--language-infix ()
  :class 'transient-option
  :key "-l"
  :argument "--language="
  :prompt "Language: "
  :choices (lambda () (clipimg-ocr-languages (clipimg-menu--backend)))
  :format " %k %d"
  :description (lambda (object)
                 (clipimg-menu--label
                  "language" (oref object value)
                  (or clipimg-ocr-language "engine default"))))

(transient-define-infix clipimg-menu--service-infix ()
  :class 'transient-option
  :key "-s"
  :argument "--service="
  :prompt "Upload to: "
  :always-read t
  :choices (lambda () (mapcar #'symbol-name (clipimg-upload-service-names)))
  :format " %k %d"
  :description (lambda (object)
                 (clipimg-menu--label
                  "service" (oref object value)
                  (clipimg-upload-label clipimg-upload-service))))

(transient-define-infix clipimg-menu--layout-infix ()
  :class 'transient-switches
  :key "-y"
  :argument-format "--layout=%s"
  ;; Group 1 must be the whole argument: transient re-reads every infix
  ;; from the prefix value after each key.
  :argument-regexp "\\`\\(--layout=\\(?:block\\|sparse\\|auto\\)\\)\\'"
  :choices '("block" "sparse" "auto")
  :format " %k %d"
  :description (lambda (object)
                 (clipimg-menu--label
                  "layout"
                  (and-let* ((value (oref object value)))
                    (substring value (length "--layout=")))
                  (symbol-name clipimg-ocr-layout))))


;;;; Actions

(defun clipimg-menu--call (function)
  "Call FUNCTION with the clip of the menu and the options it names."
  (funcall function
           clipimg-menu--clip
           (clipimg-menu--backend)
           (clipimg-menu--language)
           (clipimg-menu--layout)))

(defun clipimg-menu-ocr-buffer ()
  "Show the text of the clipboard image in a buffer."
  (interactive)
  (clipimg-menu--call #'clipimg-ocr-to-buffer))

(defun clipimg-menu-ocr-kill-ring ()
  "Put the text of the clipboard image on the kill ring."
  (interactive)
  (clipimg-menu--call #'clipimg-ocr-to-kill-ring))

(defun clipimg-menu-ocr-insert ()
  "Insert the text of the clipboard image at point."
  (interactive)
  (with-current-buffer (clipimg-menu--origin-buffer)
    (clipimg-menu--call #'clipimg-ocr-insert)))

(defun clipimg-menu-upload ()
  "Upload the clipboard image and put the URL on the kill ring."
  (interactive)
  (clipimg-upload-to-kill-ring clipimg-menu--clip (clipimg-menu--service)))

(defun clipimg-menu-upload-insert ()
  "Upload the clipboard image and insert the URL at point."
  (interactive)
  (with-current-buffer (clipimg-menu--origin-buffer)
    (clipimg-upload-insert clipimg-menu--clip (clipimg-menu--service))))

(defun clipimg-menu-read-clipboard ()
  "Read the clipboard again, replacing the image the menu acts on."
  (interactive)
  (setq clipimg-menu--clip (clipimg-clipboard-clip-or-error))
  (message "clipimg: %s" (clipimg-clip-summary clipimg-menu--clip)))


;;;; The menu

;;;###autoload (autoload 'clipimg "clipimg-menu" nil t)
(transient-define-prefix clipimg ()
  "Act on the image on the system clipboard.
The image is read once, when the menu opens, so every command works on
that snapshot even after the clipboard moves on."
  :refresh-suffixes t
  [:class transient-column
   :setup-children clipimg-menu--header]
  [["Recognize text"
    (clipimg-menu--backend-infix)
    (clipimg-menu--language-infix)
    (clipimg-menu--layout-infix)
    ("RET" "to buffer" clipimg-menu-ocr-buffer
     :inapt-if clipimg-menu--ocr-inapt-p)
    ("w" "to kill ring" clipimg-menu-ocr-kill-ring
     :inapt-if clipimg-menu--ocr-inapt-p)
    ("i" "insert at point" clipimg-menu-ocr-insert
     :inapt-if (lambda ()
                 (or (clipimg-menu--ocr-inapt-p)
                     (clipimg-menu--origin-read-only-p))))]
   ["Upload"
    (clipimg-menu--service-infix)
    ("u" "to kill ring" clipimg-menu-upload)
    ("U" "insert URL at point" clipimg-menu-upload-insert
     :inapt-if clipimg-menu--origin-read-only-p)]]
  [:class transient-row
   ("r" "re-read clipboard" clipimg-menu-read-clipboard :transient t)]
  ;; The Return key arrives as this event while an active map binds it,
  ;; which evil's Org map does, and Emacs turns it into RET only when
  ;; none does.
  ["" :hide always
   ("<return>" "to buffer" clipimg-menu-ocr-buffer
    :inapt-if clipimg-menu--ocr-inapt-p)]
  (interactive)
  (setq clipimg-menu--clip (clipimg-clipboard-clip-or-error))
  (transient-setup 'clipimg))

(provide 'clipimg-menu)
;;; clipimg-menu.el ends here
