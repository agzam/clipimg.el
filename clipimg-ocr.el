;;; clipimg-ocr.el --- Read the text in a clipboard image -*- lexical-binding: t; -*-
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

;; Text recognition for `clipimg'.  Engines live in
;; `clipimg-ocr-backends', one entry per engine, so adding one is data
;; rather than code.  Two ship: tesseract everywhere, and the Vision
;; framework on macOS, which needs nothing installed and knows thirty
;; languages.
;;
;; Vision hands back one box per stretch of text, out of reading order,
;; so the lines are assembled here from the boxes' coordinates.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'clipimg)

(defcustom clipimg-ocr-backends
  '((vision
     :label "Vision"
     :available-p clipimg-ocr--vision-available-p
     :languages clipimg-ocr--vision-languages
     :recognize clipimg-ocr--vision)
    (tesseract
     :label "tesseract"
     :available-p clipimg-ocr--tesseract-available-p
     :languages clipimg-ocr--tesseract-languages
     :recognize clipimg-ocr--tesseract))
  "Text recognition engines, in the order `auto' prefers them.
Each entry maps a symbol to a plist: `:label' names the engine for the
menu, `:available-p' takes no argument and says whether this machine can
run it, `:languages' returns the language names it accepts, and
`:recognize' takes a file, a language and a layout symbol and returns
the text it read."
  :type '(alist :key-type symbol :value-type plist)
  :group 'clipimg)

(defcustom clipimg-ocr-backend 'auto
  "Engine `clipimg' recognizes text with.
The default, `auto', takes the first entry of `clipimg-ocr-backends'
this machine can run, which is Vision on macOS and tesseract elsewhere."
  :type '(choice (const :tag "First available" auto) symbol)
  :group 'clipimg)

(defcustom clipimg-ocr-language nil
  "Language the engine should read, or nil for its own default."
  :type '(choice (const :tag "Engine default" nil) string)
  :group 'clipimg)

(defcustom clipimg-ocr-layout 'block
  "How the text of an image is laid out.
`block' reads it as lines of a single block, joining runs that sit side
by side.  `sparse' keeps every run on a line of its own, which suits
scattered labels.  `auto' leaves the engine to decide."
  :type '(choice (const block) (const sparse) (const auto))
  :group 'clipimg)

(defcustom clipimg-ocr-buffer-name "*clipimg OCR*"
  "Name of the buffer recognized text is shown in."
  :type 'string
  :group 'clipimg)


;;;; Backends

(defun clipimg-ocr--property (backend key)
  "Return property KEY of BACKEND in `clipimg-ocr-backends'."
  (plist-get (alist-get backend clipimg-ocr-backends) key))

(defun clipimg-ocr-backend-names ()
  "Return the symbol of every known backend, preferred first."
  (mapcar #'car clipimg-ocr-backends))

(defun clipimg-ocr-available-p (backend)
  "Return non-nil when BACKEND can run on this machine."
  (when-let* ((predicate (clipimg-ocr--property backend :available-p)))
    (funcall predicate)))

(defun clipimg-ocr-label (backend)
  "Return the name BACKEND is shown under."
  (or (clipimg-ocr--property backend :label) (symbol-name backend)))

(defun clipimg-ocr-languages (backend)
  "Return the languages BACKEND accepts, as strings."
  (when-let* ((languages (clipimg-ocr--property backend :languages)))
    (funcall languages)))

(defun clipimg-ocr-backend ()
  "Return the backend in effect, resolving `auto'."
  (if (eq clipimg-ocr-backend 'auto)
      (or (seq-find #'clipimg-ocr-available-p (clipimg-ocr-backend-names))
          (car (clipimg-ocr-backend-names)))
    clipimg-ocr-backend))

(defun clipimg-ocr-problem (backend)
  "Return why BACKEND cannot run, or nil when it can."
  (cond ((null (clipimg-ocr--property backend :recognize))
         (format "%s is not a known OCR backend" backend))
        ((not (clipimg-ocr-available-p backend))
         (format "%s is not available on this machine" (clipimg-ocr-label backend)))))

(defun clipimg-ocr-recognize (clip &optional backend language layout)
  "Return the text BACKEND recognizes in CLIP.
LANGUAGE and LAYOUT default to `clipimg-ocr-language' and
`clipimg-ocr-layout'."
  (let ((backend (or backend (clipimg-ocr-backend))))
    (when-let* ((problem (clipimg-ocr-problem backend)))
      (user-error "%s" problem))
    (funcall (clipimg-ocr--property backend :recognize)
             (clipimg-clip-file clip)
             (or language clipimg-ocr-language)
             (or layout clipimg-ocr-layout))))


;;;; tesseract

(defconst clipimg-ocr--tesseract-modes
  '((block . "6") (sparse . "11") (auto . "3"))
  "Page segmentation mode tesseract gets for each layout.")

(defvar clipimg-ocr--tesseract-language-cache nil
  "Languages tesseract reported, cached for the session.")

(defun clipimg-ocr--tesseract-available-p ()
  "Return non-nil when tesseract is on PATH."
  (and (executable-find "tesseract") t))

(defun clipimg-ocr--tesseract-languages ()
  "Return the languages tesseract has data files for."
  (or clipimg-ocr--tesseract-language-cache
      (setq clipimg-ocr--tesseract-language-cache
            (when-let* ((output (clipimg-ocr--output "tesseract" "--list-langs")))
              ;; The first line names the directory and the count.
              (seq-filter (lambda (line) (string-match-p "\\`[A-Za-z_-]+\\'" line))
                          (split-string output "\n" t))))))

(defun clipimg-ocr--tesseract (file language layout)
  "Return the text tesseract recognizes in FILE.
LANGUAGE is a tesseract language name or nil, LAYOUT one of the symbols
`clipimg-ocr-layout' takes."
  (let ((arguments (append (list file "-")
                           (when language (list "-l" language))
                           (list "--psm" (alist-get layout clipimg-ocr--tesseract-modes "6")))))
    (string-trim-right (or (apply #'clipimg-ocr--output "tesseract" arguments) ""))))


;;;; Vision

(defconst clipimg-ocr--vision-script "\
ObjC.import('Foundation');
ObjC.import('Vision');
const env = $.NSProcessInfo.processInfo.environment;
const path = ObjC.unwrap(env.objectForKey('CLIPIMG_FILE'));
const language = ObjC.unwrap(env.objectForKey('CLIPIMG_LANGUAGE'));
const handler = $.VNImageRequestHandler.alloc.initWithURLOptions(
  $.NSURL.fileURLWithPath(path), $());
const request = $.VNRecognizeTextRequest.alloc.init;
request.recognitionLevel = 0;
request.usesLanguageCorrection = true;
if (language) request.recognitionLanguages = $(language.split(','));
handler.performRequestsError($([request]), $());
const results = request.results;
const runs = [];
for (let i = 0; i < results.count; i++) {
  const observation = results.objectAtIndex(i);
  const candidate = observation.topCandidates(1).objectAtIndex(0);
  const box = observation.boundingBox;
  runs.push({text: ObjC.unwrap(candidate.string),
             x: box.origin.x, y: box.origin.y, height: box.size.height});
}
JSON.stringify(runs);"
  "JavaScript that reads the text of CLIPIMG_FILE with the Vision framework.
It prints one JSON object per text run, with the normalized bottom-left
bounding box Vision reports, and takes the languages from
CLIPIMG_LANGUAGE as a comma separated list.")

(defconst clipimg-ocr--vision-languages-script "\
ObjC.import('Vision');
const request = $.VNRecognizeTextRequest.alloc.init;
request.recognitionLevel = 0;
ObjC.unwrap(request.supportedRecognitionLanguagesAndReturnError($()))
  .map(function (language) { return ObjC.unwrap(language); })
  .join(' ');"
  "JavaScript that prints the languages the Vision text request accepts.")

(defvar clipimg-ocr--vision-language-cache nil
  "Languages Vision reported, cached for the session.")

(defun clipimg-ocr--vision-available-p ()
  "Return non-nil when the Vision framework can be reached."
  (and (eq system-type 'darwin) (executable-find "osascript") t))

(defun clipimg-ocr--osascript (script &rest environment)
  "Return what SCRIPT prints, with ENVIRONMENT added to the process environment.
ENVIRONMENT is a list of \"NAME=VALUE\" strings."
  (let ((process-environment (append environment process-environment)))
    (clipimg-ocr--output "osascript" "-l" "JavaScript" "-e" script)))

(defun clipimg-ocr--vision-languages ()
  "Return the languages the Vision text request accepts."
  (or clipimg-ocr--vision-language-cache
      (setq clipimg-ocr--vision-language-cache
            (when-let* ((output (clipimg-ocr--osascript
                                 clipimg-ocr--vision-languages-script)))
              (split-string output "[ \n]" t)))))

(defun clipimg-ocr--vision-boxes (output)
  "Return the text boxes the Vision script printed as OUTPUT."
  (unless (string-empty-p (string-trim output))
    (json-parse-string output :array-type 'list :object-type 'plist)))

(defun clipimg-ocr--vision (file language layout)
  "Return the text the Vision framework recognizes in FILE.
LANGUAGE is a comma separated list of Vision language names or nil,
LAYOUT one of the symbols `clipimg-ocr-layout' takes."
  (let ((output (clipimg-ocr--osascript
                 clipimg-ocr--vision-script
                 (concat "CLIPIMG_FILE=" (expand-file-name file))
                 (concat "CLIPIMG_LANGUAGE=" (or language "")))))
    (if output
        (clipimg-ocr--assemble (clipimg-ocr--vision-boxes output) layout)
      "")))


;;;; Assembling boxes into lines

(defun clipimg-ocr--same-line-p (one other)
  "Return non-nil when boxes ONE and OTHER sit on the same line.
They do when they overlap vertically by more than half the shorter
one, which separates a wrapped line from a second column."
  (let* ((one-bottom (plist-get one :y))
         (one-top (+ one-bottom (plist-get one :height)))
         (other-bottom (plist-get other :y))
         (other-top (+ other-bottom (plist-get other :height)))
         (overlap (- (min one-top other-top) (max one-bottom other-bottom))))
    (< (* 0.5 (min (plist-get one :height) (plist-get other :height)))
       overlap)))

(defun clipimg-ocr--lines (boxes)
  "Group BOXES, ordered top to bottom, into lines of side by side boxes."
  (let (lines line reference)
    (dolist (box boxes)
      (cond ((null line)
             (setq line (list box) reference box))
            ((clipimg-ocr--same-line-p reference box)
             (push box line))
            (t
             (push (nreverse line) lines)
             (setq line (list box) reference box))))
    (when line
      (push (nreverse line) lines))
    (nreverse lines)))

(defun clipimg-ocr--assemble (boxes layout)
  "Return the text of BOXES in reading order, as LAYOUT asks for it.
A box is a plist of `:text', `:x', `:y' and `:height', with normalized
coordinates measured from the bottom left.  Boxes side by side share a
line under `block', and take one line each under `sparse'."
  (let ((separator (if (eq layout 'sparse) "\n" " "))
        (ordered (sort (copy-sequence boxes)
                       (lambda (one other)
                         (< (plist-get other :y) (plist-get one :y))))))
    (mapconcat (lambda (line)
                 (mapconcat (lambda (box) (plist-get box :text))
                            (sort line (lambda (one other)
                                         (< (plist-get one :x)
                                            (plist-get other :x))))
                            separator))
               (clipimg-ocr--lines ordered)
               "\n")))


;;;; Running a program

(defun clipimg-ocr--output (program &rest arguments)
  "Return what PROGRAM run with ARGUMENTS writes to standard output.
Signal a `user-error' when it fails, and return nil when it is absent.
Standard error goes to a file of its own, so that the chatter tesseract
writes there never lands in the text."
  (when-let* ((executable (executable-find program)))
    (let ((errors (make-temp-file "clipimg-stderr-")))
      (unwind-protect
          (with-temp-buffer
            (let ((status (apply #'call-process executable nil (list t errors)
                                 nil arguments)))
              (unless (eq status 0)
                (user-error "%s failed: %s" program
                            (string-trim (clipimg-ocr--file-string errors))))
              (buffer-string)))
        (delete-file errors)))))

(defun clipimg-ocr--file-string (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))


;;;; Showing the text

(defvar-local clipimg-ocr--clip nil
  "Clip the text in this buffer was read from.")

(defvar-local clipimg-ocr--options nil
  "Backend, language and layout the text in this buffer was read with.")

(defvar-keymap clipimg-ocr-mode-map
  :doc "Keys of the buffer holding recognized text."
  "w" #'clipimg-ocr-copy)

(define-derived-mode clipimg-ocr-mode special-mode "clipimg-OCR"
  "Major mode for text read out of a clipboard image.
\\<clipimg-ocr-mode-map>\\[revert-buffer] reads the image again,
\\[clipimg-ocr-copy] copies the text without touching the clipboard."
  (setq-local revert-buffer-function #'clipimg-ocr--revert))

(defun clipimg-ocr--revert (&rest _)
  "Read the clip of this buffer again with the same options."
  (apply #'clipimg-ocr-to-buffer clipimg-ocr--clip clipimg-ocr--options))

(defun clipimg-ocr-text ()
  "Return the recognized text of the current buffer, without the preview."
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (buffer-substring-no-properties (point) (point-max))))

(defun clipimg-ocr-copy ()
  "Copy the recognized text to the kill ring, leaving the clipboard alone."
  (interactive nil clipimg-ocr-mode)
  (clipimg-ocr--kill (clipimg-ocr-text)))

(defun clipimg-ocr--kill (text)
  "Put TEXT on the kill ring without replacing the image on the clipboard."
  (let ((select-enable-clipboard nil))
    (kill-new text))
  (message "clipimg: %d characters on the kill ring" (length text)))

(defun clipimg-ocr--render (clip text options)
  "Fill the OCR buffer with the preview of CLIP and its TEXT read with OPTIONS."
  (with-current-buffer (get-buffer-create clipimg-ocr-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (clipimg-clip-preview clip) "\n\n"
              (if (string-empty-p (string-trim text))
                  (propertize "no text found" 'face 'shadow)
                text)))
    (clipimg-ocr-mode)
    (setq clipimg-ocr--clip clip
          clipimg-ocr--options options)
    (goto-char (point-min))
    (current-buffer)))


;;;; Commands

(defun clipimg-ocr-to-buffer (clip &optional backend language layout)
  "Show the text BACKEND recognizes in CLIP in a buffer.
LANGUAGE and LAYOUT are passed to `clipimg-ocr-recognize'."
  (let ((text (clipimg-ocr-recognize clip backend language layout)))
    (display-buffer (clipimg-ocr--render clip text (list backend language layout)))))

(defun clipimg-ocr-to-kill-ring (clip &optional backend language layout)
  "Put the text BACKEND recognizes in CLIP on the kill ring.
The image stays on the system clipboard, so the same clip can be read
again.  LANGUAGE and LAYOUT are passed to `clipimg-ocr-recognize'."
  (clipimg-ocr--kill (clipimg-ocr-recognize clip backend language layout)))

(defun clipimg-ocr-insert (clip &optional backend language layout)
  "Insert the text BACKEND recognizes in CLIP at point.
LANGUAGE and LAYOUT are passed to `clipimg-ocr-recognize'."
  (when buffer-read-only
    (user-error "Buffer %s is read only" (buffer-name)))
  (insert (clipimg-ocr-recognize clip backend language layout)))

;;;###autoload
(defun clipimg-ocr ()
  "Show the text of the image on the clipboard in a buffer."
  (interactive)
  (clipimg-ocr-to-buffer (clipimg-clipboard-clip-or-error)))

(provide 'clipimg-ocr)
;;; clipimg-ocr.el ends here
