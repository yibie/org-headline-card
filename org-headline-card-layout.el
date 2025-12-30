;;; org-headline-card-layout.el --- Canonical layout generation -*- lexical-binding: t; -*-

;;; Commentary:
;; This module builds a canonical, borderless text layout that is used as the
;; single source of truth for both preview and export.  Preview may decorate
;; the layout (e.g., ASCII border), but the interior text comes from here.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'subr-x)

(declare-function org-headline-card--get-contents "org-headline-card")
(declare-function org-headline-card--apply-text-properties "org-headline-card")
(declare-function org-headline-card--protect-urls "org-headline-card")

(defun org-headline-card-layout--char-index-at-cols (string start max-cols)
  "Return the char index in STRING from START that fits within MAX-COLS columns."
  (let ((pos start)
        (acc 0)
        (len (length string)))
    (while (and (< pos len) (< acc max-cols))
      (let ((w (char-width (aref string pos))))
        (if (> (+ acc w) max-cols)
            ;; Ensure progress even when a single character exceeds MAX-COLS
            ;; (e.g. CJK fullwidth char when MAX-COLS is 1).
            (progn
              (setq pos (1+ pos))
              (setq acc max-cols))
          (setq acc (+ acc w))
          (setq pos (1+ pos)))))
    pos))

(defun org-headline-card-layout--find-break (string start end)
  "Find a break position in STRING between START and END (exclusive).
Returns a cons (BREAK . NEXT) where BREAK is the end index for the chunk,
and NEXT is the next start index (skipping whitespace)."
  (let ((break end)
        (next end))
    (when (> end start)
      (let ((i (1- end)))
        (while (and (>= i start)
                    (not (memq (aref string i) '(?\s ?\t))))
          (setq i (1- i)))
        (when (and (>= i start) (< i (1- end)))
          (setq break i)
          (setq next (1+ i))
          (while (and (< next (length string))
                      (memq (aref string next) '(?\s ?\t)))
            (setq next (1+ next))))))
    (cons break next)))

(defun org-headline-card-layout--wrap-with-prefix (prefix body width)
  "Wrap BODY (string with text-properties) to WIDTH columns, adding PREFIX.
PREFIX is a cons (FIRST . NEXT), each a string (may include properties)."
  (let* ((first-prefix (or (car prefix) ""))
         (next-prefix (or (cdr prefix) ""))
         (first-prefix-cols (string-width first-prefix))
         (next-prefix-cols (string-width next-prefix))
         (pos 0)
         (len (length body))
         (lines '())
         (is-first t))
    (while (or (< pos len) (and is-first (= len 0)))
      (let* ((pfx (if is-first first-prefix next-prefix))
             (avail (max 1 (- width (if is-first first-prefix-cols next-prefix-cols))))
             (hard-end (min len (org-headline-card-layout--char-index-at-cols body pos avail)))
             (break (if (= hard-end len)
                        (cons hard-end hard-end)
                      (org-headline-card-layout--find-break body pos hard-end)))
             (chunk-end (car break))
             (next-pos (cdr break))
             (chunk (if (<= chunk-end pos) (substring body pos hard-end) (substring body pos chunk-end))))
        (push (concat pfx chunk) lines)
        (setq is-first nil)
        (setq pos (if (and (= chunk-end pos) (< hard-end len)) hard-end next-pos))))
    (nreverse lines)))

(defun org-headline-card-layout--wrap-line (line width)
  "Wrap a single LINE (with text-properties) to WIDTH columns.
Preserves list markers by indenting continuation lines."
  (let* ((s line)
         (marker
          (cond
           ((string-match "\\`\\([-+*]\\s-+\\)" s) (match-string 1 s))
           ((string-match "\\`\\([0-9]+[.)]\\s-+\\)" s) (match-string 1 s))
           (t nil))))
    (if (not marker)
        (org-headline-card-layout--wrap-with-prefix (cons "" "") s width)
      (let* ((marker-end (match-end 1))
             (marker-cols (string-width marker))
             (raw-body (substring s marker-end))
             (body (if (string-match "\\`[ \t]+" raw-body)
                       (substring raw-body (match-end 0))
                     raw-body))
             (next-prefix (make-string marker-cols ?\s)))
        (org-headline-card-layout--wrap-with-prefix
         (cons (substring s 0 marker-end) next-prefix)
         body
         width)))))

(defun org-headline-card-layout--split-content (content width)
  "Split CONTENT into canonical lines wrapped to WIDTH columns.
Returns a list of strings with text-properties applied."
  (let* ((raw-lines (split-string (or content "") "\n" nil))
         (out '()))
    (dolist (raw raw-lines)
      (let* ((protected (ignore-errors (org-headline-card--protect-urls raw)))
             (processed-text (or (car-safe protected) raw))
             (normalized
              (thread-last processed-text
                (replace-regexp-in-string "^\\s-+" "")
                (replace-regexp-in-string "^[-*+]\\s-*" "- ")
                (replace-regexp-in-string "^\\([0-9]+[.)]\\)\\s-*" "\\1 ")))
             (styled (org-headline-card--apply-text-properties normalized)))
        (if (string-empty-p (string-trim styled))
            (push "" out)
          (setq out (nconc (nreverse (org-headline-card-layout--wrap-line styled width)) out)))))
    (nreverse out)))

(defun org-headline-card-layout-build-lines (headline theme-style &optional context)
  "Return canonical borderless lines for HEADLINE using THEME-STYLE.
Each line is a string with text-properties.
The returned list includes a separator line with `org-headline-card-separator'."
  (let* ((title (org-element-property :raw-value headline))
         (contents (org-headline-card--get-contents headline))
         (wrap-width org-headline-card-export-width)
         (base-title-size (string-to-number (alist-get 'titleFontSize theme-style)))
         (scale (if (eq context 'export) org-headline-card-export-title-scale 1.0))
         (title-size (max 1 (round (* base-title-size scale))))
         (title-raw (copy-sequence (or title "")))
         (_b (add-face-text-property 0 (length title-raw) 'bold nil title-raw))
         (_h (put-text-property 0 (length title-raw) 'height title-size title-raw))
         (title-processed (org-headline-card--apply-text-properties title-raw))
         (title-lines (org-headline-card-layout--wrap-line title-processed wrap-width))
         (content-lines (when contents (org-headline-card-layout--split-content contents wrap-width)))
         (sep (make-string (max 1 wrap-width) ?\s)))
    (put-text-property 0 (length sep) 'org-headline-card-separator t sep)
    (append title-lines (list sep) content-lines)))

(defun org-headline-card-layout-build-text (headline theme-style &optional context)
  "Return canonical borderless text for HEADLINE using THEME-STYLE."
  (mapconcat #'identity (org-headline-card-layout-build-lines headline theme-style context) "\n"))

(provide 'org-headline-card-layout)
;;; org-headline-card-layout.el ends here
