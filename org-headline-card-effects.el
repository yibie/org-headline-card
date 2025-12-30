;;; org-headline-card-effects.el --- Pluggable text effects system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; This file is part of org-headline-card.

;;; Commentary:

;; This module provides a pluggable effect system for org-headline-card.
;; Effects can be registered and applied to text regions in the preview buffer.
;; Each effect defines how it appears in preview (via text-properties) and
;; how it renders in SVG export.

;; Architecture:
;; - Effects are registered in `org-headline-card-effects-registry'
;; - Each effect provides :preview-fn and :svg-fn
;; - Applied effects are stored in buffer-local `org-headline-card--applied-effects'
;; - On export, effects are passed to the SVG generator

;;; Code:

(require 'cl-lib)
(require 'color)

;;; Effect Defaults

(defun org-headline-card--normalize-color-rgba (color)
  "Normalize COLOR to a CSS-style #RRGGBBAA string.
COLOR can be a color name, #RRGGBB, #RRRRGGGGBBBB, or include alpha."
  (when color
    (let* ((s (string-trim (format "%s" color)))
           (hex (and (string-prefix-p "#" s) (substring s 1)))
           (len (and hex (length hex))))
      (cond
       ;; #RGB
       ((and hex (= len 3))
        (format "#%c%c%c%c%c%cFF"
                (aref hex 0) (aref hex 0)
                (aref hex 1) (aref hex 1)
                (aref hex 2) (aref hex 2)))
       ;; #RRGGBB
       ((and hex (= len 6))
        (concat s "FF"))
       ;; #RRGGBBAA
       ((and hex (= len 8))
        s)
       ;; #RRRRGGGGBBBB (16-bit/channel)
       ((and hex (= len 12))
        (format "#%c%c%c%c%c%cFF"
                (aref hex 0) (aref hex 1)
                (aref hex 4) (aref hex 5)
                (aref hex 8) (aref hex 9)))
       ;; #RRRRGGGGBBBBAAAA (16-bit/channel + alpha)
       ((and hex (= len 16))
        (format "#%c%c%c%c%c%c%c%c"
                (aref hex 0) (aref hex 1)
                (aref hex 4) (aref hex 5)
                (aref hex 8) (aref hex 9)
                (aref hex 12) (aref hex 13)))
       ;; color name -> use Emacs color-values
       (t
        (let ((rgb (ignore-errors (color-values s))))
          (when rgb
            (pcase-let ((`(,r ,g ,b) rgb))
              (format "#%02X%02X%02XFF"
                      (lsh r -8) (lsh g -8) (lsh b -8))))))))))

(defun org-headline-card--rgba->rgb (rgba)
  "Return #RRGGBB from RGBA-like strings."
  (let ((n (org-headline-card--normalize-color-rgba rgba)))
    (when (and n (>= (length n) 7))
      (substring n 0 7))))

(defcustom org-headline-card-effect-highlight-color "#FFFF00"
  "Default background color for highlight effect."
  :type 'string
  :group 'org-headline-card)

(defcustom org-headline-card-effect-glow-color "#00FFFF"
  "Default color for glow effect."
  :type 'string
  :group 'org-headline-card)

(defcustom org-headline-card-effect-box-color "#000000"
  "Default border color for box effect."
  :type 'string
  :group 'org-headline-card)

(defcustom org-headline-card-effect-box-width 1
  "Default border width (in px) for box effect."
  :type 'integer
  :group 'org-headline-card)

(defcustom org-headline-card-effect-wave-color "#FF0000"
  "Default color for wavy underline effect."
  :type 'string
  :group 'org-headline-card)

(defcustom org-headline-card-effect-wave-width 2
  "Default stroke width (in px) for wavy underline effect."
  :type 'integer
  :group 'org-headline-card)

;;; Effect Registry

(defvar org-headline-card-effects-registry '()
  "Registry of available text effects.
Each effect is a plist with:
  :name       - Symbol identifying the effect
  :key        - Key binding in preview mode (string)
  :description - Human-readable description
  :preview-fn - Function (start end &optional params) to apply in preview
  :svg-fn     - Function (text x y width height &optional params) to generate SVG
  :svg-defs-fn - Optional function to generate SVG <defs> elements")

(defvar-local org-headline-card--applied-effects '()
  "List of effects applied in current buffer.
Each element is (START END EFFECT-NAME PARAMS).")

(defvar-local org-headline-card--preview-border-vertical nil
  "Preview border vertical character for stable effect anchoring.")

(defvar-local org-headline-card--preview-left-padding-cols 0
  "Preview left padding columns before content (including border).")

(defun org-headline-card--preview-line-content (pos)
  "Return (CONTENT START-COL END-COL) for line containing POS in preview.
CONTENT is the trimmed line text without borders/padding.
START-COL/END-COL are display columns relative to CONTENT start."
  (save-excursion
    (goto-char pos)
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (line (buffer-substring-no-properties line-start line-end))
           (line-len (length line))
           (abs-col (current-column))
           (left 0)
           (right line-len))
      ;; Strip border when the vertical char is known and present on both sides.
      (when (and org-headline-card--preview-border-vertical
                 (>= line-len 2)
                 (eq (aref line 0) org-headline-card--preview-border-vertical)
                 (eq (aref line (1- line-len)) org-headline-card--preview-border-vertical))
        (setq left org-headline-card--preview-left-padding-cols)
        (setq right (1- right)))
      ;; Strip trailing spaces from content (preview pads to width).
      (while (and (< left right)
                  (eq (aref line (1- right)) ?\s))
        (setq right (1- right)))
      (let ((content (if (<= right left) "" (substring line left right)))
            (rel-col (max 0 (- abs-col left))))
        (list content rel-col)))))

;;; Effect Registration API

(defun org-headline-card-register-effect (&rest props)
  "Register a new text effect.
PROPS is a plist with :name, :key, :description, :preview-fn, :svg-fn.
Optional: :svg-defs-fn for effects that need <defs> elements."
  (let ((name (plist-get props :name)))
    (unless name
      (error "Effect must have a :name"))
    ;; Remove existing effect with same name
    (setq org-headline-card-effects-registry
          (cl-remove-if (lambda (e) (eq (plist-get e :name) name))
                        org-headline-card-effects-registry))
    ;; Add new effect
    (push props org-headline-card-effects-registry)))

(defun org-headline-card-get-effect (name)
  "Get effect by NAME from registry."
  (cl-find-if (lambda (e) (eq (plist-get e :name) name))
              org-headline-card-effects-registry))

(defun org-headline-card-list-effects ()
  "List all registered effects."
  (mapcar (lambda (e) (plist-get e :name))
          org-headline-card-effects-registry))

;;; Effect Application

(defun org-headline-card-apply-effect (effect-name start end &optional params)
  "Apply EFFECT-NAME to region from START to END.
PARAMS is an optional plist of effect-specific parameters."
  (let ((effect (org-headline-card-get-effect effect-name)))
    (unless effect
      (error "Unknown effect: %s" effect-name))
    ;; Apply preview visualization
    (let ((preview-fn (plist-get effect :preview-fn)))
      (when preview-fn
        (funcall preview-fn start end params)))
    ;; Record the effect with the selected text content and a stable line anchor.
    ;; Anchor uses the preview line's trimmed content + display columns within that content.
    (let* ((selected-text (buffer-substring-no-properties start end))
           (start-line (line-number-at-pos start))
           (end-line (line-number-at-pos (max start (1- end))))
           (start-line-info (and (= start-line end-line)
                                 (org-headline-card--preview-line-content start)))
           (end-line-info (and (= start-line end-line)
                               (org-headline-card--preview-line-content end)))
           (line-text (car-safe start-line-info))
           (col-start (cadr start-line-info))
           (col-end (cadr end-line-info)))
      (message "DEBUG: Recording effect on text: '%s'" selected-text)
      (push (list start end effect-name params selected-text
                  line-text col-start col-end)
            org-headline-card--applied-effects))
    (message "Applied effect: %s" (plist-get effect :description))))

(defun org-headline-card-remove-effect-at-point ()
  "Remove effect at point."
  (interactive)
  (let ((pos (point)))
    (setq org-headline-card--applied-effects
          (cl-remove-if (lambda (e)
                          (and (>= pos (nth 0 e))
                               (<= pos (nth 1 e))))
                        org-headline-card--applied-effects))
    ;; TODO: Also remove text-properties
    (message "Removed effects at point")))

(defun org-headline-card-clear-all-effects ()
  "Clear all applied effects."
  (interactive)
  (setq org-headline-card--applied-effects '())
  (message "Cleared all effects"))

(defun org-headline-card-get-applied-effects ()
  "Get list of currently applied effects."
  org-headline-card--applied-effects)

;;; Built-in Effects

;; Highlight Effect
(defun org-headline-card-effect--highlight-preview (start end &optional params)
  "Apply highlight effect in preview buffer from START to END.
PARAMS can contain :color (default yellow)."
  (let* ((color (or (plist-get params :color) org-headline-card-effect-highlight-color))
         (rgb (or (org-headline-card--rgba->rgb color) org-headline-card-effect-highlight-color)))
    (add-face-text-property start end `(:background ,rgb) nil)))

(defun org-headline-card-effect--highlight-svg (_text x y width height &optional params)
  "Generate SVG for highlight effect.
Returns a rect element to be placed before the text."
  (let ((color (or (plist-get params :color) "#FFFF00"))
        (opacity (or (plist-get params :opacity) "0.4")))
    (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\" opacity=\"%s\"/>"
            x (- y height) width height color opacity)))

;; Register highlight effect
(org-headline-card-register-effect
 :name 'highlight
 :key "h"
 :description "背景高亮 (Background highlight)"
 :preview-fn #'org-headline-card-effect--highlight-preview
 :svg-fn #'org-headline-card-effect--highlight-svg)

;; Glow Effect (for future)
(defun org-headline-card-effect--glow-preview (start end &optional params)
  "Apply glow effect in preview buffer from START to END."
  (let* ((color (or (plist-get params :color) org-headline-card-effect-glow-color))
         (rgb (or (org-headline-card--rgba->rgb color) org-headline-card-effect-glow-color)))
    ;; In terminal, just use a distinct background
    (add-face-text-property start end `(:background ,rgb :foreground "black") nil)))

(defun org-headline-card-effect--glow-svg-defs (&optional params)
  "Generate SVG defs for glow filter."
  (let ((_color (or (plist-get params :color) "#00FFFF"))  ; reserved for future use
        (blur (or (plist-get params :blur) "3")))
    (format "    <filter id=\"glowEffect\" x=\"-50%%\" y=\"-50%%\" width=\"200%%\" height=\"200%%\">
      <feGaussianBlur in=\"SourceGraphic\" stdDeviation=\"%s\" result=\"blur\"/>
      <feMerge>
        <feMergeNode in=\"blur\"/>
        <feMergeNode in=\"SourceGraphic\"/>
      </feMerge>
    </filter>\n" blur)))

(defun org-headline-card-effect--glow-svg (_text _x _y _width _height &optional _params)
  "Generate SVG for glow effect - returns filter attribute."
  ;; Glow is applied via filter on text, not a separate element
  "filter=\"url(#glowEffect)\"")

;; Register glow effect
(org-headline-card-register-effect
 :name 'glow
 :key "g"
 :description "文字光晕 (Text glow)"
 :preview-fn #'org-headline-card-effect--glow-preview
 :svg-fn #'org-headline-card-effect--glow-svg
 :svg-defs-fn #'org-headline-card-effect--glow-svg-defs)

;; Stroke/Outline Effect
(defun org-headline-card-effect--stroke-preview (start end &optional params)
  "Apply box outline effect in preview buffer from START to END.
PARAMS can contain :color and :width."
  (let* ((color (or (plist-get params :color) org-headline-card-effect-box-color))
         (rgb (or (org-headline-card--rgba->rgb color) org-headline-card-effect-box-color))
        (width (or (plist-get params :width) org-headline-card-effect-box-width)))
    (add-face-text-property start end `(:box (:line-width ,width :color ,rgb)) nil)))

(defun org-headline-card-effect--stroke-svg (_text _x _y _width _height &optional params)
  "Generate SVG stroke attributes for text."
  (let ((color (or (plist-get params :color) "#000000"))
        (stroke-width (or (plist-get params :width) "1")))
    (format "stroke=\"%s\" stroke-width=\"%s\"" color stroke-width)))

;; Register stroke effect
(org-headline-card-register-effect
 :name 'stroke
 :key "s"
 :description "文本描边框 (Text box outline)"
 :preview-fn #'org-headline-card-effect--stroke-preview
 :svg-fn #'org-headline-card-effect--stroke-svg)

;; Wavy underline effect
(defun org-headline-card-effect--wave-preview (start end &optional params)
  "Apply wavy underline effect in preview buffer from START to END.
PARAMS can contain :color and :width."
  (let* ((color (or (plist-get params :color) org-headline-card-effect-wave-color))
         (rgb (or (org-headline-card--rgba->rgb color) org-headline-card-effect-wave-color))
         (width (or (plist-get params :width) org-headline-card-effect-wave-width)))
    (add-face-text-property start end
                            `(:underline (:style wave :color ,rgb)
                              :underline-width ,width)
                            nil)))

(org-headline-card-register-effect
 :name 'wave
 :key "w"
 :description "波浪线 (Wavy underline)"
 :preview-fn #'org-headline-card-effect--wave-preview
 :svg-fn nil)

(provide 'org-headline-card-effects)
;;; org-headline-card-effects.el ends here
