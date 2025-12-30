;;; org-headline-card-preview.el --- Interactive preview system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Yibie <yibie@outlook.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: org-mode, preview, interactive
;; URL: https://github.com/yibie/org-headline-card

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides interactive preview functionality for org-headline-card.
;; Users can preview cards with full styling, edit content, and switch themes
;; in real-time before exporting to images.

;; Features:
;; - Live preview with text-properties
;; - Theme switching with instant refresh
;; - Interactive editing of card content
;; - Border and color theme integration

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-headline-card-effects)
(require 'org-headline-card-layout)
(require 'org-headline-card-border)

;; Forward declarations for effect system (loaded separately)
(declare-function org-headline-card-apply-effect "org-headline-card-effects")
(declare-function org-headline-card-remove-effect-at-point "org-headline-card-effects")
(declare-function org-headline-card-clear-all-effects "org-headline-card-effects")

;; Preview configuration
(defcustom org-headline-card-enable-preview t
  "Whether to show preview buffer when rendering cards."
  :type 'boolean
  :group 'org-headline-card)

(defcustom org-headline-card-preview-buffer-name "*Org Headline Card Preview*"
  "Name of the preview buffer."
  :type 'string
  :group 'org-headline-card)

;; Preview mode
(define-derived-mode org-headline-card-preview-mode special-mode "Card Preview"
  "Major mode for org-headline-card preview buffer.

\\{org-headline-card-preview-mode-map}"
  (setq buffer-read-only t)
  (setq show-trailing-whitespace nil))

;; Keybindings
(define-key org-headline-card-preview-mode-map (kbd "q") 'quit-window)
(define-key org-headline-card-preview-mode-map (kbd "r") 'org-headline-card-preview-refresh)
(define-key org-headline-card-preview-mode-map (kbd "t") 'org-headline-card-set-theme)
(define-key org-headline-card-preview-mode-map (kbd "b") 'org-headline-card-set-border-theme)
(define-key org-headline-card-preview-mode-map (kbd "e") 'org-headline-card-preview-edit-content)
(define-key org-headline-card-preview-mode-map (kbd "RET") 'org-headline-card-preview-export)
;; Font size adjustment
(define-key org-headline-card-preview-mode-map (kbd "+") 'org-headline-card-preview-increase-font)
(define-key org-headline-card-preview-mode-map (kbd "-") 'org-headline-card-preview-decrease-font)
(define-key org-headline-card-preview-mode-map (kbd "=") 'org-headline-card-preview-increase-font)
;; Effect keybindings
(define-key org-headline-card-preview-mode-map (kbd "h") 'org-headline-card-preview-apply-highlight)
(define-key org-headline-card-preview-mode-map (kbd "g") 'org-headline-card-preview-apply-glow)
(define-key org-headline-card-preview-mode-map (kbd "s") 'org-headline-card-preview-apply-stroke)
(define-key org-headline-card-preview-mode-map (kbd "w") 'org-headline-card-preview-apply-wave)
(define-key org-headline-card-preview-mode-map (kbd "0") 'org-headline-card-remove-effect-at-point)
(define-key org-headline-card-preview-mode-map (kbd "C-c C-c") 'org-headline-card-clear-all-effects)

;; Mode line indicator
(or (assq 'org-headline-card-preview-mode minor-mode-alist)
    (push '(org-headline-card-preview-mode " CardPreview") minor-mode-alist))

;; Preview state
(defvar org-headline-card--preview-current-headline nil
  "The headline element currently being previewed.")

(defvar org-headline-card--preview-current-source-buffer nil
  "The buffer where the previewed headline comes from.")

(defvar org-headline-card--export-effects nil
  "Effects to be applied during export.
Set by preview-export, consumed by export function.")

;; Main preview function
(defun org-headline-card--show-preview (headline)
  "Display HEADLINE in preview buffer.
Shows the rendered text card with full styling (bold, italic, underline)."
  (interactive)
  (let* ((buffer (get-buffer-create org-headline-card-preview-buffer-name))
         (theme-style (org-headline-card--get-theme-style))
         (canonical-lines (org-headline-card-layout-build-lines headline theme-style 'preview))
         (rendered-text (org-headline-card--preview-render-with-border canonical-lines))
         (fg-color (alist-get 'rectangleFontColor theme-style))
         (bg-color (alist-get 'rectangleBackgroundColor theme-style))
         (font-family (alist-get 'defaultFontName theme-style)))

    ;; Save state
    (setq org-headline-card--preview-current-headline headline)
    (setq org-headline-card--preview-current-source-buffer (current-buffer))

    ;; Switch to preview buffer
    (switch-to-buffer buffer)
    (org-headline-card-preview-mode)
    (let* ((theme-spec (alist-get org-headline-card-border-theme org-headline-card-border-themes))
           (border-style (alist-get 'border-style theme-spec))
           (chars (org-headline-card--get-border-chars border-style))
           (left-pad (or (alist-get 'padding-horizontal theme-spec) 2)))
      (setq-local org-headline-card--preview-border-vertical (alist-get 'vertical chars))
      (setq-local org-headline-card--preview-left-padding-cols (1+ left-pad)))
    (read-only-mode -1)
    (erase-buffer)

    ;; Apply buffer-local styling
    (setq-local line-spacing 0)
    ;; Only apply colors, relying on default monospace font for alignment
    (face-remap-add-relative 'default 
                            :foreground fg-color 
                            :background bg-color)

    ;; Insert Rendered Text (with properties)
    (insert rendered-text)
    
    ;; Make sure we are at the top
    (goto-char (point-min))

    ;; Make buffer read-only
    (read-only-mode 1)

    ;; Display help text
    (message "[Card Preview] q:quit r:refresh t:color b:border +/-:font h:highlight g:glow s:box w:wave 0:clear RET:export")))

(defun org-headline-card--preview-render-with-border (canonical-lines)
  "Render CANONICAL-LINES (borderless) into a bordered preview string."
  (let* ((content-width org-headline-card-export-width)
         (theme-spec (alist-get org-headline-card-border-theme org-headline-card-border-themes))
         (left-pad (or (alist-get 'padding-horizontal theme-spec) 2))
         (right-pad left-pad)
         (inner-width (+ content-width left-pad right-pad))
         (border-width (+ inner-width 2))
         (border-style (alist-get 'border-style theme-spec))
         (chars (org-headline-card--get-border-chars border-style))
         (v (alist-get 'vertical chars))
         (vertical (string v))
         (top (org-headline-card--generate-border-line border-width chars 'top))
         (bottom (org-headline-card--generate-border-line border-width chars 'bottom))
         (sep (org-headline-card--generate-border-line border-width chars 'separator))
         (out '())
         (seen-separator nil))
    (push top out)
    (dolist (line canonical-lines)
      (if (get-text-property 0 'org-headline-card-separator line)
          (progn
            (push sep out)
            (setq seen-separator t))
        (let* ((line-width (string-width line))
               (content-pad (max 0 (- content-width line-width)))
               (line-str (concat vertical
                                 (make-string left-pad ?\s)
                                 line
                                 (make-string (+ content-pad right-pad) ?\s)
                                 vertical)))
          (push line-str out))))
    (unless seen-separator
      (push sep out))
    (push bottom out)
    (mapconcat #'identity (nreverse out) "\n")))

(defun org-headline-card--apply-preview-styling (fg-color bg-color)
  "Apply styling colors to the entire preview buffer."
  (let ((inhibit-read-only t))
    (add-face-text-property (point-min) (point-max)
                           `((:foreground ,fg-color)
                             (:background ,bg-color)))))

;;;###autoload
(defun org-headline-card-preview ()
  "Preview the card for headline at point.
Opens a preview buffer showing the card with all styling applied."
  (interactive)
  (let ((element (org-element-at-point)))
    (if (eq (org-element-type element) 'headline)
        (org-headline-card--show-preview element)
      (error "Point must be at an Org headline"))))

;;;###autoload
(defun org-headline-card-preview-refresh ()
  "Refresh the current preview buffer.
Updates the preview to reflect theme or content changes."
  (interactive)
  (if (and org-headline-card--preview-current-headline
           org-headline-card--preview-current-source-buffer
           (buffer-live-p org-headline-card--preview-current-source-buffer))
      (let ((source-buffer org-headline-card--preview-current-source-buffer)
            (marker-pos (org-element-property :begin org-headline-card--preview-current-headline)))
        (with-current-buffer source-buffer
          (save-excursion
            (goto-char marker-pos)
            (let ((new-headline (org-element-at-point)))
              (org-headline-card--show-preview new-headline))))
        (message "Preview refreshed"))
    (message "No active source buffer to refresh")))

(defun org-headline-card-preview-edit-content ()
  "Edit the content of the card being previewed.
Allows interactive editing of the headline title and content."
  (interactive)
  (if (not (and org-headline-card--preview-current-headline
               org-headline-card--preview-current-source-buffer))
      (message "No active preview to edit")
    (let ((headline org-headline-card--preview-current-headline)
          (source-buffer org-headline-card--preview-current-source-buffer))
      ;; Switch to source buffer
      (switch-to-buffer source-buffer)
      ;; Go to headline
      (goto-char (org-element-property :begin headline))
      (message "Edit the headline, then press 'r' in preview to refresh"))))

(defun org-headline-card-preview-export ()
  "Export the currently previewed card to an image file.
If effects have been applied in preview, they will be included in the export."
  (interactive)
  (if (not (and org-headline-card--preview-current-headline
               org-headline-card--preview-current-source-buffer))
      (message "No active preview to export")
    (let* ((headline org-headline-card--preview-current-headline)
           (source-buffer org-headline-card--preview-current-source-buffer)
           (applied-effects (when (fboundp 'org-headline-card-get-applied-effects)
                              (org-headline-card-get-applied-effects))))
      ;; Store effects for the export to pick up
      (setq org-headline-card--export-effects applied-effects)
      (message "DEBUG: Stored %d effects for export" (length applied-effects))
      ;; Go back to source and use standard export (which uses SVG borders)
      (switch-to-buffer source-buffer)
      (goto-char (org-element-property :begin headline))
      (org-headline-card-at-point)
      ;; Clear effects after export
      (setq org-headline-card--export-effects nil))))

;; Auto-refresh on theme change
(defun org-headline-card--preview-refresh-on-theme-change (&rest _)
  "Auto-refresh preview when theme changes."
  (when (and org-headline-card-enable-preview
             org-headline-card--preview-current-headline
             (get-buffer org-headline-card-preview-buffer-name))
    (org-headline-card-preview-refresh)))

;; Font size adjustment functions
(defun org-headline-card-preview-increase-font ()
  "Increase font size for preview and export."
  (interactive)
  (setq org-headline-card-export-font-size (min 72 (+ org-headline-card-export-font-size 2)))
  (message "Font size: %d" org-headline-card-export-font-size)
  (org-headline-card-preview-refresh))

(defun org-headline-card-preview-decrease-font ()
  "Decrease font size for preview and export."
  (interactive)
  (setq org-headline-card-export-font-size (max 8 (- org-headline-card-export-font-size 2)))
  (message "Font size: %d" org-headline-card-export-font-size)
  (org-headline-card-preview-refresh))

;; Add hooks
(add-hook 'org-headline-card-theme-change-hook
          #'org-headline-card--preview-refresh-on-theme-change)

;;; Effect application functions

(defun org-headline-card-preview-apply-highlight (arg)
  "Apply highlight effect to selected region."
  (interactive "P")
  (org-headline-card-preview--apply-effect 'highlight arg))

(defun org-headline-card-preview-apply-glow (arg)
  "Apply glow effect to selected region."
  (interactive "P")
  (org-headline-card-preview--apply-effect 'glow arg))

(defun org-headline-card-preview-apply-stroke (arg)
  "Apply stroke effect to selected region."
  (interactive "P")
  (org-headline-card-preview--apply-effect 'stroke arg))

(defun org-headline-card-preview-apply-wave (arg)
  "Apply wavy underline effect to selected region."
  (interactive "P")
  (org-headline-card-preview--apply-effect 'wave arg))

(defun org-headline-card-preview--apply-effect (effect-name &optional arg)
  "Apply EFFECT-NAME to the selected region or word at point."
  (let ((start (if (use-region-p) (region-beginning) (save-excursion (backward-word) (point))))
        (end (if (use-region-p) (region-end) (save-excursion (forward-word) (point))))
        (params nil))
    (when (use-region-p)
      (let ((start-line (line-number-at-pos start))
            (end-line (line-number-at-pos (max start (1- end)))))
        (unless (= start-line end-line)
          (user-error "Effect can only be applied within a single line"))))
    (when arg
      (setq params
            (pcase effect-name
              ('highlight
               (list :color (org-headline-card--normalize-color-rgba
                             (read-color "Highlight color: " t nil
                                         org-headline-card-effect-highlight-color))))
              ('glow
               (list :color (org-headline-card--normalize-color-rgba
                             (read-color "Glow color: " t nil
                                         org-headline-card-effect-glow-color))))
              ('stroke
               (list :color (org-headline-card--normalize-color-rgba
                             (read-color "Box border color: " t nil
                                         org-headline-card-effect-box-color))
                     :width (read-number "Box border width (px): "
                                         org-headline-card-effect-box-width)))
              ('wave
               (list :color (org-headline-card--normalize-color-rgba
                             (read-color "Wave color: " t nil
                                         org-headline-card-effect-wave-color))
                     :width (read-number "Wave width (px): "
                                         org-headline-card-effect-wave-width)))
              (_ nil))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (org-headline-card-apply-effect effect-name start end params))
      (deactivate-mark))))

(provide 'org-headline-card-preview)
;;; org-headline-card-preview.el ends here
