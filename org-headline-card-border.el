;;; org-headline-card-border.el --- Character art border generation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Yibie <yibie@outlook.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: org-mode, preview, image, unicode
;; URL: https://github.com/yibie/org-headline-card

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides character art border generation for org-headline-card.
;; It uses Unicode box-drawing characters to create elegant card decorations.

;; Features:
;; - Multiple border styles (single, double, mixed)
;; - Corner style options (square, round)
;; - Shadow effects
;; - Configurable padding and dimensions

;;; Code:

(require 'cl-lib)

;; Border character sets
(defconst org-headline-card--border-chars
  '((single
     . ((horizontal . ?─)
        (vertical . ?│)
        (top-left . ?┌)
        (top-right . ?┐)
        (bottom-left . ?└)
        (bottom-right . ?┘)
        (left-tee . ?├)
        (right-tee . ?┤)
        (top-tee . ?┬)
        (bottom-tee . ?┴)
        (cross . ?┼)))

    (double
     . ((horizontal . ?═)
        (vertical . ?║)
        (top-left . ?╔)
        (top-right . ?╗)
        (bottom-left . ?╚)
        (bottom-right . ?╝)
        (left-tee . ?╠)
        (right-tee . ?╣)
        (top-tee . ?╦)
        (bottom-tee . ?╩)
        (cross . ?╬)))

    (mixed
     . ((horizontal . ?─)
        (vertical . ?║)
        (top-left . ?╔)
        (top-right . ?╗)
        (bottom-left . ?╚)
        (bottom-right . ?╝)
        (left-tee . ?╟)
        (right-tee . ?╢)
        (top-tee . ?╤)
        (bottom-tee . ?╧)
        (cross . ?┼)))

    (rounded
     . ((horizontal . ?─)
        (vertical . ?│)
        (top-left . ?╭)
        (top-right . ?╮)
        (bottom-left . ?╰)
        (bottom-right . ?╯)
        (left-tee . ?├)
        (right-tee . ?┤)
        (top-tee . ?┬)
        (bottom-tee . ?┴)
        (cross . ?┼))))
  "Character sets for different border styles.")

;; Border theme configuration
(defcustom org-headline-card-border-themes
  '((elegant
     . ((border-style . double)
        (corner-style . square)
        (shadow . t)
        (padding-horizontal . 2)
        (padding-vertical . 1)
        (decoration . simple)))

    (classic
     . ((border-style . single)
        (corner-style . square)
        (shadow . nil)
        (padding-horizontal . 1)
        (padding-vertical . 1)
        (decoration . none)))

    (minimal
     . ((border-style . single)
        (corner-style . round)
        (shadow . nil)
        (padding-horizontal . 1)
        (padding-vertical . 1)
        (decoration . none))))
  "Border decoration themes for cards.
Each theme specifies:
- border-style: single, double, mixed, or rounded
- corner-style: square or round
- shadow: whether to add drop shadow
- padding-horizontal: horizontal padding inside border
- padding-vertical: vertical padding inside border
- decoration: simple, none, or ornate"
  :type '(alist :key-type symbol :value-type (alist :key-type symbol :value-type sexp))
  :group 'org-headline-card)

(defcustom org-headline-card-border-theme 'minimal
  "Current border theme for card generation.
Options:
- 'elegant: Double-line borders with shadow (formal)
- 'classic: Single-line square borders (traditional)
- 'minimal: Single-line rounded corners (modern, clean)"
  :type '(choice (const elegant)
                 (const classic)
                 (const minimal))
  :group 'org-headline-card)

(defun org-headline-card--get-border-chars (style)
  "Get border character set for STYLE.
STYLE should be a symbol: single, double, mixed, or rounded."
  (alist-get style org-headline-card--border-chars))

(defun org-headline-card--generate-border-line (width chars side)
  "Generate a border line of WIDTH characters using CHARS.
SIDE can be: top, bottom, or middle."
  (pcase side
    ('top
     (concat (string (alist-get 'top-left chars))
             (make-string (- width 2) (alist-get 'horizontal chars))
             (string (alist-get 'top-right chars))))
    ('bottom
     (concat (string (alist-get 'bottom-left chars))
             (make-string (- width 2) (alist-get 'horizontal chars))
             (string (alist-get 'bottom-right chars))))
    ('separator
     (concat (string (alist-get 'left-tee chars))
             (make-string (- width 2) (alist-get 'horizontal chars))
             (string (alist-get 'right-tee chars))))
    (_
     (concat (string (alist-get 'vertical chars))
             (make-string (- width 2) ?\s)
             (string (alist-get 'vertical chars))))))

(defun org-headline-card--generate-border (width height &optional theme)
  "Generate border as list of strings.
WIDTH is the total width in characters.
HEIGHT is the number of content lines.
THEME is the border theme to use (defaults to `org-headline-card-border-theme`)."
  (let* ((theme-spec (alist-get (or theme org-headline-card-border-theme)
                                org-headline-card-border-themes))
         (border-style (alist-get 'border-style theme-spec))
         (chars (org-headline-card--get-border-chars border-style))
         (shadow (alist-get 'shadow theme-spec))
         (result '())
         (total-height (+ height 2))) ;; +2 for top and bottom borders

    ;; Top border
    (push (org-headline-card--generate-border-line width chars 'top) result)

    ;; Middle lines (empty borders)
    (dotimes (_ height)
      (push (org-headline-card--generate-border-line width chars 'middle) result))

    ;; Bottom border
    (push (org-headline-card--generate-border-line width chars 'bottom) result)

    ;; Shadow (if enabled)
    (when shadow
      (let ((shadow-line (concat " " (make-string (- width 2) ?\s) "░")))
        (setq result (cons shadow-line result))))

    (nreverse result)))

(defun org-headline-card-set-border-theme (theme)
  "Set the current border theme to THEME.
THEME must be a key present in `org-headline-card-border-themes'."
  (interactive
   (list (intern
          (completing-read "Select border theme: "
                          (mapcar #'symbol-name
                                 (mapcar #'car org-headline-card-border-themes))
                          nil t))))
  (if (alist-get theme org-headline-card-border-themes)
      (progn
        (setq org-headline-card-border-theme theme)
        (message "Border theme set to: %s" theme))
    (error "Border theme '%s' not found" theme)))

(provide 'org-headline-card-border)
;;; org-headline-card-border.el ends here
