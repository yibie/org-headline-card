;;; org-headline-card.el --- Convert org headlines to preview cards -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Yibie <yibie@outlook.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, preview, image
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

;; This package provides functionality to convert Org-mode headlines and
;; their contents into preview card images using Emacs text-properties and
;; character art borders.  It's useful for creating visual representations
;; of Org headlines for presentations, documentation, or quick previews.

;; Features:
;; - Converts Org headlines to preview cards
;; - Handles Org markup (bold, italic, links, etc.)
;; - Customizable card style with themes
;; - Preserves list structure
;; - Real-time preview in Emacs
;; - Multiple border decoration styles

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-headline-card-border)
(require 'org-headline-card-export)
(require 'org-headline-card-effects)
(require 'org-headline-card-layout)
(require 'org-headline-card-preview)

(defgroup org-headline-card nil
  "Convert org headlines to preview cards."
  :group 'org)

(defcustom org-headline-card-directory
  (expand-file-name "headline-cards" user-emacs-directory)
  "Directory to store generated card images."
  :type 'directory
  :group 'org-headline-card)

(defcustom org-headline-card-max-width 80
  "Maximum width of the card content in characters.
If content is shorter, the card will be narrower."
  :type 'integer
  :group 'org-headline-card)

(defcustom org-headline-card-export-title-scale 1.0
  "Scale factor for title font size during export only.
Values below 1.0 reduce the exported title size without affecting preview."
  :type 'number
  :group 'org-headline-card)

(defcustom org-headline-card-base-theme
  '((dpi . "300")                        ;; Unified high resolution
    (defaultMonospacedFontSize . "26")    ;; Basic monospaced font size
    (padding . "40")                      ;; Unified padding
    (roundCorner . "40")                  ;; Unified round corner
    (shadowing . "false")                 ;; Default no shadow
    (handwritten . "false")               ;; Default non-handwritten style
    (lineHeight . "1.4")                  ;; Default line height
    (rectangleBorderThickness . "1")      ;; Unified border thickness
    (titleFontSize . "32")               ;; Title font size
    (contentFontSize . "26"))            ;; Content font size
  "Base theme settings that apply to all themes."
  :type '(alist :key-type symbol :value-type string)
  :group 'org-headline-card)

(defcustom org-headline-card-themes
  `((chinese-modern . ((defaultFontName . "LXGW WenKai Mono")  
                      (defaultFontSize . "16")                
                      (backgroundColor . "#FFFFFF")
                      (rectangleBorderColor . "#E8E8E8")
                      (rectangleFontColor . "#262626")        
                      (rectangleBackgroundColor . "#FFFFFF")))
    
    (chinese-ink . ((defaultFontName . "LXGW WenKai Mono")   
                    (defaultFontSize . "16")
                    (backgroundColor . "#FAFAF8")
                    (rectangleBorderColor . "#D4D4D4")
                    (rectangleFontColor . "#1A1A1A")
                    (rectangleBackgroundColor . "#FCFCFA")))
    
    (chinese-screen . ((defaultFontName . "LXGW WenKai Mono")
                      (defaultFontSize . "24")                
                      (backgroundColor . "#F8F4E9")
                      (rectangleBorderColor . "#E5DED0")
                      (rectangleFontColor . "#3F3F3F")
                      (rectangleBackgroundColor . "#FAF6ED")))

    (light . ((defaultFontName . "LXGW WenKai Mono")
              (defaultFontSize . "28")
              (backgroundColor . "#FFFFFF")
              (rectangleBorderColor . "#DDDDDD")
              (rectangleFontColor . "#2C3E50")
              (rectangleBackgroundColor . "#FFFFFF")))

    (dark . ((defaultFontName . "LXGW WenKai Mono")
             (defaultFontSize . "14")
             (backgroundColor . "#2B2B2B")
             (rectangleBorderColor . "#404040")
             (rectangleFontColor . "#E8E8E8")
             (rectangleBackgroundColor . "#333333")
             (shadow . t)
             (shadowColor . "#000000")
             (shadowOpacity . "0.4")
             (shadowBlur . "6")
             (shadowOffsetX . "4")
             (shadowOffsetY . "4")))       
    
    (warm . ((defaultFontName . "LXGW WenKai Mono")
             (defaultFontSize . "14")
             (backgroundColor . "#FAF7F2")
             (rectangleBorderColor . "#E8D5C4")
             (rectangleFontColor . "#5C4033")
             (rectangleBackgroundColor . "#FFF8F0")))

    (cyberpunk . ((defaultFontName . "LXGW WenKai Mono")
                  (defaultFontSize . "16")
                  (backgroundColor . "#000000")
                  (rectangleBorderColor . "#00FF00")
                  (rectangleFontColor . "#FFFFFF")
                  (rectangleBackgroundColor . "#000000")
                  (backgroundGradient . "<stop offset=\"0%\" stop-color=\"#ff00cc\"/>
                                       <stop offset=\"100%\" stop-color=\"#333399\"/>"))))
  "Theme-specific settings that override base theme settings."
  :type '(alist :key-type symbol :value-type (alist :key-type symbol :value-type string))
  :group 'org-headline-card)

(defcustom org-headline-card-current-theme 'light
  "Current theme for org-headline-card.
Must be a key present in `org-headline-card-themes'."
  :type 'symbol
  :group 'org-headline-card)

(defcustom org-headline-card-theme-change-hook nil
  "Hook run after changing the card theme."
  :type 'hook
  :group 'org-headline-card)

(defcustom org-headline-card-fallback-fonts
  '("Microsoft YaHei" "SimSun" "WenQuanYi Micro Hei" "Noto Sans CJK SC" "sans-serif")
  "Fallback font list for when preferred fonts are not available."
  :type '(repeat string)
  :group 'org-headline-card)

(defun org-headline-card--ensure-directory ()
  "Ensure the image directory exists."
  (unless (file-exists-p org-headline-card-directory)
    (make-directory org-headline-card-directory t))
  ;; Verify directory permissions
  (unless (file-writable-p org-headline-card-directory)
    (error "Cannot write to directory: %s" org-headline-card-directory))
  ;; (message "Using image directory: %s" org-headline-card-directory)  ; Debug
  )

(defun org-headline-card--calculate-width (content-lines)
  "Calculate optimal width for CONTENT-LINES."
  (if (null content-lines)
      30  ; If there are no content lines, return the default minimum width
    (let ((max-line-length
           (apply #'max
                  (mapcar (lambda (line)
                           (string-width line))
                         content-lines))))
      (min org-headline-card-max-width
           (+ 2 (max 30 max-line-length))))))

(defun org-headline-card--get-theme-style ()
  "Get the current theme's style settings with base theme applied."
  (let* ((base-theme org-headline-card-base-theme)
         (theme-specific (alist-get org-headline-card-current-theme org-headline-card-themes))
         (merged-theme base-theme))
    (dolist (setting theme-specific merged-theme)
      (setf (alist-get (car setting) merged-theme) (cdr setting)))))

;;;###autoload
(defun org-headline-card-check-dependencies ()
  "Check external dependencies required for exporting cards."
  (interactive)
  (let* ((emacs-ok (version<= "27.1" emacs-version))
         (org-ok (and (featurep 'org) (boundp 'org-version) (version<= "9.4" org-version)))
         (im (ignore-errors (org-headline-card--check-imagemagick))))
    (message "org-headline-card deps: Emacs=%s Org=%s ImageMagick=%s"
             (if emacs-ok "OK" "MISSING/OLD")
             (if org-ok "OK" "MISSING/OLD")
             (or im "NOT FOUND"))))

(defun org-headline-card--get-contents (headline)
  "Extract and process contents under HEADLINE.
Preserves list structure while removing metadata like PROPERTIES drawer and timestamps.
Returns a string with proper formatting for both lists and paragraphs."
  (let ((begin (org-element-property :contents-begin headline))
        (end (org-element-property :contents-end headline))
        contents)
    (when (and begin end)
      (setq contents (buffer-substring-no-properties begin end))
      ;; (message "Original contents:\n%s" contents)  ; Debug
      (with-temp-buffer
        (insert contents)
        (goto-char (point-min))
        ;; Remove PROPERTIES drawer
        (when (re-search-forward ":PROPERTIES:\n\\([^\000]*?\\):END:\n" nil t)
          (replace-match ""))
        ;; Remove all timestamps and their labels
        (goto-char (point-min))
        (while (re-search-forward
                (concat
                 "\\(?:"
                 ;; Match CLOSED, DEADLINE, SCHEDULED timestamps
                 "^\\s-*\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\): \\[.*?\\]\\s-*\n?"
                 "\\|"
                 ;; Match plain timestamps [YYYY-MM-DD Day HH:MM]
                 "\\[\\d\\{4\\}-\\d\\d-\\d\\d\\s-+[A-Za-z]\\{3\\}\\s-+\\d\\d:\\d\\d\\]"
                 "\\|"
                 ;; Match plain timestamps [YYYY-MM-DD Day]
                 "\\[\\d\\{4\\}-\\d\\d-\\d\\d\\s-+[A-Za-z]\\{3\\}\\]"
                 "\\|"
                 ;; Match plain timestamps [YYYY-MM-DD]
                 "\\[\\d\\{4\\}-\\d\\d-\\d\\d\\]"
                 "\\)")
                nil t)
          (replace-match ""))
        ;; Process the remaining content
        (let ((raw-content (buffer-string)))
          ;; (message "Cleaned contents:\n%s" raw-content)  ; Debug
          raw-content)))))

(defun org-headline-card--protect-urls (text)
  "Protect URLs in TEXT from being processed as markup.
Handles both direct URLs and org-mode link syntax.
Returns a cons cell (TEXT . LINKS) where LINKS is a list of collected links."
  (let ((links '())
        (link-counter 0)
        (result text))
    ;; Process direct URLs
    (setq result
          (replace-regexp-in-string
           "\\(https?://[^\s<>\"]+\\)"
           (lambda (match)
             (let* ((url (match-string 1 match))
                    (placeholder (format "@@URL_%d@@" link-counter)))
               (push (cons placeholder url) links)
               (cl-incf link-counter)
               url))  ; Return original URL, without placeholder
           result))
    
    ;; Process org-mode formatted links
    (setq result
          (replace-regexp-in-string
           "\\[\\[\\(https?://[^][\n]+?\\)\\]\\[\\([^][\n]+?\\)\\]\\]"
           (lambda (match)
             (let* ((url (match-string 1 match))
                    (desc (match-string 2 match))
                    (placeholder (format "@@LINK_%d@@" link-counter)))
               (push (cons placeholder (cons desc url)) links)
               (cl-incf link-counter)
               desc))  ; Return only description text
           result))
    
    ;; Return processed text and collected links
    (cons result (nreverse links))))

(defun org-headline-card--split-content (content)
  "Split CONTENT into properly formatted lines while preserving order.
Convert org-mode markup to styled text format while preserving list structure."
  ;; (message "Splitting content:\n%s" content)  ; Debug
  (let* ((lines (split-string content "\n" t))  ; Split by lines, not paragraphs
         (result '())
         (all-links '()))
    (dolist (line lines)
      (let* ((protected (org-headline-card--protect-urls line))
             (processed-text (car protected))
             (para-links (cdr protected))
             ;; Process all formatting markers first
             (marked-text
              (thread-last processed-text
                ;; Remove leading spaces from all lines to let rendering layer handle padding
                (replace-regexp-in-string "^\\s-+" "")
                ;; Process list markers - use hyphen to ensure alignment
                (replace-regexp-in-string
                 "^[-*+]\\s-*"
                 "- "))))
        
        ;; Process long line wrapping
        (with-temp-buffer
          (insert marked-text)
          (let ((fill-column org-headline-card-max-width))
            (fill-region (point-min) (point-max)))
          
          ;; Collect processed lines
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((raw-line (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
                   ;; Remove leading whitespace again (fill-region may add some)
                   (current-line (replace-regexp-in-string "^\\s-+" "" raw-line))
                   (is-continuation (and (> (line-number-at-pos) 1)
                                       (string-match-p "^\\s-*•" line)))
                   (formatted-line
                    (if is-continuation
                        (concat "  " current-line)  ; Continuation line indentation
                      current-line)))
              (push formatted-line result))
            (forward-line 1)))
        
        (setq all-links (append para-links all-links))))
    
    ;; Add links to result
    (when all-links
      (dolist (link (nreverse all-links))
        (when (consp (cdr link))  ; Only add links with description to the bottom
          (push (format "  <size:12><color:#666666>%s</color>" 
                       (cddr link))
                result))))
    
    (let ((final-result (nreverse result)))
      ;; (message "Split content:\n%s" (mapconcat #'identity final-result "\n"))  ; Debug
      final-result)))

(defun org-headline-card--wrap-line (line width)
  "Format LINE with proper indentation.
WIDTH is kept for compatibility but no longer used for wrapping."
  ;; (message "Formatting:\n%s" line)  ; Debug
  (let ((is-list-item (string-match-p "^\\s-*•" line)))
    (if is-list-item
        ;; List item: keep as is
        (list line)
      ;; Plain text: return as is
      (list line))))

(defun org-headline-card--process-title (title)
  "Process the title string for display."
  (let ((processed-title
         (thread-last title
           ;; Process org-mode links, only keep description text
           (replace-regexp-in-string
            "\\[\\[\\(?:[^][]\\|\\[\\(?:[^][]\\|\\[[^][]\\*\\]\\)*\\]\\)*\\]\\[\\(.*?\\)\\]\\]"
            "\\1")
           ;; Process separator
           (replace-regexp-in-string
            "|"
            "·"))))
    (org-headline-card--escape-string processed-title)))

(defun org-headline-card--format-line (line)
  "Format a single LINE for display."
  (thread-last line
    ;; Remove extra whitespace
    (replace-regexp-in-string "^\\s-+" "")
    (replace-regexp-in-string "\\s-+$" "")
    ;; Process special characters
    (org-headline-card--escape-string)))

(defun org-headline-card--escape-string (str)
  "Escape special characters in STR for safe display."
  (thread-last str
    (replace-regexp-in-string "\"" "\\\\\"")
    (replace-regexp-in-string "<" "\\\\<")
    (replace-regexp-in-string ">" "\\\\>")
    (replace-regexp-in-string "&" "&amp;")))

(defun org-headline-card--wrap-text (text width)
  "Wrap TEXT at WIDTH characters."
  (with-temp-buffer
    (insert text)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    ;; Convert newline characters to newline markers
    (replace-regexp-in-string
     "\n"
     "\\\\n"
     (buffer-string))))

(defun org-headline-card-set-theme (theme)
  "Set the current theme to THEME.
THEME must be a key present in `org-headline-card-themes'."
  (interactive
   (list (intern
          (completing-read "Select theme: "
                          (mapcar #'symbol-name
                                 (mapcar #'car org-headline-card-themes))
                          nil t))))
  (if (alist-get theme org-headline-card-themes)
      (progn
        (setq org-headline-card-current-theme theme)
        (run-hooks 'org-headline-card-theme-change-hook))
    (error "Theme '%s' not found in org-headline-card-themes" theme)))

(defun org-headline-card--available-cjk-fonts ()
  "Return a list of available CJK fonts on the system."
  (let ((font-list
         (cond
          ((eq system-type 'darwin)
           '("PingFang SC" "Hiragino Sans GB" "STHeiti" "Microsoft YaHei"))
          ((eq system-type 'gnu/linux)
           '("Noto Sans CJK SC" "WenQuanYi Micro Hei" "Microsoft YaHei" "SimSun"))
          (t
           '("Microsoft YaHei" "SimSun" "NSimSun" "SimHei"))))
        (available-fonts '()))
    (dolist (font font-list)
      (when (member font (font-family-list))
        (push font available-fonts)))
    (or available-fonts '("sans-serif"))))  ; If no CJK fonts are found, use sans-serif

(defun org-headline-card--get-font-string ()
  "Get font string with available CJK fonts."
  (string-join (org-headline-card--available-cjk-fonts) ", "))

;; Text-Properties Conversion
(defun org-headline-card--apply-text-properties (text)
  "Apply text-properties to TEXT based on Org markup.
Returns a string with Emacs text-properties applied for bold, italic, underline, and code.
Removes the markup delimiters from the output."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))

    ;; Process bold: *text*
    (while (re-search-forward "\\*\\([^*\n]+?\\)\\*" nil t)
      (let ((content (match-string 1)))
        (add-text-properties 0 (length content) '(face bold) content)
        (replace-match content)))

    ;; Process italic: /text/
    ;; Use stricter regex to avoid matching URLs
    (goto-char (point-min))
    (while (re-search-forward "\\(?:^\\|[^/:a-zA-Z0-9]\\)\\(/\\)\\([^/\n]+?\\)\\1\\(?:$\\|[^/:a-zA-Z0-9]\\)" nil t)
      (let* ((full-match (match-string 0))
             (match-beg (match-beginning 0))
             (content-beg (match-beginning 2))
             (content-end (match-end 2))
             (content (buffer-substring content-beg content-end))
             ;; Determine if there are prefix/suffix chars included in the match that shouldn't be replaced
             (prefix (substring full-match 0 (- content-beg match-beg 1))) ;; -1 for the opening /
             (suffix (substring full-match (+ (- content-end match-beg) 1)))) ;; +1 for the closing /
        
        (add-text-properties 0 (length content) '(face italic) content)
        (replace-match (concat prefix content suffix))))

    ;; Process underline: _text_
    (goto-char (point-min))
    (while (re-search-forward "_\\([^_\n]+?\\)_" nil t)
      (let ((content (match-string 1)))
        (add-text-properties 0 (length content) '(underline t) content)
        (replace-match content)))

    ;; Process code: ~text~ or =text=
    (goto-char (point-min))
    (while (re-search-forward "\\([~=]\\)\\([^~=\n]+?\\)\\1" nil t)
      (let ((content (match-string 2)))
        (add-text-properties 0 (length content) '(face font-lock-string-face) content)
        (replace-match content)))

    (buffer-string)))

(defun org-headline-card--apply-colors (text fg-color bg-color)
  "Apply foreground FG-COLOR and background BG-COLOR to TEXT.
Colors should be color name strings or hex values."
  (let ((fg-face (list :foreground fg-color))
        (bg-face (list :background bg-color)))
    ;; Use add-face-text-property to merge with existing faces (bold, italic, etc.)
    (add-face-text-property 0 (length text) fg-face t text)
    (when bg-color
      (add-face-text-property 0 (length text) bg-face t text))
    text))

(defun org-headline-card--apply-font-size (text size)
  "Apply font SIZE to TEXT.
SIZE should be a string like '12' or '14'."
  (let ((height (string-to-number size)))
    (put-text-property 0 (length text) 'height height)
    text))

(defun org-headline-card--column-to-char-index (line col)
  "Return char index in LINE that maps to display column COL."
  (let ((pos 0)
        (acc 0)
        (len (length line)))
    (while (and (< pos len) (< acc col))
      (setq acc (+ acc (char-width (aref line pos))))
      (setq pos (1+ pos)))
    pos))

(defun org-headline-card--split-lines-with-props (text)
  "Split TEXT into lines, preserving text properties."
  (let ((lines '()))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring (line-beginning-position)
                                      (line-end-position))))
          (push line lines))
        (forward-line 1)))
    (nreverse lines)))

(defun org-headline-card--wrap-string-with-props (text width)
  "Wrap TEXT to WIDTH columns, preserving text properties."
  (with-temp-buffer
    (insert text)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (org-headline-card--split-lines-with-props (buffer-string))))

;; Core Rendering Pipeline
(defun org-headline-card--render-to-text (headline &optional no-border)
  "Render HEADLINE org-element to decorated text with properties.
If NO-BORDER is non-nil, do not generate ASCII border characters.
Returns a string with text-properties, borders (optional), and styling applied."
  (let* ((title (org-element-property :raw-value headline))
         (contents (org-headline-card--get-contents headline))
         (wrap-width (when no-border org-headline-card-export-width))
         (content-lines
          (when contents
            (let ((org-headline-card-max-width (or wrap-width org-headline-card-max-width)))
              (org-headline-card--split-content contents))))
         (theme-style (org-headline-card--get-theme-style))
         (title-size (alist-get 'titleFontSize theme-style))
         (content-size (alist-get 'contentFontSize theme-style))
         (title-lines
          (let* ((styled-title (copy-sequence title))
                 (_dummy (add-face-text-property 0 (length styled-title) 'bold nil styled-title))
                 (_dummy2 (put-text-property 0 (length styled-title) 'height
                                             (string-to-number title-size) styled-title))
                 (processed-title (org-headline-card--apply-text-properties styled-title)))
            (if wrap-width
                (org-headline-card--wrap-string-with-props processed-title wrap-width)
              (list processed-title))))
         (max-line-length (if content-lines
                             (apply #'max (mapcar #'string-width content-lines))
                           0))
         (title-width (apply #'max (mapcar #'string-width title-lines)))
         (content-width (max max-line-length title-width))
         (padding-width 4) ;; 2 chars padding on each side
         (border-width (+ content-width padding-width 2)) ;; +2 for border chars
         (border-theme org-headline-card-border-theme)
         (border (unless no-border
                   (org-headline-card--generate-border border-width
                                                       (if content-lines
                                                           (length content-lines)
                                                         0)
                                                       border-theme))))

    ;; Build card text with borders and content
    (with-temp-buffer
      ;; Top border (only if borders enabled)
      (when border
        (insert (nth 0 border) "\n"))

      ;; Headline Title
      (let* ((theme-style (org-headline-card--get-theme-style))
             (base-title-size (string-to-number (alist-get 'titleFontSize theme-style)))
             (title-size (if no-border
                             (max 1 (round (* base-title-size org-headline-card-export-title-scale)))
                           base-title-size))
             (max-content-width (- border-width 2))
             (fixed-left-padding 2)
             (left-padding-str (make-string fixed-left-padding ?\s))
             (border-char (if no-border ""
                            (string (alist-get 'vertical
                                             (org-headline-card--get-border-chars
                                              (alist-get 'border-style
                                                        (alist-get org-headline-card-border-theme
                                                                  org-headline-card-border-themes))))))))
        (dolist (line title-lines)
          (let* ((line-len (string-width line))
                 (right-padding-len (if no-border
                                        0
                                      (max 0 (- max-content-width line-len fixed-left-padding))))
                 (right-padding-str (make-string right-padding-len ?\s))
                 (title-line (concat border-char
                                     left-padding-str
                                     line
                                     right-padding-str
                                     border-char)))
            (put-text-property 0 (length title-line) 'height title-size title-line)
            (insert title-line "\n")))
        
        ;; Separator line
        (if border
            (insert (org-headline-card--generate-border-line border-width 
                                                             (org-headline-card--get-border-chars
                                                              (alist-get 'border-style
                                                                        (alist-get org-headline-card-border-theme
                                                                                  org-headline-card-border-themes)))
                                                             'separator) "\n")
          ;; If no ASCII border, insert a placeholder for SVG vector line
          (let ((sep-line (make-string max-content-width ?\s)))
            (put-text-property 0 (length sep-line) 'org-headline-card-separator t sep-line)
            (insert sep-line "\n"))))

      ;; Content lines with side borders
      (when content-lines
        (dolist (line content-lines)
          (let* ((processed-line (org-headline-card--apply-text-properties line))
                 (max-content-width (- border-width 2)) ;; -2 for left and right border chars
                 (content-width (string-width processed-line))
                 ;; Fixed left padding of 2 spaces
                 (fixed-left-padding 2)
                 (left-padding-str (make-string fixed-left-padding ?\s))
                 ;; Calculate remaining space for right padding
                 (right-padding-len (if no-border
                                        0
                                      (max 0 (- max-content-width content-width fixed-left-padding))))
                 (right-padding-str (make-string right-padding-len ?\s))
                 (border-char (if no-border ""
                                (string (alist-get 'vertical
                                                 (org-headline-card--get-border-chars
                                                  (alist-get 'border-style
                                                            (alist-get org-headline-card-border-theme
                                                                      org-headline-card-border-themes)))))))
                 (content-line (concat border-char
                                      left-padding-str
                                      processed-line
                                      right-padding-str
                                      border-char)))
            (insert content-line "\n"))))

      ;; Bottom border (only if borders enabled)
      (when border
        (insert (nth (1- (length border)) border) "\n"))

      ;; Apply theme colors to entire buffer
      (let* ((fg-color (alist-get 'rectangleFontColor theme-style))
             (bg-color (alist-get 'rectangleBackgroundColor theme-style))
             (buffer-text (buffer-string)))
        (org-headline-card--apply-colors buffer-text fg-color bg-color)))))

(defun org-headline-card--apply-effect-range (text start end effect-name params)
  "Apply EFFECT-NAME to TEXT between START and END using PARAMS."
  (cond
   ((eq effect-name 'highlight)
    (let* ((color (or (plist-get params :color)
                      org-headline-card-effect-highlight-color))
           (rgba (or (org-headline-card--normalize-color-rgba color)
                     (concat org-headline-card-effect-highlight-color "FF")))
           (rgb (or (org-headline-card--rgba->rgb rgba)
                    org-headline-card-effect-highlight-color)))
      (add-face-text-property start end `(:background ,rgb) nil text)
      (put-text-property start end 'org-headline-card-effect 'highlight text)
      (put-text-property start end 'org-headline-card-effect-color rgba text)))
   ((eq effect-name 'glow)
    (let* ((color (or (plist-get params :color)
                      org-headline-card-effect-glow-color))
           (rgba (or (org-headline-card--normalize-color-rgba color)
                     (concat org-headline-card-effect-glow-color "FF"))))
      (put-text-property start end 'org-headline-card-effect 'glow text)
      (put-text-property start end 'org-headline-card-glow-color rgba text)))
   ((eq effect-name 'stroke)
    (let* ((color (or (plist-get params :color)
                      org-headline-card-effect-box-color))
           (rgba (or (org-headline-card--normalize-color-rgba color)
                     (concat org-headline-card-effect-box-color "FF")))
           (width (or (plist-get params :width)
                      org-headline-card-effect-box-width)))
      (put-text-property start end 'org-headline-card-effect 'stroke text)
      (put-text-property start end 'org-headline-card-box-color rgba text)
      (put-text-property start end 'org-headline-card-box-width width text)))
   ((eq effect-name 'wave)
    (let* ((color (or (plist-get params :color)
                      org-headline-card-effect-wave-color))
           (rgba (or (org-headline-card--normalize-color-rgba color)
                     (concat org-headline-card-effect-wave-color "FF")))
           (width (or (plist-get params :width)
                      org-headline-card-effect-wave-width)))
      (put-text-property start end 'org-headline-card-effect 'wave text)
      (put-text-property start end 'org-headline-card-wave-color rgba text)
      (put-text-property start end 'org-headline-card-wave-width width text))))
  text)

(defun org-headline-card--apply-effects-to-text (text effects)
  "Apply EFFECTS to TEXT by stable anchors when possible.
EFFECTS entries may include (LINE-TEXT COL-START COL-END) for anchor-based apply."
  (let* ((text-len (length text))
         (need-index (cl-some (lambda (e) (stringp (nth 5 e))) effects))
         (line-index
          (when need-index
            (let ((idx (make-hash-table :test #'equal))
                  (line-start 0))
              (while (< line-start text-len)
                (let* ((line-end (or (cl-position ?\n text :start line-start) text-len))
                       (line (substring text line-start line-end))
                       (key (string-trim-right line)))
                  (unless (gethash key idx)
                    (puthash key (list line-start line-end line) idx))
                  (setq line-start (min text-len (1+ line-end)))))
              idx))))
    (dolist (effect effects)
      (let* ((effect-name (nth 2 effect))
             (params (nth 3 effect))
             (selected-text (nth 4 effect))
             (anchor (nth 5 effect))
             (col-start (nth 6 effect))
             (col-end (nth 7 effect))
             (effect-def (org-headline-card-get-effect effect-name))
             (applied nil))
        (when (and selected-text effect-def (> (length selected-text) 0))
          (message "DEBUG: Looking for '%s' in text (len=%d)" selected-text (length text))
          ;; Anchor apply: line-text + display columns.
          (when (and (stringp anchor) (numberp col-start) (numberp col-end) line-index)
            (let ((entry (gethash anchor line-index)))
              (when entry
                (let* ((line-start (nth 0 entry))
                       (line-end (nth 1 entry))
                       (line (nth 2 entry))
                       (line-len (- line-end line-start))
                       (start-off (org-headline-card--column-to-char-index line col-start))
                       (end-off (org-headline-card--column-to-char-index line col-end))
                       (start-pos (+ line-start start-off))
                       (end-pos (+ line-start end-off)))
                  (setq start-pos (min (+ line-start line-len) (max line-start start-pos)))
                  (setq end-pos (min (+ line-start line-len) (max start-pos end-pos)))
                  (message "DEBUG: Applying %s by line-text/col at %d-%d (line \"%s\" col %d-%d)"
                           effect-name start-pos end-pos anchor col-start col-end)
                  (org-headline-card--apply-effect-range text start-pos end-pos effect-name params)
                  (setq applied t)))))
          ;; Fallback: substring search (apply all occurrences, capped).
          (unless applied
            (let ((search-start 0)
                  (count 0)
                  (max-matches 200))
              (while (and (< count max-matches)
                          (setq search-start (cl-search selected-text text :start2 search-start)))
                (let ((match-end (+ search-start (length selected-text))))
                  (message "DEBUG: Found '%s' at %d-%d, applying %s"
                           selected-text search-start match-end effect-name)
                  (org-headline-card--apply-effect-range text search-start match-end effect-name params)
                  (setq search-start match-end)
                  (setq count (1+ count))))
              (when (>= count max-matches)
                (message "DEBUG: Fallback apply stopped after %d matches for '%s'" max-matches selected-text)))))))
    text))

;;;###autoload
(defun org-headline-card-at-point ()
  "Convert org headline at point to card image and return the image path."
  (interactive)
  (let* ((element (org-element-at-point))
         (headline (org-element-property :raw-value element))
         (timestamp (format-time-string "%y%m%d"))
         (sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff]+" "-" headline))
         (sanitized-title (replace-regexp-in-string "^-+\\|-+$" "" sanitized-title))  ; trim leading/trailing dashes
         (output-filename (format "%s-%s.png" sanitized-title timestamp))
         (output-file (expand-file-name output-filename org-headline-card-directory))
         (theme-style (org-headline-card--get-theme-style))
         ;; For image export, we want clean text without ASCII borders.
         ;; The SVG engine will draw a vector border.
         (rendered-text (org-headline-card-layout-build-text element theme-style 'export))
         ;; Check for pending effects from preview
         (pending-effects (when (boundp 'org-headline-card--export-effects)
                            org-headline-card--export-effects)))

    ;; Apply pending effects to rendered text
    (when pending-effects
      (message "DEBUG: Applying %d effects, first effect: %S" (length pending-effects) (car pending-effects))
      (setq rendered-text (org-headline-card--apply-effects-to-text rendered-text pending-effects)))

    ;; Ensure output directory exists
    (org-headline-card--ensure-directory)

    ;; Export to image
    ;; (message "Exporting card to: %s" output-file)  ; Debug
    (org-headline-card--text-to-image rendered-text output-file theme-style)

    (when (called-interactively-p 'any)
      (message "Card image generated: %s" output-file))
    output-file))


(provide 'org-headline-card)
;;; org-headline-card.el ends here 
