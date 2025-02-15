;;; org-headline-card.el --- Convert org headlines to preview cards -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Yibie <yibie@outlook.com>
;; Version: 0.1.0
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
;; their contents into preview card images using PlantUML.  It's useful
;; for creating visual representations of Org headlines for presentations,
;; documentation, or quick previews.

;; Features:
;; - Converts Org headlines to preview cards
;; - Handles Org markup (bold, italic, links, etc.)
;; - Customizable card style
;; - Preserves list structure

;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-headline-card nil
  "Convert org headlines to preview cards."
  :group 'org)

(defcustom org-headline-card-directory
  (expand-file-name "headline-cards" user-emacs-directory)
  "Directory to store generated card images."
  :type 'directory
  :group 'org-headline-card)

(defcustom org-headline-card-max-width 60
  "Maximum width of the card content in characters.
If content is shorter, the card will be narrower."
  :type 'integer
  :group 'org-headline-card)

(defcustom org-headline-card-plantuml-params
  '(("-DPLANTUML_LIMIT_SIZE" . "16384")
    ("-Dcmapx" . "false")
    ("-Dgenerate_cmapx" . "false")
    ("-Dpragma.graphviz.dot" . "smetana")
    ("-Dpng.compression" . "0"))
  "Technical parameters passed to PlantUML for image generation."
  :type '(alist :key-type string :value-type string)
  :group 'org-headline-card)

(defcustom org-headline-card-plantuml-extra-args nil
  "Additional command line arguments passed to PlantUML.
Each element is a string that will be passed as-is to the PlantUML command."
  :type '(repeat string)
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
  `((chinese-modern . ((defaultFontName . "Microsoft YaHei")  
                      (defaultFontSize . "16")                
                      (backgroundColor . "#FFFFFF")
                      (rectangleBorderColor . "#E8E8E8")
                      (rectangleFontColor . "#262626")        
                      (rectangleBackgroundColor . "#FFFFFF")))
    
    (chinese-ink . ((defaultFontName . "KingHwa_OldSong")   
                    (defaultFontSize . "16")
                    (backgroundColor . "#FAFAF8")
                    (rectangleBorderColor . "#D4D4D4")
                    (rectangleFontColor . "#1A1A1A")
                    (rectangleBackgroundColor . "#FCFCFA")))
    
    (chinese-screen . ((defaultFontName . "FZKeBenFangSongS-R-GB")
                      (defaultFontSize . "24")                
                      (backgroundColor . "#F8F4E9")
                      (rectangleBorderColor . "#E5DED0")
                      (rectangleFontColor . "#3F3F3F")
                      (rectangleBackgroundColor . "#FAF6ED")))

    (light . ((defaultFontName . "Microsoft YaHei")
              (defaultFontSize . "28")
              (backgroundColor . "#FFFFFF")
              (rectangleBorderColor . "#DDDDDD")
              (rectangleFontColor . "#2C3E50")
              (rectangleBackgroundColor . "#FFFFFF")))

    (dark . ((defaultFontName . "Arial")
             (defaultFontSize . "14")
             (backgroundColor . "#2B2B2B")
             (rectangleBorderColor . "#404040")
             (rectangleFontColor . "#E8E8E8")
             (rectangleBackgroundColor . "#333333")
             (shadowing . "true")))       
    
    (warm . ((defaultFontName . "Arial")
             (defaultFontSize . "14")
             (backgroundColor . "#FAF7F2")
             (rectangleBorderColor . "#E8D5C4")
             (rectangleFontColor . "#5C4033")
             (rectangleBackgroundColor . "#FFF8F0"))))
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
  (message "Using image directory: %s" org-headline-card-directory))

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

(defun org-headline-card--headline-to-plantuml (headline)
  "Convert HEADLINE org-element and its contents to PlantUML string."
  (let* ((title (org-element-property :raw-value headline))
         (contents (org-headline-card--get-contents headline))
         (content-lines
          (when contents
            (org-headline-card--split-content contents)))
         (optimal-width (org-headline-card--calculate-width content-lines))
         ;; Process title with wrapping
         (wrapped-title
          (mapconcat #'identity wrapped-title-lines "\\n"))
         (wrapped-lines
          (mapcan (lambda (line)
                   (if (string-empty-p line)
                       (list "\\n")
                     (org-headline-card--wrap-line line optimal-width)))
                 content-lines))
         (theme-style (org-headline-card--get-theme-style))
         (style-str
          (mapconcat
           (lambda (style)
             (format "skinparam %s %s"
                     (symbol-name (car style))
                     (cdr style)))
           theme-style
           "\n"))
         (title-size (alist-get 'titleFontSize theme-style))
         (content-size (alist-get 'contentFontSize theme-style)))
    (format "@startuml
!theme plain
!pragma layout smetana

@startuml

%s

rectangle \"<size:%s><b>%s</b></size>\\n\\n%s\" as card

@enduml"
            style-str
            title-size
            wrapped-title  ; 使用处理过的标题
            (mapconcat
             (lambda (line)
               (if (equal line "\\n")
                   "\\n"
                 (format "<size:%s>%s</size>\\n" content-size line)))
             wrapped-lines
             ""))))

(defun org-headline-card--get-contents (headline)
  "Extract contents under HEADLINE, excluding PROPERTIES drawer and timestamps.
Ensure proper spacing and alignment for non-list content."
  (let ((begin (org-element-property :contents-begin headline))
        (end (org-element-property :contents-end headline))
        contents)
    (when (and begin end)
      (setq contents (buffer-substring-no-properties begin end))
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
        (let* ((raw-content (buffer-string))
               ;; Split into paragraphs (double newline as separator)
               (paragraphs (split-string raw-content "\n\n+" t))
               ;; Process each paragraph
               (formatted-paragraphs
                (mapcar (lambda (para)
                         ;; Remove leading/trailing whitespace
                         (string-trim
                          ;; Replace single newlines with spaces
                          (replace-regexp-in-string "\n" " " para)))
                       paragraphs)))
          ;; Join paragraphs with double newlines
          (mapconcat #'identity
                    (seq-filter (lambda (para)
                                (string-match-p "\\S-" para))
                              formatted-paragraphs)
                    "\n\n"))))))

(defun org-headline-card--split-content (content)
  "Split CONTENT into properly formatted lines while preserving order.
Convert org-mode markup to creole format for PlantUML.
Ensure proper spacing between paragraphs."
  (let* ((paragraphs (split-string content "\n\n+" t))
         (result '()))
    (dolist (para paragraphs)
      (let ((processed-para
             (thread-last para
               ;; Process org-mode links
               (replace-regexp-in-string
                "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]"
                "\\1")
               ;; Process bold markup
               (replace-regexp-in-string
                "\\*\\([^*\n]+?\\)\\*"
                "<b>\\1</b>")
               ;; Process network links
               (replace-regexp-in-string
                "\\[\\[\\(http[s]?://.*?\\)\\]\\[\\(.*?\\)\\]\\]"
                "[\\2](\\1)")
               ;; Process italic markup with stricter pattern
               (replace-regexp-in-string
                "\\(?:^\\|[^/]\\)/\\([^/\n]+?\\)/\\(?:$\\|[^/]\\)"
                "\\1<i>\\2</i>")
               ;; Process underline markup
               (replace-regexp-in-string
                "_\\([^_\n]+?\\)_"
                "<u>\\1</u>")
               ;; Process strikethrough markup
               (replace-regexp-in-string
                "\\+\\([^+\n]+?\\)\\+"
                "<s>\\1</s>")
               ;; Process code markup
               (replace-regexp-in-string
                "~\\([^~\n]+?\\)~"
                "<code>\\1</code>")
               ;; Process monospace markup
               (replace-regexp-in-string
                "=\\([^=\n]+?\\)="
                "<code>\\1</code>")
               ;; Process list markup
               (replace-regexp-in-string
                "^\\s-*[-*+]\\s-*"
                "• "))))
        ;; Add empty line between paragraphs
        (when result
          (push "" result))
        (push processed-para result)))
    (nreverse result)))

(defun org-headline-card--clean-line (line)
  "Clean up redundant list markers and normalize spacing in LINE."
  (let ((cleaned-line
         (thread-last line
           ;; Remove redundant dashes (if already preceded by a bullet)
           (replace-regexp-in-string "^\\(•\\)\\s-*-\\s-*" "\\1 ")
           ;; Ensure single space after bullet
           (replace-regexp-in-string "^\\(•\\)\\s-+" "\\1 ")
           ;; Remove trailing whitespace
           (replace-regexp-in-string "\\s-+$" ""))))
    cleaned-line))

(defun org-headline-card--wrap-line (line width)
  "Wrap LINE at WIDTH characters, preserving readability.
Handles both list items and normal paragraphs appropriately."
  (with-temp-buffer
    (let ((fill-column width)
          (result '())
          (is-list-item (string-match-p "^\\s-*[-*+]\\s-+" line)))
      ;; Set Chinese text wrapping properties
      (setq-local word-wrap-by-category t)
      (setq-local word-separating-categories
                  '((?C . ?C) (?c . ?c)))
      
      ;; Remove any existing line breaks and normalize spaces
      (insert (replace-regexp-in-string "[ \n\t]+" " " line))
      
      ;; Calculate actual text width considering Chinese characters
      (let* ((text-width (string-width (buffer-string)))
             (need-wrap (> text-width width)))
        
        (if (not need-wrap)
            ;; If text is shorter than width, return as single line
            (list (if is-list-item
                     (concat "• " (string-trim (buffer-string)))
                   (string-trim (buffer-string))))
          
          ;; Text needs wrapping
          (erase-buffer)
          (insert line)
          
          ;; Try to break at punctuation first
          (let ((break-points '())
                (pos 1))
            (while (< pos (point-max))
              (when (string-match-p "[，。；：！？、]" (buffer-substring pos (1+ pos)))
                (push pos break-points))
              (setq pos (1+ pos)))
            
            (if break-points
                ;; Use punctuation break points
                (let ((current-pos (point-min))
                      (current-line ""))
                  (dolist (break-pos (nreverse break-points))
                    (let ((segment (buffer-substring current-pos (1+ break-pos))))
                      (if (> (+ (string-width current-line) (string-width segment)) width)
                          (progn
                            (push (string-trim current-line) result)
                            (setq current-line segment))
                        (setq current-line (concat current-line segment)))))
                  (when (not (string-empty-p current-line))
                    (push (string-trim current-line) result)))
              
              ;; Fall back to standard fill
              (let ((adaptive-fill-mode nil))
                (fill-region (point-min) (point-max))
                (goto-char (point-min))
                (while (not (eobp))
                  (let ((current-line (string-trim
                                     (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))
                    (unless (string-empty-p current-line)
                      (push current-line result))
                    (forward-line)))))
            
            ;; Add proper indentation for list items
            (setq result (nreverse result))
            (if is-list-item
                (mapcar (lambda (line)
                         (concat (if (= (length result) 1) "• " "  ") line))
                       result)
              result)))))))

(defun org-headline-card--process-title (title)
  "Process the title string for PlantUML."
  (let ((processed-title
         (thread-last title
           ;; Process org-mode links, only keeping the description text
           (replace-regexp-in-string
            "\\[\\[\\(?:[^][]\\|\\[\\(?:[^][]\\|\\[[^][]\\*\\]\\)*\\]\\)*\\]\\[\\(.*?\\)\\]\\]"
            "\\1")
           ;; Process separators
           (replace-regexp-in-string
            "|"
            "·"))))
    (org-headline-card--escape-string processed-title)))

(defun org-headline-card--format-line (line)
  "Format a single LINE for PlantUML display."
  (thread-last line
    ;; Remove extra whitespace
    (replace-regexp-in-string "^\\s-+" "")
    (replace-regexp-in-string "\\s-+$" "")
    ;; Process special characters
    (org-headline-card--escape-string)))

(defun org-headline-card--escape-string (str)
  "Escape special characters in STR for PlantUML."
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
    ;; Convert newline characters to PlantUML newline markers
    (replace-regexp-in-string
     "\n"
     "\\\\n"
     (buffer-string))))

(defun org-headline-card--generate-image (plantuml-str headline)
  "Generate image from PLANTUML-STR and HEADLINE."
  (org-headline-card--ensure-directory)
  (let* ((title (org-element-property :raw-value headline))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title))
         (output-filename (format "card-%s-%s.png" sanitized-title timestamp))
         (temp-file (make-temp-file "org-headline-card-" nil ".puml"))
         (output-file (expand-file-name output-filename org-headline-card-directory))
         (error-buffer (generate-new-buffer "*plantuml-error*"))
         (jar-path (expand-file-name plantuml-jar-path))
         (args (append
                (list "-jar" jar-path "-tpng")
                (mapcar (lambda (param)
                         (format "%s=%s" (car param) (cdr param)))
                       org-headline-card-plantuml-params)
                org-headline-card-plantuml-extra-args
                (list temp-file "-o" output-file))))
    ;; Add more detailed debugging information
    (message "Generated PlantUML content:\n%s" plantuml-str)
    (message "Writing to temp file: %s" temp-file)
    (with-temp-file temp-file
      (insert plantuml-str))
    (message "Command: java %s" (mapconcat #'identity args " "))
    (let ((result (apply #'call-process
                        "java"
                        nil
                        error-buffer
                        nil
                        args)))
      (unless (= result 0)
        (with-current-buffer error-buffer
          (let ((error-msg (buffer-string)))
            (message "PlantUML error output:\n%s" error-msg)))
        (kill-buffer error-buffer)
        (error "PlantUML process failed with code %d" result)))
    (kill-buffer error-buffer)
    (delete-file temp-file)
    output-file))

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
    (or available-fonts '("sans-serif"))))  ; 如果没有找到中文字体，使用 sans-serif

(defun org-headline-card--get-font-string ()
  "Get font string with available CJK fonts."
  (string-join (org-headline-card--available-cjk-fonts) ", "))

;;;###autoload
(defun org-headline-card-at-point ()
  "Convert org headline at point to card image and return the image path."
  (interactive)
  (let* ((element (org-element-at-point))
         (plantuml-str (org-headline-card--headline-to-plantuml element))
         (image-file (org-headline-card--generate-image plantuml-str element)))
    (when (called-interactively-p 'any)
      (message "Card image generated: %s" image-file))
    image-file))


(provide 'org-headline-card)
;;; org-headline-card.el ends here 
