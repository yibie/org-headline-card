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
  "Convert HEADLINE org-element and its contents to PlantUML string.
Ensures proper formatting of title, content, and list structure."
  (let* ((title (org-element-property :raw-value headline))
         (contents (org-headline-card--get-contents headline))
         (content-lines
          (when contents
            (org-headline-card--split-content contents)))
         (optimal-width (org-headline-card--calculate-width content-lines))
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
    (message "最终处理内容:\n%s" 
             (mapconcat (lambda (line)
                         (if (equal line "\\n")
                             "[空行]"
                           line))
                       wrapped-lines
                       "\n"))
    (format "@startuml
!theme plain
!pragma layout smetana

%s

rectangle \"<size:%s><b>%s</b></size>\\n\\n%s\" as card

@enduml"
            style-str
            title-size
            (org-headline-card--process-title title)
            (mapconcat
             (lambda (line)
               (if (equal line "\\n")
                   "\\n" ; 保持空行
                 (format "<size:%s>%s</size>\\n" content-size line)))
             wrapped-lines
             ""))))

(defun org-headline-card--get-contents (headline)
  "Extract and process contents under HEADLINE.
Preserves list structure while removing metadata like PROPERTIES drawer and timestamps.
Returns a string with proper formatting for both lists and paragraphs."
  (let ((begin (org-element-property :contents-begin headline))
        (end (org-element-property :contents-end headline))
        contents)
    (when (and begin end)
      (setq contents (buffer-substring-no-properties begin end))
      (message "原始内容:\n%s" contents)
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
          (message "清理后内容:\n%s" raw-content)
          raw-content)))))

(defun org-headline-card--split-content (content)
  "Split CONTENT into properly formatted lines while preserving order.
Convert org-mode markup to PlantUML format while preserving list structure."
  (message "开始分割内容:\n%s" content)
  (let* ((paragraphs (split-string content "\n\n+" t))
         (result '())
         (links '())  ; 存储所有链接
         (link-counter 0))
    (dolist (para paragraphs)
      (let ((processed-para
             (thread-last para
               ;; 处理列表标记
               (replace-regexp-in-string
                "^\\s-*[-*+]\\s-*"
                "• ")
               
               ;; 提取链接并替换为占位符
               (replace-regexp-in-string
                "\\[\\[\\(http[s]?://[^]]+?\\)\\]\\[\\([^]]+?\\)\\]\\]"
                (lambda (match)
                  (let* ((url (match-string 1 match))
                         (desc (match-string 2 match))
                         (placeholder (format "@@LINK_%d@@" link-counter)))
                    ;; 保存链接信息
                    (push (cons placeholder (cons desc url)) links)
                    (cl-incf link-counter)
                    desc)))  ; 只返回描述文本
               
               ;; 处理粗体
               (replace-regexp-in-string
                "\\*\\([^*\n]+?\\)\\*"
                "<b>\\1</b>")
               
               ;; 处理斜体（避免误匹配路径中的斜杠）
               (replace-regexp-in-string
                "\\(?:^\\|[^/]\\)/\\([^/\n]+?\\)/\\(?:$\\|[^/]\\)"
                "\\1<i>\\2</i>")
               
               ;; 处理下划线
               (replace-regexp-in-string
                "_\\([^_\n]+?\\)_"
                "<u>\\1</u>")
               
               ;; 处理删除线
               (replace-regexp-in-string
                "\\+\\([^+\n]+?\\)\\+"
                "<s>\\1</s>")
               
               ;; 处理代码和等宽字体
               (replace-regexp-in-string
                "\\(?:~\\|=\\)\\([^~=\n]+?\\)\\(?:~\\|=\\)"
                "<code>\\1</code>"))))
        
        (message "处理段落:\n%s\n变为:\n%s" para processed-para)
        (push processed-para result)))
    
    ;; 添加链接到结果，不添加额外空行
    (when links
      (dolist (link (nreverse links))
        (push (format "  <size:12><color:#666666>%s</color>" 
                     (cddr link))
              result)))
    
    (let ((final-result (nreverse result)))
      (message "分割后内容:\n%s" (mapconcat #'identity final-result "\n"))
      final-result)))
      
(defun org-headline-card--wrap-line (line width)
  "Wrap LINE at WIDTH characters, preserving format and readability.
Handles both list items and normal text differently, with proper indentation
and line breaks for Chinese text."
  (message "开始换行处理:\n%s" line)
  (with-temp-buffer
    (let* ((fill-column (- width 4))  ; 减少填充宽度以适应缩进
           (result '())
           (is-list-item (string-match-p "^\\s-*•" line)))
      
      ;; 设置中文文本的填充属性
      (setq-local word-wrap-by-category t)
      (setq-local word-separating-categories
                  '((?C . ?C)   ; 在中文字符间允许换行
                    (?c . ?c)))
      
      ;; 处理列表项
      (if is-list-item
          ;; 列表项处理
          (let* ((items (split-string line "• \\s-*" t))
                 (indent "  "))
            ;; 处理每个列表项
            (dolist (item items)
              (unless (string-empty-p item)
                ;; 插入并填充当前项
                (erase-buffer)
                (insert item)
                (fill-region (point-min) (point-max))
                
                ;; 收集处理后的行
                (goto-char (point-min))
                (let ((first-line t))
                  (while (not (eobp))
                    (let ((current-line (buffer-substring (line-beginning-position)
                                                        (line-end-position))))
                      (push (concat 
                             (if first-line
                                 "• "  ; 第一行使用项目符号
                               indent) ; 后续行使用缩进
                             current-line)
                            result)
                      (setq first-line nil))
                    (forward-line 1))))))
        
        ;; 普通段落处理
        (insert line)
        (fill-region (point-min) (point-max))
        
        ;; 收集处理后的行
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((current-line (buffer-substring (line-beginning-position)
                                               (line-end-position)))
                 ;; 普通段落：第一行不缩进，后续行缩进
                 (indented-line (if (= (line-number-at-pos) 1)
                                  current-line
                                (concat "  " current-line))))
            (push indented-line result))
          (forward-line 1)))
      
      ;; 返回处理后的行列表
      (let ((final-result (nreverse result)))
        (message "换行后内容:\n%s" (mapconcat #'identity final-result "\n"))
        final-result))))

(defun org-headline-card--process-title (title)
  "Process the title string for PlantUML."
  (let ((processed-title
         (thread-last title
           ;; 处理 org-mode 链接，只保留描述文本
           (replace-regexp-in-string
            "\\[\\[\\(?:[^][]\\|\\[\\(?:[^][]\\|\\[[^][]\\*\\]\\)*\\]\\)*\\]\\[\\(.*?\\)\\]\\]"
            "\\1")
           ;; 处理分隔符
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
