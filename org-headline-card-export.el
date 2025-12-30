;;; org-headline-card-export.el --- Image export functionality (SVG fix) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-headline-card-effects)

;; Export configuration variables
(defcustom org-headline-card-export-font-size 16
  "Font size (in points) for SVG export."
  :type 'integer
  :group 'org-headline-card)

(defcustom org-headline-card-export-width 50
  "Maximum width (in characters) for SVG export.
Exported images keep a fixed width for consistent publishing."
  :type 'integer
  :group 'org-headline-card)

(defcustom org-headline-card-export-dpi 72
  "DPI (dots per inch) for PNG export via ImageMagick.
Higher values produce sharper images but larger file sizes.
Common values: 72 (screen), 144 (2x retina), 300 (print quality)."
  :type 'integer
  :group 'org-headline-card)

(defcustom org-headline-card-export-convert-timeout 60
  "Maximum seconds to wait for ImageMagick conversion.
When nil, wait without timeout."
  :type '(choice (const :tag "No timeout" nil)
                 (integer :tag "Seconds"))
  :group 'org-headline-card)

(defcustom org-headline-card-export-keep-svg nil
  "When non-nil, keep the intermediate SVG file and report its path."
  :type 'boolean
  :group 'org-headline-card)

(defcustom org-headline-card-export-highlight-mode 'measure
  "Highlight rendering mode for SVG export.
When set to `measure', measure text width using Emacs rendering.
When set to `per-char', draw per-character rects for debugging.
When set to `range', draw a single rect using char-width estimates."
  :type '(choice (const :tag "Measured width" measure)
                 (const :tag "Per character (debug)" per-char)
                 (const :tag "Range" range))
  :group 'org-headline-card)

(defcustom org-headline-card-export-debug-highlight nil
  "When non-nil, draw debug overlays for highlight alignment."
  :type 'boolean
  :group 'org-headline-card)

(defcustom org-headline-card-export-glow-blur 2
  "Blur strength for glow filter."
  :type 'integer
  :group 'org-headline-card)

(defcustom org-headline-card-export-wave-amplitude 2
  "Amplitude (px) of exported wavy underline."
  :type 'integer
  :group 'org-headline-card)

(defcustom org-headline-card-export-wave-period 10
  "Wavelength (px) of exported wavy underline."
  :type 'integer
  :group 'org-headline-card)

;; Helper function to find a suitable monospace font
(defun org-headline-card--find-mono-font ()
  "Find a suitable monospace font that supports CJK characters.
Returns the font file path if found, nil otherwise."
  (cl-loop for font in
           (cond
            ((eq system-type 'darwin)
             '("/Library/Fonts/LXGWWenKaiMono-Regular.ttf"
               "/Users/chenyibin/Library/Fonts/LXGWWenKaiMono-Regular.ttf"
               "/System/Library/Fonts/Hiragino Sans GB.ttc"
               "/System/Library/Fonts/STHeiti Medium.ttc"))
            ((eq system-type 'gnu/linux)
             '("/usr/share/fonts/truetype/lxgw/LXGWWenKaiMono-Regular.ttf"
               "~/.local/share/fonts/LXGWWenKaiMono-Regular.ttf"
               "/usr/share/fonts/truetype/noto/NotoSansCJK-Regular.ttc"))
            ((eq system-type 'windows-nt)
             '("C:/Windows/Fonts/LXGWWenKaiMono-Regular.ttf"
               "C:/Windows/Fonts/msyh.ttc"))
            (t
             '()))
           when (file-exists-p font)
           return font
           finally return nil))

;; Built-in background patterns
(defconst org-headline-card--patterns
  '((dots . "<circle cx=\"10\" cy=\"10\" r=\"2\" fill=\"%s\"/>")
    (grid . "<path d=\"M 20 0 L 0 0 0 20\" fill=\"none\" stroke=\"%s\" stroke-width=\"0.5\"/>")
    (lines . "<path d=\"M 0 20 L 20 0\" fill=\"none\" stroke=\"%s\" stroke-width=\"0.5\"/>")
    (cross-lines . "<path d=\"M 0 20 L 20 0 M 0 0 L 20 20\" fill=\"none\" stroke=\"%s\" stroke-width=\"0.5\"/>")
    (waves . "<path d=\"M 0 10 Q 5 5 10 10 T 20 10\" fill=\"none\" stroke=\"%s\" stroke-width=\"0.5\"/>"))
  "Built-in SVG patterns for card backgrounds.
Each pattern is a format string that takes a color argument.")

(defun org-headline-card--generate-pattern-svg (pattern-name color opacity)
  "Generate SVG pattern definition for PATTERN-NAME with COLOR and OPACITY."
  (let ((pattern-template (alist-get pattern-name org-headline-card--patterns)))
    (when pattern-template
      (format "    <pattern id=\"bgPattern\" patternUnits=\"userSpaceOnUse\" width=\"20\" height=\"20\" opacity=\"%s\">
      %s
    </pattern>\n"
              opacity
              (format pattern-template color)))))

;; Helper function to convert image file to base64 data URI
(defun org-headline-card--image-to-base64 (image-path)
  "Convert IMAGE-PATH to base64 data URI string.
Returns nil if file doesn't exist or can't be read."
  (when (and image-path (file-exists-p image-path))
    (let* ((extension (downcase (file-name-extension image-path)))
           (mime-type (pcase extension
                        ("png" "image/png")
                        ("jpg" "image/jpeg")
                        ("jpeg" "image/jpeg")
                        ("gif" "image/gif")
                        ("webp" "image/webp")
                        (_ "image/png"))))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally image-path)
        (format "data:%s;base64,%s"
                mime-type
                (base64-encode-string (buffer-string) t))))))

;; Helper function to escape special XML characters
(defun org-headline-card--escape-svg-text (text)
  "Escape special XML characters in TEXT for SVG."
  (setq text (replace-regexp-in-string "&" "&amp;" text))
  (setq text (replace-regexp-in-string "<" "&lt;" text))
  (setq text (replace-regexp-in-string ">" "&gt;" text))
  (setq text (replace-regexp-in-string "\"" "&quot;" text))
  text)

(defun org-headline-card--svg-id-safe (s)
  "Return a safe SVG id fragment derived from S."
  (let* ((down (downcase (format "%s" s)))
         (nohash (replace-regexp-in-string "#" "" down)))
    (replace-regexp-in-string "[^a-z0-9_-]+" "_" nohash)))

(defun org-headline-card--split-rgba (rgba)
  "Split RGBA string into (RGB OPACITY).
RGBA may be #RRGGBB, #RRGGBBAA, or 16-bit forms; OPACITY is a float 0..1."
  (let* ((n (org-headline-card--normalize-color-rgba rgba))
         (rgb (and n (substring n 0 7)))
         (aa (and n (substring n 7 9)))
         (alpha (if aa (string-to-number aa 16) 255)))
    (list rgb (/ alpha 255.0))))

(defun org-headline-card--wave-path-d (x y width amplitude period)
  "Return an SVG path d for a wave from (X,Y) with WIDTH."
  (let* ((amp (max 1 amplitude))
         (per (max 4 period))
         (half (/ per 2.0))
         (n (max 1 (ceiling (/ width half))))
         (d (format "M %d %d" x y))
         (dir 1)
         (remaining width))
    (dotimes (_ n)
      (let* ((step (min half remaining))
             (ctrl (/ step 2.0))
             (cy (* dir amp)))
        (setq d (format "%s q %.2f %.2f %.2f 0" d ctrl cy step))
        (setq dir (* -1 dir))
        (setq remaining (- remaining step))))
    d))

(defun org-headline-card--get-svg-style-for-face (face)
  "Convert Emacs FACE to SVG style attributes.
Returns a string of CSS classes or style attributes."
  (let ((classes '()))
    (cond
     ((listp face)
      (when (memq 'bold face) (push "bold" classes))
      (when (memq 'italic face) (push "italic" classes))
      (when (memq 'font-lock-string-face face) (push "code" classes)))
     ((symbolp face)
      (when (eq face 'bold) (push "bold" classes))
      (when (eq face 'italic) (push "italic" classes))
      (when (eq face 'font-lock-string-face) (push "code" classes))))
    (mapconcat #'identity (nreverse classes) " ")))

(defun org-headline-card--extract-background-color (face)
  "Extract background color from FACE property.
FACE can be a face symbol, a list of faces, or a plist."
  (cond
   ;; plist like (:background "#FFFF00")
   ((and (listp face) (plist-get face :background))
    (plist-get face :background))
   ;; List of face specs - check each
   ((listp face)
    (cl-loop for f in face
             when (and (listp f) (plist-get f :background))
             return (plist-get f :background)))
   (t nil)))

(defun org-headline-card--trim-leading-space (line)
  "Trim leading whitespace in LINE while preserving text properties."
  (if (string-match "^\\s-+" line)
      (substring line (match-end 0))
    line))

(defun org-headline-card--frame-ppi ()
  "Return the current frame's pixels-per-inch estimate."
  (if (display-graphic-p)
      (let* ((attrs (frame-monitor-attributes))
             (mm-size (cdr (assq 'mm-size attrs)))
             (geom (cdr (assq 'geometry attrs))))
        (if (and mm-size geom)
            (let* ((mm-width (car mm-size))
                   (px-width (nth 2 geom))
                   (inches (/ mm-width 25.4)))
              (if (> inches 0)
                  (/ px-width inches)
                96.0))
          96.0))
    96.0))

(defun org-headline-card--px-to-face-height (px)
  "Convert PX to an Emacs face :height value (1/10 pt)."
  (round (* px 10.0 (/ 72.0 (org-headline-card--frame-ppi)))))

(defun org-headline-card--prepare-measure-string (string)
  "Normalize STRING for pixel measurement by converting :height units."
  (let ((s (copy-sequence string))
        (pos 0)
        (len (length string)))
    (while (< pos len)
      (let* ((next (next-single-property-change pos 'height s len))
             (height (get-text-property pos 'height s)))
        (when height
          (put-text-property pos next 'height
                             (org-headline-card--px-to-face-height height)
                             s))
        (setq pos next)))
    s))

(defun org-headline-card--measure-string-px (string font-family font-size)
  "Measure STRING in pixels using Emacs rendering.
Returns nil if pixel measurement isn't available."
  (when (and (fboundp 'string-pixel-width) (display-graphic-p))
    (let ((buf (get-buffer-create " *org-headline-card-measure*")))
      (with-current-buffer buf
        (let ((remap (face-remap-add-relative
                      'default :family font-family
                      :height (org-headline-card--px-to-face-height font-size))))
          (unwind-protect
              (string-pixel-width (org-headline-card--prepare-measure-string string))
            (face-remap-remove-relative remap)))))))

(defun org-headline-card--line-effect-ranges (line effect-name &optional color-prop)
  "Return ranges in LINE as (START END COLOR) for EFFECT-NAME.
COLOR-PROP specifies the property for color (default highlight color)."
  (let* ((pos 0)
         (len (length line))
         (ranges '()))
    (while (< pos len)
      (let* ((next-change (next-single-property-change
                           pos 'org-headline-card-effect line len))
             (effect (get-text-property pos 'org-headline-card-effect line))
             (color (and (eq effect effect-name)
                         (or (and color-prop
                                  (get-text-property pos color-prop line))
                             "#FFFF00"))))
        (when (and color (< pos next-change))
          (push (list pos next-change color) ranges))
        (setq pos next-change)))
    (nreverse ranges)))

(defun org-headline-card--line-highlight-ranges (line)
  "Return highlight ranges in LINE as (START END COLOR)."
  (org-headline-card--line-effect-ranges
   line 'highlight 'org-headline-card-effect-color))

(defun org-headline-card--line-highlight-char-boxes (line start end line-char-width padding-px y-acc l-height color &optional outline-only)
  "Return SVG rects for each character in LINE from START to END.
Boxes are computed using `char-width' and the given layout params.
When OUTLINE-ONLY is non-nil, draw stroke-only boxes."
  (let* ((pos start)
         (x-cols (string-width (substring line 0 start)))
         (rects '()))
    (while (< pos end)
      (let* ((ch (aref line pos))
             (cols (max 1 (char-width ch)))
             (rect-x (+ padding-px (round (* x-cols line-char-width))))
             (rect-y (round y-acc))
             (rect-w (max 1 (round (* cols line-char-width))))
             (rect-h (round l-height)))
        (if outline-only
            (push (format "  <rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"#FF0000\" stroke-width=\"0.5\" opacity=\"0.8\"/>\n"
                          rect-x rect-y rect-w rect-h)
                  rects)
          (push (format "  <rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\" opacity=\"0.4\"/>\n"
                        rect-x rect-y rect-w rect-h color)
                rects))
        (setq x-cols (+ x-cols cols))
        (setq pos (1+ pos))))
    (nreverse rects)))

(defun org-headline-card--line-effect-segments (line effect-name)
  "Return list of (START END) segments for EFFECT-NAME in LINE."
  (let* ((pos 0)
         (len (length line))
         (segments '()))
    (while (< pos len)
      (let* ((next-change (next-single-property-change
                           pos 'org-headline-card-effect line len))
             (effect (get-text-property pos 'org-headline-card-effect line)))
        (when (and (eq effect effect-name) (< pos next-change))
          (push (list pos next-change) segments))
        (setq pos next-change)))
    (nreverse segments)))

(defun org-headline-card--text-to-svg-tspans (text)
  "Convert TEXT with properties to SVG tspans (no trimming)."
  (if (equal text "")
      ""
    (let ((result "")
          (pos 0)
          (len (length text))
          (line text))
      (while (< pos len)
        (let* ((next-change-face (next-single-property-change pos 'face line len))
               (next-change-underline (next-single-property-change pos 'underline line len))
               (next-change-height (next-single-property-change pos 'height line len))
               (next-change-effect (next-single-property-change pos 'org-headline-card-effect line len))
               (limit (min next-change-face
                           (min next-change-underline
                                (min next-change-height next-change-effect))))
               (chunk (substring line pos limit))
               (face (get-text-property pos 'face line))
               (underline (get-text-property pos 'underline line))
               (height (get-text-property pos 'height line))
               (classes (org-headline-card--get-svg-style-for-face face))
               (style-attr ""))

          (when height
            (setq style-attr (format "font-size: %dpx;" height)))
          (when underline
            (setq classes (concat classes (if (string-empty-p classes) "" " ") "underline")))
          (setq chunk (org-headline-card--escape-svg-text chunk))

          (let ((class-attr (if (string-empty-p classes) "" (format " class=\"%s\"" classes)))
                (style-tag (if (string-empty-p style-attr) "" (format " style=\"%s\"" style-attr))))
            (if (and (string-empty-p classes) (string-empty-p style-attr))
                (setq result (concat result chunk))
              (setq result (concat result (format "<tspan%s%s>%s</tspan>" class-attr style-tag chunk)))))
          (setq pos limit)))
      result)))

(defun org-headline-card--line-to-svg-tspans (line)
  "Convert a line of text with properties to SVG tspans."
  (if (equal line "")
      ""
    (org-headline-card--text-to-svg-tspans line)))

;; Simplified SVG export without complex text-properties handling
(defun org-headline-card--text-to-svg (text svg-file theme-style)
  "Convert TEXT to SVG file SVG-FILE (simplified version)."
  (let ((svg-content (org-headline-card--text-to-svg-string text theme-style)))
    (with-temp-file svg-file
      (insert svg-content))
    svg-file))

(defun org-headline-card--text-to-svg-string (text theme-style)
  "Convert TEXT to SVG string with full styling support.
THEME-STYLE is the theme configuration.
Returns SVG string that can be displayed in browsers or Emacs."
  (let* ((fg-color (or (alist-get 'rectangleFontColor theme-style) "#000000"))
         (bg-color (or (alist-get 'rectangleBackgroundColor theme-style) "#FFFFFF"))
         (border-color (or (alist-get 'rectangleBorderColor theme-style) fg-color))
         (bg-gradient (alist-get 'backgroundGradient theme-style))
         (bg-image (alist-get 'backgroundImage theme-style))
         (bg-image-opacity (or (alist-get 'backgroundImageOpacity theme-style) "1.0"))
         (bg-pattern (alist-get 'backgroundPattern theme-style))
         (bg-pattern-color (or (alist-get 'backgroundPatternColor theme-style) "#E0E0E0"))
         (bg-pattern-opacity (or (alist-get 'backgroundPatternOpacity theme-style) "0.3"))
         (bg-gradient-type (or (alist-get 'backgroundGradientType theme-style) 'linear))
         (font-family (or (alist-get 'defaultFontName theme-style) "LXGW WenKai Mono"))
         ;; Shadow configuration (disabled - SVG shadow has poor cross-renderer compatibility)
         ;; (shadow-enabled (alist-get 'shadow theme-style))
         ;; (shadow-color (or (alist-get 'shadowColor theme-style) "#000000"))
         ;; (shadow-opacity (or (alist-get 'shadowOpacity theme-style) "0.25"))
         ;; (shadow-blur (string-to-number (or (alist-get 'shadowBlur theme-style) "4")))
         ;; (shadow-offset-x (string-to-number (or (alist-get 'shadowOffsetX theme-style) "4")))
         ;; (shadow-offset-y (string-to-number (or (alist-get 'shadowOffsetY theme-style) "4")))
         ;; (shadow-extra (if shadow-enabled (+ (* shadow-blur 2) (max shadow-offset-x shadow-offset-y) 5) 0))
         (shadow-enabled nil)
         (shadow-extra 0)
         ;; Determine border style from current border theme
         (border-theme-config (alist-get org-headline-card-border-theme org-headline-card-border-themes))
         (corner-style (alist-get 'corner-style border-theme-config))
         (corner-radius (if (eq corner-style 'round) "15" "0"))
         (border-stroke-width "3")
         (lines (split-string text "\n" nil))
         ;; char-width ≈ font-size * 0.65 for monospace fonts
         (base-char-width (* org-headline-card-export-font-size 0.65))
         ;; Maximum width in pixels (based on org-headline-card-export-width)
         (max-content-width-px (* org-headline-card-export-width base-char-width))
         ;; We'll calculate height and actual content width dynamically
         (total-height 0)
         (max-line-width-px 0)
         (processed-lines '())
         (svg-parts '())
         (glow-colors '())
         (padding-px 20))

    ;; First pass: calculate dimensions and collect effect parameters.
    (dolist (line lines)
	      (let* ((raw-height (or (cl-loop for pos from 0 below (length line)
	                                      maximize (or (get-text-property pos 'height line) 0))
	                             0))
	             ;; Fix: (or 0 default) returns 0 in Elisp since 0 is truthy
	             (line-max-height (if (zerop raw-height)
	                                  org-headline-card-export-font-size
	                                raw-height))
             (line-spacing (* line-max-height 1.4))
	             ;; Calculate this line's pixel width
	             (line-char-width (* line-max-height 0.65))
	             (line-width-px (or (org-headline-card--measure-string-px
	                                 line font-family line-max-height)
	                                (* (string-width line) line-char-width))))
	        ;; Collect glow colors used in this line (for per-color filters).
	        (let* ((glow-segs (org-headline-card--line-effect-segments line 'glow))
	               (tline line))
	          (dolist (seg glow-segs)
	            (let* ((start (nth 0 seg))
	                   (color (or (get-text-property start 'org-headline-card-glow-color tline)
	                              org-headline-card-effect-glow-color)))
              (push color glow-colors))))
        (setq max-line-width-px (max max-line-width-px line-width-px))
        (setq total-height (+ total-height line-spacing))
        (push (list line line-max-height line-spacing) processed-lines)))
    (setq processed-lines (nreverse processed-lines))

    ;; Content width: fixed to configured export width
    (let* ((content-width max-content-width-px)
           (content-height total-height)
           ;; Add extra space for shadow if enabled
           (svg-width (+ content-width (* padding-px 2) shadow-extra))
           (svg-height (+ content-height (* padding-px 2) shadow-extra)))

      ;; Build SVG parts
      (push "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" svg-parts)
      (push (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\" shape-rendering=\"geometricPrecision\">
" 
                    svg-width svg-height svg-width svg-height)
            svg-parts)
      (push "  <defs>\n" svg-parts)
      (push "    <style>\n" svg-parts)
      (push (format "      .text { font-family: '%s', monospace; font-size: %dpx; fill: %s; white-space: pre; letter-spacing: 0; }\n"
                      font-family org-headline-card-export-font-size fg-color)
            svg-parts)
      (push "      .text { font-kerning: none; font-variant-ligatures: none; }\n" svg-parts)
      (push "      .bold { font-weight: bold; }\n" svg-parts)
      (push "      .italic { font-style: italic; }\n" svg-parts)
      (push "      .underline { text-decoration: underline; }\n" svg-parts)
      (push (format "      .code { font-family: '%s', monospace; }\n" font-family)
            svg-parts)
      (push "      .highlight { filter: url(#highlightFilter); }\n" svg-parts)
      ;; Glow/stroke effects are rendered explicitly (not via CSS class).
      (push "      .glow { }\n" svg-parts)
      (push "      .stroke { }\n" svg-parts)
      (push "    </style>\n" svg-parts)

      ;; Highlight filter (yellow background behind text)
      (push "    <filter id=\"highlightFilter\" x=\"-5%\" y=\"-20%\" width=\"110%\" height=\"140%\">
      <feFlood flood-color=\"#FFFF00\" flood-opacity=\"0.5\" result=\"bg\"/>
      <feMerge>
        <feMergeNode in=\"bg\"/>
        <feMergeNode in=\"SourceGraphic\"/>
      </feMerge>
    </filter>\n" svg-parts)
      
      ;; Glow filters (per color for better contrast control)
      (dolist (color (delete-dups (nreverse glow-colors)))
        (let* ((rgba (org-headline-card--split-rgba color))
               (rgb (or (nth 0 rgba) "#00FFFF"))
               (op (nth 1 rgba))
               (id (org-headline-card--svg-id-safe (org-headline-card--normalize-color-rgba color))))
          (push (format "    <filter id=\"glow-%s\" x=\"-50%%\" y=\"-50%%\" width=\"200%%\" height=\"200%%\">
      <feGaussianBlur in=\"SourceGraphic\" stdDeviation=\"%d\" result=\"blur\"/>
      <feFlood flood-color=\"%s\" flood-opacity=\"%.3f\" result=\"glowColor\"/>
      <feComposite in=\"glowColor\" in2=\"blur\" operator=\"in\" result=\"coloredBlur\"/>
      <feMerge>
        <feMergeNode in=\"coloredBlur\"/>
        <feMergeNode in=\"coloredBlur\"/>
        <feMergeNode in=\"SourceGraphic\"/>
      </feMerge>
    </filter>\n"
                        id org-headline-card-export-glow-blur rgb op)
                svg-parts)))
      
      ;; Drop shadow filter (using compatible SVG 1.1 syntax)
      (when shadow-enabled
        (push (format "    <filter id=\"dropShadow\" x=\"-50%%\" y=\"-50%%\" width=\"200%%\" height=\"200%%\">
      <feGaussianBlur in=\"SourceAlpha\" stdDeviation=\"%d\" result=\"blur\"/>
      <feOffset in=\"blur\" dx=\"%d\" dy=\"%d\" result=\"offsetBlur\"/>
      <feFlood flood-color=\"%s\" flood-opacity=\"%s\" result=\"color\"/>
      <feComposite in=\"color\" in2=\"offsetBlur\" operator=\"in\" result=\"shadow\"/>
      <feMerge>
        <feMergeNode in=\"shadow\"/>
        <feMergeNode in=\"SourceGraphic\"/>
      </feMerge>
    </filter>\n"
                      shadow-blur shadow-offset-x shadow-offset-y shadow-color shadow-opacity)
              svg-parts))
      
      (when bg-gradient
        (setq bg-gradient (replace-regexp-in-string "[\n\r]+" " " bg-gradient))
        (if (eq bg-gradient-type 'radial)
            ;; Radial gradient
            (push (format "    <radialGradient id=\"bgGradient\" cx=\"50%%\" cy=\"50%%\" r=\"70%%\">
%s
    </radialGradient>\n" 
                          bg-gradient)
                  svg-parts)
          ;; Linear gradient (default)
          (push (format "    <linearGradient id=\"bgGradient\" x1=\"0%%\" y1=\"0%%\" x2=\"100%%\" y2=\"100%%\">
%s
    </linearGradient>\n" 
                        bg-gradient)
                svg-parts)))
      
      ;; Background image pattern
      (when bg-image
        (let ((base64-image (org-headline-card--image-to-base64 bg-image)))
          (when base64-image
            (push (format "    <pattern id=\"bgImagePattern\" patternUnits=\"userSpaceOnUse\" width=\"%d\" height=\"%d\">
      <image href=\"%s\" width=\"%d\" height=\"%d\" preserveAspectRatio=\"xMidYMid slice\" opacity=\"%s\"/>
    </pattern>\n"
                          (+ content-width padding-px) (+ content-height padding-px)
                          base64-image
                          (+ content-width padding-px) (+ content-height padding-px)
                          bg-image-opacity)
                  svg-parts))))
      
      ;; Built-in background pattern
      (when bg-pattern
        (let ((pattern-svg (org-headline-card--generate-pattern-svg 
                            bg-pattern bg-pattern-color bg-pattern-opacity)))
          (when pattern-svg
            (push pattern-svg svg-parts))))
      
      (push "  </defs>\n" svg-parts)
      
      ;; Background
      (let ((fill-attr (cond
                        (bg-image "url(#bgImagePattern)")
                        (bg-gradient "url(#bgGradient)")
                        (t bg-color)))
            (filter-attr (if shadow-enabled " filter=\"url(#dropShadow)\"" ""))
            (rect-x (/ padding-px 2))
            (rect-y (/ padding-px 2))
            (rect-w (+ content-width padding-px))
            (rect-h (+ content-height padding-px)))
        ;; Main background rect
        (push (format "  <rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\" stroke=\"%s\" stroke-width=\"%s\" rx=\"%s\" ry=\"%s\"%s/>\n"
                      rect-x rect-y rect-w rect-h
                      fill-attr border-color border-stroke-width corner-radius corner-radius
                      filter-attr)
              svg-parts)
        ;; Pattern overlay (if pattern is used with solid background)
        (when (and bg-pattern (not bg-image) (not bg-gradient))
          (push (format "  <rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"url(#bgPattern)\" rx=\"%s\" ry=\"%s\"/>\n"
                        rect-x rect-y rect-w rect-h corner-radius corner-radius)
                svg-parts)))

      ;; Render lines
      (let ((y-acc (+ (/ padding-px 2) 5))) ;; Small initial offset
        (dolist (p-line processed-lines)
          (let* ((line (nth 0 p-line))
                 (l-height (nth 1 p-line))
                 (l-spacing (nth 2 p-line))
                 (line-char-width (* l-height 0.65)))
            (if (get-text-property 0 'org-headline-card-separator line)
                (push (format "  <line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"1\" opacity=\"0.5\"/>\n"
                              padding-px (+ y-acc (/ l-spacing 2))
                              (+ content-width padding-px) (+ y-acc (/ l-spacing 2))
                              border-color)
                      svg-parts)
              (let* ((highlight-ranges (org-headline-card--line-highlight-ranges line)))
                ;; Highlight background
                (dolist (range highlight-ranges)
                  (let* ((start (nth 0 range))
                         (end (nth 1 range))
                         (color (nth 2 range)))
                    (if (eq org-headline-card-export-highlight-mode 'per-char)
                        (dolist (rect (org-headline-card--line-highlight-char-boxes
                                       line start end line-char-width padding-px y-acc l-height color))
                          (push rect svg-parts))
                      (let* ((prefix (substring line 0 start))
                             (segment (substring line start end))
                             (prefix-w (and (eq org-headline-card-export-highlight-mode 'measure)
                                            (org-headline-card--measure-string-px
                                             prefix font-family l-height)))
                             (segment-w (and (eq org-headline-card-export-highlight-mode 'measure)
                                             (org-headline-card--measure-string-px
                                              segment font-family l-height))))
                        (if (and prefix-w segment-w)
                            (let* ((rect-x (+ padding-px (round prefix-w)))
                                   (rect-y (round y-acc))
                                   (rect-w (max 1 (round segment-w)))
                                   (rect-h (round l-height))
                                   (rgba (org-headline-card--split-rgba color))
                                   (fill (or (nth 0 rgba) "#FFFF00"))
                                   (op (* 0.4 (nth 1 rgba))))
                              (push (format "  <rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\" opacity=\"%.3f\"/>\n"
                                            rect-x rect-y rect-w rect-h fill op)
                                    svg-parts))
                          (let* ((start-cols (string-width (substring line 0 start)))
                                 (end-cols (string-width (substring line 0 end)))
                                 (base-x (+ padding-px (round (* start-cols line-char-width))))
                                 (base-w (max 1 (round (* (- end-cols start-cols) line-char-width))))
                                 (pad-x (max 1 (round (* l-height 0.06))))
                                 (pad-y (max 1 (round (* l-height 0.12))))
                                 (rect-x (max 0 (- base-x pad-x)))
                                 (rect-y (max 0 (- (round y-acc) pad-y)))
                                 (rect-w (+ base-w (* 2 pad-x)))
                                 (rect-h (+ (round l-height) (* 2 pad-y)))
                                 (rgba (org-headline-card--split-rgba color))
                                 (fill (or (nth 0 rgba) "#FFFF00"))
                                 (op (* 0.4 (nth 1 rgba))))
                            (push (format "  <rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\" opacity=\"%.3f\"/>\n"
                                          rect-x rect-y rect-w rect-h fill op)
                                  svg-parts)))))))

                ;; Debug overlays
                (when org-headline-card-export-debug-highlight
                  (dolist (range highlight-ranges)
                    (let* ((start (nth 0 range))
                           (end (nth 1 range))
                           (color (nth 2 range)))
                      (dolist (rect (org-headline-card--line-highlight-char-boxes
                                     line start end line-char-width padding-px y-acc l-height color t))
                        (push rect svg-parts)))))

                ;; Glow segments
                (let ((glow-segments (org-headline-card--line-effect-segments line 'glow)))
                  (dolist (seg glow-segments)
                    (let* ((start (nth 0 seg))
                           (end (nth 1 seg))
                           (prefix (substring line 0 start))
                           (chunk (substring line start end))
                           (prefix-w (or (org-headline-card--measure-string-px
                                          prefix font-family l-height)
                                         (* (string-width prefix) line-char-width)))
                           (x (+ padding-px (round prefix-w)))
                           (y (+ y-acc l-height))
                           (color (or (get-text-property start 'org-headline-card-glow-color line)
                                      org-headline-card-effect-glow-color))
                           (rgba (org-headline-card--split-rgba color))
                           (fid (org-headline-card--svg-id-safe (org-headline-card--normalize-color-rgba color))))
                      (push (format "  <text x=\"%d\" y=\"%d\" class=\"text\" filter=\"url(#glow-%s)\" xml:space=\"preserve\">%s</text>\n"
                                    x y fid (org-headline-card--text-to-svg-tspans chunk))
                            svg-parts))))

                ;; Wave underline segments
                (let ((wave-segments (org-headline-card--line-effect-segments line 'wave)))
                  (dolist (seg wave-segments)
                    (let* ((start (nth 0 seg))
                           (end (nth 1 seg))
                           (prefix (substring line 0 start))
                           (chunk (substring line start end))
                           (prefix-w (or (org-headline-card--measure-string-px
                                          prefix font-family l-height)
                                         (* (string-width prefix) line-char-width)))
                           (chunk-w (or (org-headline-card--measure-string-px
                                         chunk font-family l-height)
                                        (* (string-width chunk) line-char-width)))
                           (x (+ padding-px (round prefix-w)))
                           (baseline (+ y-acc l-height))
                           (y (+ baseline (max 2 (round (* l-height 0.12)))))
                           (color (or (get-text-property start 'org-headline-card-wave-color line)
                                      org-headline-card-effect-wave-color))
                           (rgba (org-headline-card--split-rgba color))
                           (stroke (or (nth 0 rgba) "#FF0000"))
                           (stroke-op (nth 1 rgba))
                           (width (or (get-text-property start 'org-headline-card-wave-width line)
                                      org-headline-card-effect-wave-width))
                           (d (org-headline-card--wave-path-d
                               x y (max 1 (round chunk-w))
                               (max 1 org-headline-card-export-wave-amplitude)
                               (max 4 org-headline-card-export-wave-period))))
                      (push (format "  <path d=\"%s\" fill=\"none\" stroke=\"%s\" stroke-opacity=\"%.3f\" stroke-width=\"%d\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/>\n"
                                    d stroke stroke-op width)
                            svg-parts))))

                ;; Box outline segments (formerly stroke)
                (let ((box-segments (org-headline-card--line-effect-segments line 'stroke)))
                  (dolist (seg box-segments)
                    (let* ((start (nth 0 seg))
                           (end (nth 1 seg))
                           (prefix (substring line 0 start))
                           (chunk (substring line start end))
                           (prefix-w (or (org-headline-card--measure-string-px
                                          prefix font-family l-height)
                                         (* (string-width prefix) line-char-width)))
                           (chunk-w (or (org-headline-card--measure-string-px
                                         chunk font-family l-height)
                                        (* (string-width chunk) line-char-width)))
                           (x (+ padding-px (round prefix-w)))
                           (pad-x (max 2 (round (* l-height 0.15))))
                           (pad-y (max 2 (round (* l-height 0.18))))
                           (rect-x (max 0 (- x pad-x)))
                           (rect-y (max 0 (- (round y-acc) pad-y)))
                           (rect-w (+ (max 1 (round chunk-w)) (* 2 pad-x)))
                           (rect-h (+ (round l-height) (* 2 pad-y)))
                           (color (or (get-text-property start 'org-headline-card-box-color line)
                                      org-headline-card-effect-box-color))
                           (rgba (org-headline-card--split-rgba color))
                           (stroke (or (nth 0 rgba) "#000000"))
                           (stroke-op (nth 1 rgba))
                           (width (or (get-text-property start 'org-headline-card-box-width line)
                                      org-headline-card-effect-box-width)))
                      (push (format "  <rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"%s\" stroke-opacity=\"%.3f\" stroke-width=\"%d\"/>\n"
                                    rect-x rect-y rect-w rect-h stroke stroke-op width)
                            svg-parts))))

                ;; Normal text (top layer)
                (push (format "  <text x=\"%d\" y=\"%d\" class=\"text\" xml:space=\"preserve\">%s</text>\n"
                              padding-px (+ y-acc l-height)
                              (org-headline-card--line-to-svg-tspans line))
                      svg-parts)))
            (setq y-acc (+ y-acc l-spacing))))))

      (push "</svg>\n" svg-parts)
      (apply 'concat (nreverse svg-parts))))

;; ImageMagick Integration
(defun org-headline-card--check-imagemagick ()
  "Check if ImageMagick is installed and return the command name."
  (cond
   ((executable-find "magick") "magick")
   ((executable-find "convert") "convert")
   (t nil)))

(defun org-headline-card--export-via-imagemagick (svg-file output-file)
  "Convert SVG-FILE to OUTPUT-FILE using ImageMagick."
  (let ((cmd (org-headline-card--check-imagemagick))
        (density (number-to-string org-headline-card-export-dpi)))
    (unless cmd
      (error "ImageMagick not found. Please install ImageMagick (convert or magick command)"))

    ;; Use -density for input resolution and white background for compatibility.
    ;; Use an async process + accept-process-output loop so Emacs doesn't appear frozen.
    (let* ((args (if (string-equal cmd "magick")
                     (list "convert" "-density" density
                           "-background" "white" "-flatten"
                           svg-file output-file)
                   (list "-density" density
                         "-background" "white" "-flatten"
                         svg-file output-file)))
           (buf (get-buffer-create " *org-headline-card-export*"))
           (proc (make-process :name "org-headline-card-export"
                               :buffer buf
                               :command (cons cmd args)
                               :noquery t))
           (deadline (and org-headline-card-export-convert-timeout
                          (+ (float-time) org-headline-card-export-convert-timeout))))
      (while (process-live-p proc)
        (when (and deadline (> (float-time) deadline))
          (delete-process proc)
          (error "ImageMagick conversion timed out after %ss" org-headline-card-export-convert-timeout))
        (accept-process-output proc 0.1))
      (let ((status (process-exit-status proc)))
        (unless (zerop status)
          (with-current-buffer buf
            (error "ImageMagick failed (exit %d): %s" status (string-trim (buffer-string))))))
      t)))

(defun org-headline-card--text-to-image (text output-file theme-style)
  "Convert decorated TEXT to image file OUTPUT-FILE using THEME-STYLE.
Generates an intermediate SVG file and converts it to PNG via ImageMagick."
  (let ((svg-file (make-temp-file "org-headline-card-" nil ".svg")))
    (unwind-protect
        (progn
          (org-headline-card--text-to-svg text svg-file theme-style)
          (org-headline-card--export-via-imagemagick svg-file output-file))
      ;; Cleanup temp file unless debugging
      (if org-headline-card-export-keep-svg
          (message "SVG kept at: %s" svg-file)
        (when (file-exists-p svg-file)
          (delete-file svg-file))))))

(provide 'org-headline-card-export)
;;; org-headline-card-export.el ends here
