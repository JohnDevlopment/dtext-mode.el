;;; dtext-mode.el --- Major mode for Danbooru DText

;; Copyright (C) 2023 John Russell
;; Author:           John Russell <johndevlopment7@gmail.com>
;; URL:              https://github.com/JohnDevlopment/dtext-mode.el
;; Keywords:         languages
;; Package-Version:  0.1
;; Package-Requires: ((emacs "24.4") (bbcode-mode "20190304.2122"))

;; This file is NOT part of Emacs

;; This program is free software: you can redistribute it and/or modify
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
;; A major mode for editing DText files. DText is the editing language for
;; Danbooru. Automaticallu detects .dtext files.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'bbcode-mode)

;;; Font Lock ==================================================================

(require 'font-lock)

(eval-and-compile
  (defgroup dtext nil
    "Major mode for editing DText files."
    :prefix "dtext-"
    :group 'text
    :link '(url-link "https://github.com/JohnDevlopment/dtext-mode.el"))

  (defgroup dtext-faces nil
    "Faces used in DText Mode."
    :group 'dtext
    :group 'faces)

  (defface dtext-heading-face
    '((t (:inherit bold)))
    "Face for headings."
    :group 'dtext-faces)

  (defface dtext-strikethrough-face
    '((t (:strike-through t)))
    "Face for strikethrough text."
    :group 'dtext-faces)

  (defface dtext-small-face
    '((t (:inherit shadow :scale 0.8)))
    "Face for small text."
    :group 'dtext-faces)

  (defface dtext-link-face
    '((t (:inherit link)))
    "Face for regular links."
    :group 'dtext-faces)

  (defface dtext-keyword-face
    '((t (:inherit font-lock-keyword-face)))
    "Face for keywords."
    :group 'dtext-faces)

  (defface dtext-variable-face
    '((t (:inherit font-lock-variable-name-face)))
    "Face for variables."
    :group 'dtext-faces)

  (defface dtext-link-text-face
    '((t (:inherit italic)))
    "Face for the description in links."
    :group 'dtext-faces)

  (defface dtext-link-face
    '((t (:inherit link)))
    "Face for wiki links."
    :group 'dtext-faces)

  (defface dtext-code-face
    '((t (:inherit font-lock-function-name-face)))
    "Face for the text between code tags."
    :group 'dtext-faces)

  (defconst dtext-heading-regexp
    "^h[1-6]\\.[[:blank:]]*?.+"
    "The regular expression for headings without IDs.")

  ;; Link regular expressions
  (defconst dtext-link-markdown-regexp
    "\\[\\(.+?\\)](\\([#/]?.*?\\))"
    "The regular expression used for Markdown-style links.
Group 1 matches the text.
Group 2 matches the URL.")

  (defconst dtext-link-url-regexp
    "https?://\\(?:www\\.\\)?[%./A-Z_a-z-]*"
    "The regular expression used for bare links.
Groups 1 and 2 are nil.")

  (defconst dtext-link-regexp
    "\"\\(.+?\\)\":\\[\\([#/]?.+?\\)\\]"
    "The regular expression used for DText-style links \"text\":[url].
Group 1 matches the text.
Group 2 matches the URL.")

  (defconst dtext-link-wiki-regexp
    "\\[\\[\\(.+?\\)\\(?:|\\(.*?\\)\\)?]]"
    "The regular expression used for wiki links [[url]], [[url|text]], or [[url|]].
Group 1 matches the URL.
Group 2 matches the text (optional).")

  (defconst dtext-link-search-regexp
    "{{\\([ 0-9A-Z_a-z]+?\\)}}"
    "The regular expression used for search links.
Group 1 matches the \"URL\".")

  ;;---

  (defconst dtext-topic-link-regexp
    "topic #[1-9][0-9]*\\(?:/[1-9]\\(?:0-9\\)*\\)?"
    "The regular expression for topic links.")

  ;; Keys that insert most tags are prefixed with 'C-c C-t'.
  ;; Keys for DText-specific tags begin with 'C-c C-d'
  ;; Keys for tables begin with 'C-c C-b'
  (defconst dtext-tags
    '(("b"        bold                     "C-c C-t b" 1)
      ("code"     dtext-code-face          "C-c C-t c" t)
      ("i"        italic                   "C-c C-t i" 1)
      ("quote"    nil                      "C-c C-t q" 1)
      ("s"        dtext-strikethrough-face "C-c C-t s" 1)
      ("spoilers" nil                      "C-c C-d s" 1)
      ("table"    nil                      "C-c C-b t" 1)
      ("tbody"    nil                      nil         1)
      ("td"       dtext-variable-face      "C-c C-b d" 1)
      ("th"       bold                     "C-c C-b h" 1)
      ("tn"       dtext-small-face         nil         1)
      ("tr"       nil                      "C-c C-b r" 1)
      ("u"        underline                "C-c C-t u" 1)))

  (defconst dtext-post-links
    (list
     "post"        "forum"
     "comment"     "pool"
     "favgroup"    "wiki"
     "user"        "ban"
     "feeback"     "appeal"
     "flag"        "note"
     "BUR"         "alias"
     "implication" "mod action"
     "artist"      "issue"
     "pixiv"       "pawoo"
     "seiga"       "nijie"
     "twitter"     "deviantart"
     "artstation"  "sankaku"
     "gelbooru"    "yandere")
    "A list of post links (e.g., post #1)")

  (defconst dtext-font-lock-keywords
    `(;; Opening tag
      (,(concat (regexp-quote "[")
		(regexp-opt (mapcar #'car dtext-tags) t)
		"]")
       (0 'dtext-keyword-face))
      ;; Opening tag with attributes
      (,(concat (regexp-quote "[")
	      (regexp-opt (mapcar #'car dtext-tags) t)
	      "[ =]]\\(.*?\\)"
	      "]")
       (0 'dtext-keyword-face)
       (2 'font-lock-preprocessor-face t))
      ;; Links
      (dtext-fontify-wiki-links)
      (dtext-fontify-links)
      (dtext-fontify-markdown-links)
      (dtext-fontify-search-links)
      (dtext-fontify-url-links)
      ;; Post links
      (,(concat (regexp-opt dtext-post-links t)
		(regexp-quote " #")
		"[0-9]+")
       (0 'dtext-link-face))
      ;; Topic post links
      (,dtext-topic-link-regexp
       (0 'dtext-link-face t))
      ;; Headings
      (,dtext-heading-regexp
       (0 'dtext-heading-face))
      ;; Closing tag
      (,(concat (regexp-quote "[/")
		(regexp-opt (mapcar #'car dtext-tags) t)
		"]")
       (0 'dtext-keyword-face))
      ;; Highlight the body of some tags with a tag-specific face
      ,@(let (patterns (face->tags (make-hash-table)))
	  ;; For each TAG-SPEC in DTEXT-TAGS...
	  (dolist (tag-spec dtext-tags)
	    ;; TAG = first element
	    ;; FACE = second element
	    (let* ((tag (nth 0 tag-spec))
		   (face (nth 1 tag-spec)))
	      ;; FACE->TAGS[FACE] = (TAG . FACE->TAGS[FACE])
	      (puthash face (cons tag (gethash face face->tags)) face->tags)))
	  (maphash (lambda (face tags)
		     (when face
		       (push `(,(concat (regexp-quote "[")   ;; [(b|th)](\[^\]\[\]+)[
					(regexp-opt tags t)  ;;
					"]"
					"\\([^][]+\\)"
					(regexp-quote "["))
			       (2 ',face t))
			     patterns)))
		   face->tags)
	  patterns))
    "Regular expressions to match DText markup."))

(defun dtext--range-property-any (begin end prop prop-values)
  "Return t if PROP from BEGIN to END is equal to one of the given PROP-VALUES.
Also returns t if PROP is a list containing one of the PROP-VALUES.
Return nil otherwise."
  (let (props)
    (catch 'found
      (dolist (loc (number-sequence begin end))
        (when (setq props (get-text-property loc prop))
          (cond ((listp props)
                 ;; Props is a list, check for membership
                 (dolist (val prop-values)
                   (when (memq val props) (throw 'found loc))))
                (t
                 ;; Props is a scalar, check for equality
                 (dolist (val prop-values)
                   (when (eq val props) (throw 'found loc))))))))))

(defun dtext--string-ends-with (pattern str)
  "Returns t if PATTERN is found at the end of STR."
  (let ((pattern (concat pattern "$")))
    (if (string-match pattern str) t)))

(defun dtext--string-contains (pattern str)
  "Returns t if PATTERN is found in STR."
  (if (string-match pattern str) t))

;; (defun dtext--num-chars (char str)
;;   (let ((sep (char-to-string char)))
;;     (1- (length (split-string str sep)))))

(defun dtext--match-links (last &optional markdown wiki search url-link)
  "Match links with markup between point and LAST.
TYPE indicates what kind of link it is.
Group 1 corresponds to the text part, if any, of the link.
Group 2 corresponds to the URL part."
  (let* ((pattern (cond
		   (markdown dtext-link-markdown-regexp)
		   (wiki dtext-link-wiki-regexp)
		   (search dtext-link-search-regexp)
		   (url-link dtext-link-url-regexp)
		   (t dtext-link-regexp)))
	 (prohibited-faces '(dtext-code-face dtext-link-face))
	 beg end
	 text-beg text-end
	 url-beg url-end
	 found)
    (while
	(and (not found)
	     (< (point) last)
	     (progn
	       ;; Clear match data so we can test it
	       (set-match-data nil)
	       ;; Preliminary search. Continue search if it passes
	       (re-search-forward pattern last 'limit)))
      ;; Continue searching if this is part of a code block
      (if (dtext--range-property-any (match-beginning 0)
				     (match-end 0)
				     'face prohibited-faces)
	  (set-match-data nil)
	(setq found t)))
    (when found
      (cond
       (url-link
	;; Plain URLs have no text, so set those to nil
	(setq beg (match-beginning 0)
	      end (match-end 0)
	      text-beg nil
	      text-end nil
	      url-beg beg
	      url-end end))
       (wiki
	;; Wiki links: [[id]], [[id|]], or [[id|text]]
	(setq beg (match-beginning 0)
	      end (match-end 0)
	      text-beg (match-beginning 2)
	      text-end (match-end 2)
	      url-beg (match-beginning 1)
	      url-end (match-end 1)
	      ))
       (search
	;; Search links
	;; This has some additional checks to it. Firstly,
	;; the matched string cannot end with an
	;; underscore. Second, there cannot be two or more
	;; consecutive underscores. Third, the string cannot
	;; contain more than 170 characters overall. The
	;; other rules are handled by the regular expression.
	(let ((substr (buffer-substring (match-beginning 1)
					(match-end 1))))
	  (if (not (or (dtext--string-ends-with "_" substr)
		       (dtext--string-contains "__+" substr)
		       (> (length substr) 170)))
	      (setq beg (match-beginning 0)
		    end (match-end 0)
		    text-beg nil
		    text-end nil
		    url-beg (match-beginning 1)
		    url-end (match-end 1))
	    (setq found nil))))
       (t
	;; DText and Markdown-style links
	(setq beg (match-beginning 0)
	      end (match-end 0)
	      text-beg (match-beginning 1)
	      text-end (match-end 1)
	      url-beg (match-beginning 2)
	      url-end (match-end 2))))
      (set-match-data
       (list
	beg end
	text-beg text-end
	url-beg url-end)))
    found))

(defmacro write-fontify-function (name doc args)
  "Generates a function called dtext-fontify-NAME.
DOC is the function's docstring, and ARGS is a list of arguments passed to
`dtext--match-links' following LAST."
  (declare (indent 2))
  (let* ((name (symbol-name name))
	 (function-name (intern
			 (concat "dtext-fontify-"
				 name))))
    `(progn
       (defun ,function-name (last) ,doc
	      (when (dtext--match-links last ,@args)
		(let* ((text-beg (match-beginning 1))
		       (text-end (match-end 1))
		       (url-beg (match-beginning 2))
		       (url-end (match-end 2)))
		  (when text-beg
		    (add-face-text-property text-beg text-end 'dtext-link-text-face))
		  (when url-beg
		    (add-face-text-property url-beg url-end 'dtext-link-face))
		  t))))))

(write-fontify-function markdown-links
    "Fontify Markdown-style links from point to LAST." (t))

(write-fontify-function wiki-links
    "Fontify wiki links from point to LAST." (nil t))

(write-fontify-function search-links
    "Fontify search links from point to LAST." (nil nil t))

(write-fontify-function url-links
    "Fontify plain URLs from point to LAST." (nil nil nil t))

(write-fontify-function links
    "Fontify links from point to LAST." ())

;;;###autoload
(define-derived-mode dtext-mode text-mode "DText"
  "Major mode for writing Danbooru's DText markup.

This mode map indirectly depends on `bbcode-mode', so make sure that is
installed before this one. Some of the bbcode-insert-tag-* commands are binded
to the same keys as they would be in `bbcode-mode'.

\\{dtext-mode-map}"
  :group 'dtext
  (set 'font-lock-multiline t)
  (set 'font-lock-defaults
       '(dtext-font-lock-keywords nil t))
  (auto-fill-mode 0)
  (visual-line-mode 1))

(defmacro dtext-bind-bbcode-insert-tag-commands ()
  "Bind `bbcode-mode''s insert-tag-* commands with their respective key bindings."
  (declare (indent 2))
  `(progn
     ,@(cl-mapcan
	(lambda (tag-spec)
	  (cl-destructuring-bind (tag _face key _body . _attrs) tag-spec
	    (when key
	      (let ((function-name (intern (format "bbcode-insert-tag-%s" tag))))
		`((define-key dtext-mode-map (kbd ',key) ',function-name))))))
	dtext-tags)))

(dtext-bind-bbcode-insert-tag-commands)

;;;###autoload
(defun dtext-scratch ()
  "Open *dtext-scratch* buffer to quickly edit DText posts."
  (interactive)
  (switch-to-buffer (get-buffer-create "*dtext-scratch*"))
  (unless (equal 'dtext-mode major-mode)
    (dtext-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dtext$" . dtext-mode))

(provide 'dtext-mode)

;;; dtext-mode.el ends here
