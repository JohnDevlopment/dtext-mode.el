;;; dtext-mode.el --- Major mode for Danbooru DText

;; Copyright (C) 2023 John Russell
;; Author:           John Russell <johndevlopment7@gmail.com>
;; URL:              https://github.com/JohnDevlopment/dtext-mode.el
;; Keywords:         languages
;; Package-Version:  0.1
;; Package-Requires: ((emacs "24.4") (bbcode-mode "20190304.2122"))

;; This file is NOT part of Emacs

;;; Commentary:
;; A major mode for editing DText files. DText is the editing language for
;; Danbooru.

;;; Code:

(eval-when-compile
  (require 'bbcode-mode)
  (require 'cl-lib))

;; Keys that insert most tags are prefixed with 'C-c C-t'.
;; Keys related to modifying font properties begin with 'C-c C-f'.
;; Keys for creating lists begin with 'C-c C-l'.
;; Keys for tables begin with 'C-c C-b'
;; Keys for special, uncommon tags begin with 'C-c C-s'.

(defconst dtext-tags
  '(("*"           nil                           "C-c C-l *" nil)
    ("attachment"  font-lock-variable-name-face  "C-c C-s a" 1)
    ("b"           bold                          "C-c C-t b" 1)
    ("center"      nil                           "C-c C-t n" 1)
    ("code"        font-lock-function-name-face  "C-c C-t c" t)
    ("color"       font-lock-variable-name-face  "C-c C-f c" 1 color)
    ("del"         nil                           "C-c C-t d" 1)
    ("email"       link                          "C-c C-t e" 1)
    ("font"        font-lock-variable-name-face  "C-c C-f f" 1)
    ("gvideo"      font-lock-variable-name-face  "C-c C-s g" 1)
    ("i"           italic                        "C-c C-t i" 1)
    ("img"         link                          "C-c C-t m" 1 width height)
    ("li"          font-lock-keyword-face        "C-c C-l i" 1)
    ("list"        nil                           "C-c C-l l" t)
    ("manual"      font-lock-variable-name-face  "C-c C-s m" 1)
    ("ol"          nil                           "C-c C-l o" t)
    ("quote"       nil                           "C-c C-t q" t name)
    ("s"           nil                           "C-c C-t s" 1)
    ("size"        font-lock-variable-name-face  "C-c C-f s" 1 size)
    ("style"       nil                           "C-C C-f y" 1 color size)
    ("table"       nil                           "C-c C-b t" t)
    ("td"          font-lock-variable-name-face  "C-c C-b d" 1)
    ("th"          bold                          "C-c C-b h" 1)
    ("tr"          nil                           "C-c C-b r" 1)
    ("u"           underline                     "C-c C-t u" 1)
    ("ul"          nil                           "C-c C-l u" t)
    ("url"         link                          "C-c C-t l" 1 url)
    ("wiki"        link                          "C-c C-s w" 1)
    ("youtube"     link                          "C-c C-s y" 1)))

(defconst dtext-font-lock-keywords
  `(;; Opening tag
    (,(concat (regexp-quote "[")
	      (regexp-opt (mapcar #'car dtext-tags) t)
	      "]")
     (0 'font-lock-keyword-face))
    ;; Opening tag with attributes
    ;; (,(concat (regexp-quote "[")
    ;; 	      (regexp-opt (mapcar #'car dtext-tags) t)
    ;; 	      "[ =]]\\(.*?\\)"
    ;; 	      "]")
    ;;  (0 'font-lick-keyword-face)
    ;;  (2 'font-lock-preprocessor-face t))
    ;; Closing tag
    (,(concat (regexp-quote "[/")
	      (regexp-opt (mapcar #'car dtext-tags) t)
	      "]")
     (0 'font-lock-keyword-face))
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
  "Regular expressions to match DText markup.")

;;;###autoload
(define-derived-mode dtext-mode text-mode "DText"
  "Major mode for writing Danbooru's DText markup.

\\{dtext-mode-map}"
  (set 'font-lock-multiline t)
  (set 'font-lock-defaults
       '(dtext-font-lock-keywords nil t))
  (auto-fill-mode 0)
  (visual-line-mode 1))

(add-to-list 'auto-mode-alist '("\\.dtext$" . dtext-mode))

(provide 'dtext-mode)

;;; dtext-mode.el ends here
