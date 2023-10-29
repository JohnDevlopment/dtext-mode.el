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
;; Danbooru. Automaticallu detects .dtext files.

;;; Code:

(eval-when-compile
  (require 'bbcode-mode)
  (require 'cl-lib))

;; Constants

;; Keys that insert most tags are prefixed with 'C-c C-t'.
;; Keys for tables begin with 'C-c C-b'
(eval-and-compile
  (defconst dtext-tags
    '(("b"           bold                          "C-c C-t b" 1)
      ("code"        font-lock-function-name-face  "C-c C-t c" t)
      ("i"           italic                        "C-c C-t i" 1)
      ("img"         link                          "C-c C-t m" 1 width height)
      ("quote"       nil                           "C-c C-t q" t name)
      ("s"           nil                           "C-c C-t s" 1)
      ("table"       nil                           "C-c C-b t" t)
      ("td"          font-lock-variable-name-face  "C-c C-b d" 1)
      ("th"          bold                          "C-c C-b h" 1)
      ("tr"          nil                           "C-c C-b r" 1)
      ("u"           underline                     "C-c C-t u" 1))))

(eval-and-compile
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
    "Regular expressions to match DText markup."))

;;;###autoload
(define-derived-mode dtext-mode text-mode "DText"
  "Major mode for writing Danbooru's DText markup.

This mode map indirectly depends on `bbcode-mode', so make sure that is
installed before this one. Some of the bbcode-insert-tag-* commands are binded
to the same keys as they would be in `bbcode-mode'.

\\{dtext-mode-map}"
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
	    (let ((function-name (intern (format "bbcode-insert-tag-%s" tag))))
	      `((define-key dtext-mode-map (kbd ',key) ',function-name)))))
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
