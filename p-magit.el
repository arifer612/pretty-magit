;;; -*- lexical-binding: t; -*-

;;Requires: (cl-lib dash)

;;; Code:

(require 'cl-lib)
(require 'dash)

(defvar p-magit-headers '(type scope subject)
  "Names for groups of text in a git-message.

  The default git-message format is:
<type>(<scope>): <subject>
  --BLANK LINE--
  <body>
  --BLANK LINE--
  <footer>")

(defvar p-magit-alist '()
  "List containing lists of magit replacement nouns.")

(defvar p-magit-header-regexp
  '(bol
    (zero-or-more (not (any " ")))  ; git-ref - compulsory
    " "
    (optional (zero-or-more (not (any " ")))  ; HEAD - optional
              " ")
    (zero-or-more (not (any "("  ; type - compulsory
                            ":"
                            " ")))
    (optional "("  ; scope - optional
              (zero-or-more (not (any ")"
                                      ":"
                                      " ")))
              ")")
    ": "
    (zero-or-more not-newline)  ; subject - compulsory
    eol)
  "sequence of regexp in a magit header"
  )

(defun p-magit--ensure-target (target)
  "Ensure TARGET exists in p-magit-headers."
  (cond ((member target p-magit-headers)
         target)
        (t
         (progn
           (message (concat (symbol-name target)
                            " is not a valid p-magit-header. "
                            "Defaulting to 'type."))
           'type))))

(defun p-magit--test-ensure-target ()
  "TODO Test function for p-magit-ensure-target."
  (let ((p-magit-headers '(t-target))
        (a-target 't-target)
        (b-target 's-target))
    (cond (`,(p-magit--ensure-target ,a-target)  ; Should fail
           (raise-error)))
    (cond (`,(p-magit--ensure-target ',a-target) ; Should pass
           t))
    (cond (`,(p-magit--ensure-target ',b-target) ; Should fail
           (raise-error)))))

(defun p-magit--match-headers (target)
  "Match TARGET to regexp list element number.

The first nth value is 0."
  (cond ((equal target 'type)
         4)
        ((equal target 'scope)
         5)
        ((equal target 'subject)
         7)))

(defun p-magit--type-regexp (word)
  "String regexp for target 'type with a single WORD."
  (list
   `(group ,word)))

(defun p-magit--scope-regexp (word)
  "String regexp for target 'scope with a single WORD."
  (list
   "("
   '(zero-or-more (not (any ")"
                            ":"
                            " ")))
   `(group ,word)
   '(zero-or-more (not (any ")"
                            ":"
                            " ")))
   ")"))

(defun p-magit--target-regexp (word target)
  "Return sanitised WORD regexp for TARGET replacement in
p-magit-header-regexp."
  (cond ((equal target 'type)
         (p-magit--type-regexp word))
        ((equal target 'scope)
         (p-magit--scope-regexp word))))

(defun p-magit--build-regexp-list (word target)
  "Build new regexp list with WORD by changing out TARGET entry in
p-magit-header-regexp."
  (let ((col (p-magit--match-headers target))
        (r-list (p-magit--target-regexp word target))
        (headers p-magit-header-regexp))
    (append (cl-subseq headers 0 col)
            r-list
            (cl-subseq headers (+ col 1) (length headers)))))

(defun p-magit--build-regexp-string (rx-list)
  "Build new regexp string from RX-LIST."
  (rx-to-string `(: ,@rx-list)))

(cl-defmacro p-magit-load (word icon props &optional (target 'type))
  "Replace WORD in TARGET of a magit header with ICON & text property PROPS.

Usage:
(p-magit-load \"fix\" ?  (:foreground \"#FB6542\" :height 1.2) type)

Add WORD, a string regexp for WORD, ICON, PROPS, and TARGET to p-magit-alist.

PROPS is a text property plist. e.g. '(:foreground \"#375E97\" :height 1.2)

TARGET can take in any of the symbols in p-magit-headers. These symbols are
derived from the convention for a git-message:
<type>(<scope>): <subject>
--BLANK LINE--
<body>
--BLANK LINE--
<footer>"
  (let* ((target (p-magit--ensure-target target))
         (rgx-list (p-magit--build-regexp-list word target))
         (rgx (p-magit--build-regexp-string rgx-list)))
    `(prog1
         (add-to-list 'p-magit-alist
                      '(,word
                        :icon ,icon
                        :props ,props
                        :target ,target
                        :rgx ,rgx)))))

(defun p-magit--reload-alist ()
  "Reload contents of p-magit-alist."
  (let* ((tmp 'nil)
    (--each p-magit-alist
      (-let (((word . (&plist :icon :props :target :rgx)) it))
        (add-to-list 'tmp
                     `(,word
                       :icon ,icon
                       :props ,props
                       :target ,target
                       :rgx (p-magit--build-rgx-string
                             (p-magit--build-rgx-list ,word ,target)))))))
    (setq p-magit-alist tmp)))

(defun p-magit-prettify ()
  "Clean up magit buffer messages with dictionary in p-magit-alist."
  (interactive)
  (with-silent-modifications
    (--each p-magit-alist
      (-let (((_ . (&plist :icon :props :target :rgx)) it))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp rgx nil t)
            (compose-region
             (match-beginning 1) (match-end 1) icon)
            (when props
              (add-face-text-property
               (match-beginning 1) (match-end 1) props))))))))

(provide 'p-magit)
;; p-magit.el ends here


(defun pants-visual/prettify-magit ()
  "Prettify magit buffer with rules from pretty-magit-alist."
  (interactive)
  (with-silent-modifications
    (--each pretty-magit-alist
      (-let (((rgx icon props target) it))
        (save-excursion
          (goto-char (point-min))
          ))))
  )

(defun add-magit-faces ()
  "Add face properties and compose symbols for buffer from pretty-magit."
  (interactive)
  (with-silent-modifications
    (--each pretty-magit-alist
      (-let (((rgx icon props) it))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp rgx nil t)
            (compose-region
             (match-beginning 1) (match-end 1) icon)
            (when props
              (add-face-text-property
               (match-beginning 1) (match-end 1) props))))))))



