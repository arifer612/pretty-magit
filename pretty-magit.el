;;; -*- lexical-binding: t; -*-
;;; pretty-magit.el --- Prettify Git messages in a magit buffer

;; Copyright (C) 2022 Arif Er <arifer612@proton.me>

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Arif Er <arifer612@proton.me>
;; Created: 29 May 2022
;; URL: https://github.com/arifer612/pretty-magit
;; Package-Requires: (dash)
;; Keywords: faces, vc

;;; Commentary:

;; This package brings in the functionality to replace important keywords in a
;; Git message's header with icons specified by text properties of your choice.
;; This package is meant to work in line with the Git message conventions as
;; laid out in "Conventional Commits" https://www.conventionalcommits.org/

;; A typical Git message should be of the form where the text in angled brackets
;; are the default names for sections of the Git message:
;; <type>(<scope>): <subject>
;; --BLANK LINE--
;; <body>
;; --BLANK LINE--
;; <footer>

;;; Code:

(require 'dash)

;; Custom variables

(defcustom pretty-magit-rules
  '((test :icon 63027
          :props (:foreground "#FAAED2" :height 1.2)
          :target type
          :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(test!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)")
    (style :icon 63119
           :props (:foreground "#FFFF3D" :height 1.2)
           :target type
           :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(style!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)")
    (revert :icon 62830
            :props (:foreground "#FDFD96" :height 1.2)
            :target type
            :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(revert!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)")
    (refactor :icon 64324
              :props (:foreground "#F5F5F5" :height 1.2)
              :target type
              :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(refactor!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)")
    (perf :icon 61847
          :props (:foreground "#607D8B" :height 1.2)
          :target type
          :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(perf!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)")
    (fix :icon 61832
         :props (:foreground "#FB6542" :height 1.2)
         :target type
         :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(fix!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)")
    (feat :icon 58014 :props
          (:foreground "#8D012F" :height 1.2)
          :target type :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(feat!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)")
    (docs :icon 62072 :props
          (:foreground "#A1f757" :height 1.2)
          :target type :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(docs!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)")
    (ci :icon 59239 :props
        (:foreground "#008080" :height 1.2)
        :target type :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(ci!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)")
    (chore :icon 62945 :props
           (:foreground "#F5F5DC" :height 1.2)
           :target type :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(chore!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)")
    (build :icon 58022 :props
           (:foreground "#00008B" :height 1.2)
           :target type :rgx "\\(?:^[^ ]* \\(?:[^ ]*\\* \\)?\\(?:[^ ]* \\)?\\(build!?\\)\\(\\(?:([^ ):]*)\\)?\\): \\(.*\\)$\\)"))
  "List containing magit replacing rules.")

(defcustom pretty-magit-text-prop '()
  "Default text properties for pretty-magit.")

;; Variables:

(defvar pretty-magit--headers '((type . 1) (scope . 2) (subject . 3))
  "Alist of symbols and their group position in a Git message header.")

(defvar pretty-magit--git-ref-rx
  '((zero-or-more (not (any " "))))
  "Default rx sequence for the git-ref.")

(defvar pretty-magit--git-log-rx
  '((optional (zero-or-more (not (any " ")))
              "* "))
  "Default rx sequence for markup used when viewing magit log.")

(defvar pretty-magit--git-branch-name-rx
  '((optional (zero-or-more (not (any " ")))
              " "))
  "Default rx sequence for branch or tag names in a magit log with the 'decorate
flag on.")

(defvar pretty-magit--type-rx
  '((group (zero-or-more (not (any "("
                                   ":"
                                   " ")))))
  "Default rx sequence for the 'type component of a Git message header.")

(defvar pretty-magit--scope-rx
  '((group (optional "("
                     (zero-or-more (not (any ")"
                                             ":"
                                             " ")))
                     ")")))
  "Default rx sequence for the 'scope component of a Git message header.")

(defvar pretty-magit--subject-rx
  '((group (zero-or-more not-newline)))
  "Default rx sequence for the 'subject component of a Git message header.")

;; Functions:

(defun pretty-magit--ensure-target (target)
  "Ensure TARGET exists in pretty-magit--headers.

Return TARGET if it is a valid header, otherwise return the default 'type."
  (cond ((unless target)
         'type)
        ((assoc target pretty-magit--headers)
         target)
        (t
         (progn
           (message (concat (symbol-name target)
                            " is not a valid header. Defaulting to 'type."))
           'type))))

(defun pretty-magit--new-breaking-type-rx (word)
  "Prepare an rx sequence with a single WORD in the 'type component."
  `((group ,word)))

(defun pretty-magit--new-type-rx (word)
  "Prepare an rx sequence with a single WORD in the 'type component for breaking
changes identified by a '!' at the end."
  `((group ,word
           (optional "!"))))

(defun pretty-magit--new-scope-rx (word)
  "Prepare an rx sequence with a single WORD in the 'scope component."
  `("("
    (group ,word)
    ")"))

(defun pretty-magit--rx-list (word &optional target)
  "Prepare the final rx sequence list with WORD in TARGET."
  (let* ((target (pretty-magit--ensure-target target))
         (pretty-magit--type-rx (cond ((equal target 'type)
                                       (pretty-magit--new-type-rx
                                        (symbol-name word)))
                                      (t
                                       pretty-magit--type-rx)))
         (pretty-magit--scope-rx (cond ((equal target 'scope)
                                        (pretty-magit--new-scope-rx
                                         (symbol-name word)))
                                       (t
                                        pretty-magit--scope-rx))))
    (append '(bol)
            pretty-magit--git-ref-rx
            '(" ")
            pretty-magit--git-log-rx
            pretty-magit--git-branch-name-rx
            pretty-magit--type-rx
            pretty-magit--scope-rx
            '(": ")
            pretty-magit--subject-rx
            '(eol))))

(defun pretty-magit--rx-string (rx-list)
  "Convert RX-LIST to a regexp string."
  (rx-to-string `(: ,@rx-list)))

;;;###autoload
(defmacro pretty-magit-rx (word &optional target)
  "Return a regexp string to search for WORD in TARGET."
  (pretty-magit--rx-string (pretty-magit--rx-list word target)))

(defun pretty-magit--rule-exist-p (rule)
  "Check if RULE exists in pretty-magit-rules.

The WORD and SCOPE of rules are used to check for existence. If RULE exists,
the ICON and PROPS are compared. If all are equal, return 't, otherwise return
the index number of the rule in pretty-magit-rules. If the rules does not exist,
return 'nil."
  (catch 'exist
    (unless pretty-magit-rules
      (throw 'exist 'nil))
    (-let* (((r-word . (&plist :icon r-icon :props r-props :target r-target))
             rule)
            (r-target (cond (r-target)
                            (t 'type))))
      (--each pretty-magit-rules
        (-let (((word . (&plist :icon :props :target)) it))
          (when (and (string= word r-word)
                     (equal target r-target))
            (when (and (equal icon r-icon)
                       (equal props r-props))
              (throw 'exist t))
            (throw 'exist it-index)))))))

(defun pretty-magit--rulep (rule)
  "Return 't if RULE is a proper rule.

Rules are alists where the car is the word to be replaced. Its cdr is a plist
with keyword-value pairs of the icon, props, and target. Proper rules need to
have a car and a value for the icon property.

e.g.
'(docs
  :icon ?
  :props (:foreground \"#3F681C\" :height 1.2)
  :target type)"
  (cond ((not (car rule))
         'nil)
        ((not (plist-get (cdr rule) :icon))
         'nil)
        (t)))

(defun pretty-magit--add-rule (rule)
  "Add a single rule for replacing WORD to ICON with PROPS in TYPE."
  (when (pretty-magit--rulep rule)
    (-let* (((word . (&plist :icon :props :target)) rule)
            (target (pretty-magit--ensure-target target))
            (rx-list (pretty-magit--rx-list word target))
            (rgx (pretty-magit--rx-string rx-list))
            (props (cond (props)
                         (t pretty-magit-text-prop)))
            (rule `(,word :icon ,icon :props ,props :target ,target :rgx ,rgx))
            (elt (pretty-magit--rule-exist-p rule)))
      (cond ((integerp elt)
             (setf (nth elt pretty-magit-rules) rule))
            ((not elt)
             (progn
               (push rule pretty-magit-rules)
               rule))
            (t
             rule)))))

;;;###autoload
(defun pretty-magit-add-rule (rule-list)
  "Add a list of rules specified in RULE-LIST for replacing WORD to ICON with
  PROPS into TYPE.

e.g. a single rule
(pretty-magit-add-rule '(docs
                         :icon ?
                         :props (:foreground \"#3F681C\" :height 1.2)
                         :target type))

e.g. 2 rules
(pretty-magit-add-rule '((docs
                          :icon ?
                          :props (:foreground \"#3F681C\" :height 1.2)
                          :target type)
                         (fix
                          :icon ?
                          :props (:foreground \"#FB6542\" :height 1.2)
                          :target type)))"
  (cond ((when (pretty-magit--rulep rule-list)
           (pretty-magit--add-rule rule-list)))
        (t
         (--each rule-list
           (when (pretty-magit--rulep it)
             (pretty-magit--add-rule it))))))

(defun pretty-magit-reload-rules ()
  "Rebuild pretty-magit-rules with the default regexp strings.

Useful if it is edited by hand and the regexp string needs to be fixed. Improper
rules will be deleted."
  (interactive)
  (let* ((tmp (copy-alist pretty-magit-rules)))
    (setq pretty-magit-rules 'nil)
    (pretty-magit-add-rule tmp)))

;;;###autoload
(defun pretty-magit-prettify ()
  "Clean up the magit buffer using pretty-magit-rules."
  (interactive)
  (with-silent-modifications
    (--each pretty-magit-rules
      (-let (((_ . (&plist :icon :props :target :rgx)) it))
        (save-excursion
          (goto-char (point-min))
          (let ((match-index (cdr (assoc target pretty-magit--headers)))
                (scope-index (cdr (assoc 'scope pretty-magit--headers))))
            (while (search-forward-regexp rgx nil t)
              (let* ((scope (> (match-end scope-index)
                               (match-beginning scope-index)))
                     (match (match-string match-index))
                     (beg (match-beginning match-index))
                     (end (if (and (equal target 'type)
                                   scope)
                              (match-end match-index)
                            (+ 1 (match-end match-index))))
                     (icon-length (length (string icon)))
                     (icon (if (and (equal target 'type)
                                    scope)
                               (concat (string icon) " ")
                             (string icon)))
                     (breaking (and (equal target 'type)
                                    (string=
                                     (substring match -1)
                                     "!"))))
                (replace-region-contents beg end (lambda () icon))
                (add-face-text-property beg (+ beg icon-length) props)
                (when breaking
                  (add-face-text-property beg (+ beg icon-length)
                                          '(:underline "#FF0000") t))))))))))

;; Minor mode:

;;;###autoload
(define-minor-mode pretty-magit-mode
  "Prettify Git messages on Magit with icons."
  :init-value nil :lighter nil :keymap nil
  (dolist (target '(magit-status magit-refresh-buffer))
    (if (bound-and-true-p pretty-magit-mode)
        (advice-add target :after #'pretty-magit-prettify)
      (advice-remove target #'pretty-magit-prettify))))

(provide 'pretty-magit)
;;; pretty-magit.el ends here
