;;; refuse.el --- Extract references from PDF files and use them somewhere -*- lexical-binding: t -*-

;; Copyright © 2022 Mykhailo Shevchuk

;; Author: Mykhailo Shevchuk <mail+dev@mshevchuk.com>
;; Created: 05 January 2022
;; URL: https://github.com/myshevchuk/refuse
;; Keywords: bib, files, wp
;; 
;; Package-Requires: ((emacs "27.2") (bibtex-completion "2.0.0"))
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; see the file LICENSE.  If not, visit
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; RefUse allows to extract text referenes from PDF files and convert them into
;; different formats.  It is an Emacs interface to the command-line tool AnyStyle.

;;; Code:
;;
;; This package used to be a part of Org Roam BibTeX (ORB) as
;; orb-pdf-scrapper.el.  Older commits can be tracked here:
;; https://github.com/org-roam/org-roam-bibtex

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'refuse-anystyle)

(require 'cl-lib)
(require 'cl-extra)
(require 'bibtex)
(require 'rx)

(require 'orb-core)

(require 'parsebib)
(require 'bibtex-completion)
(require 'bibkey)

(eval-when-compile
  (require 'subr-x))

(defvar refuse-parse-buffer-function
  (or (and (fboundp 'parsebib-parse-buffer)
           #'parsebib-parse-buffer)
      (and (fboundp 'parsebib-parse-bib-buffer)
           #'parsebib-parse-bib-buffer))
  "Ensure compatibility with different versions of 'parsebib'.")


;; ============================================================================
;;; Customize definitions
;; ============================================================================

(defgroup refuse nil
  "Refuse - retrieve references from PDF."
  :group 'refuse
  :prefix "refuse-")

(defgroup refuse-anystyle nil
  "Elisp interface to `anystyle-cli`."
  :group 'refuse
  :prefix "refuse-anystyle-")

(defcustom refuse-prompt-to-generate-keys 'when-buffer-modified
  "Prompt the user to generate keys in the BibTeX buffer?
After having finished editing in the BibTeX buffer and before
proceeding to Org buffer, the user will be prompted
to (re-)generate citation keys according to the value of this
option on these occasions:

- symbol `when-buffer-modified' - only when the buffer has
  modified and changes have not been saved
- nil - never
- t or any other value - always."
  :group 'refuse
  :type '(choice
          (const when-buffer-modified)
          (const :tag "Yes" t)
          (const :tag "No" nil)))

(defcustom refuse-grouped-export
  '((in-roam "In Org Roam database" list)
    (in-bib "In BibTeX file" list)
    (valid "Valid citation keys" table)
    (invalid "Invalid citation keys" table))
  "Determines appearence of grouped references in Org view.
A list of five elements of the form (GROUP TITLE TYPE).

GROUP must be one of the symbols `parent', `in-roam', `in-bib',
`valid' or `invalid'.

TITLE is an arbitrary string, which will be the title of the
group's headline.

TYPE must be one of the symbols `list' or `table' determining how
the generated citations will appear under the group's headline.
TYPE is ignored for the `parent' group and defaults to `list' for
other groups when set to nil.

Takes effect when `refuse-group-references' is t."
  :type '(list (list :tag "\nIn-roam"
                     (const :format "" in-roam)
                     (string :tag "Title")
                     (radio :tag "Type" :value list
                            (const list) (const table)))
               (list :tag "\nIn-bib"
                     (const :format "" in-bib)
                     (string :tag "Title")
                     (radio :tag "Type" :value list
                            (const list) (const table)))
               (list :tag "\nValid"
                     (const :format "" valid)
                     (string :tag "Title")
                     (radio :tag "Type" :value table
                            (const list) (const table)))
               (list :tag "\nInvalid"
                     (const :format "" invalid)
                     (string :tag "Title")
                     (radio :tag "Type" :value table
                            (const list) (const table))))
  :group 'refuse)

(defcustom refuse-ungrouped-export 'list
  "Determines appearence of ungrouped references in Org view.
Valid values are the symbols `list' and `table'.

Takes effect when `refuse-group-references' is nil."
  :group 'refuse
  :type '(radio
          (const list)
          (const table)))

(defcustom refuse-table-export-fields
  '("#" "citekey" "author" "editor" "journal"
    "date" "volume" "pages")
  "BibTeX fields for export into Org mode tables in the Org view.
A list in which each element is of form FIELD or (FIELD . TYPES).
The order of items in this list determines the order of table
columns.

FIELD is a field to export.  Field mapping according to the value
of `orb-bibtex-field-aliases' is recognized.  The non-standard
BibTeX field `citation-number' created during the reference
extraction process is treated specially according to the value of
the variable `refuse-reference-numbers'.

TYPES is a list of strings corresponding to BibTeX entry types
for which to export the FIELD.  E.g. a value of a list cell

\(\"editor\" . \"collection\")

means to export the value of the field \"editor\" only for
entries whose entry type is \"collection\".  If it is
nil (default), export the FIELD for all entry types.

See also the variables `refuse-grouped-export' and
`refuse-ungrouped-export', which allow to choose between
list and table export."
  :type '(repeat (string :tag "Field"))
  :group 'refuse)

(defcustom refuse-group-references t
  "If this is non-nil, group the retrieved references in the Org view.
These groups are `in-roam', `in-bib', `valid' and `invalid'."
  :group 'refuse
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)))

(defcustom refuse-list-style 'unordered-hyphen
  "Format of Org lists produced by Refuse.
Valid values are symbols:
`ordered-point'       => 1. citation
`ordered-parenthesis' => 1) citation
`unordered-hyphen'    => - citation
`unordered-plus'      => + citation
`unordered-asterisk'  => * citation

This variable can also take a string value, in which case the
string should be a valid format string containing the '%s'
specifier, which will be used to insert citation numbers, for
example:

\"- [%s] \"            => - [1] citation

The format string should take care of padding as no additional
spaces will be inserted automatically.

The variable `refuse-reference-numbers' allows to
choose the numbering scheme for ordered and custom format lists."
  :group 'refuse
  :type
  '(choice
    (const :tag "Point-terminated ordered list" ordered-point)
    (const :tag "Parenthesis-terminated ordered list" ordered-parenthesis)
    (const :tag "Hyphen-led unordered list" unordered-hyphen)
    (const :tag "Plus-led unordered list" unordered-plus)
    (const :tag "Asterisk-led unordered list" unordered-asterisk)
    (string :tag "Custom ordered format")))

(defcustom refuse-reference-numbers 'citation-number
  "This variable specifies the source of ordered list numbers.
Valid values are:

`citation-number' - use the extracted `citation-number',
stripping off any non-numerical characters.  Fall back to
unordered hyphen-led list if this field is non-existent, empty,
or does not contain a number.

`citation-number-alnum' - use the extracted `citation-number',
stripping off any non-alphanumerical characters, fall back to
unordered hyphen-led list if this field is non-existent, empty or
does not contain alphanumerical characters.  This value will only
have effect when `refuse-list-style' is a custom format
string and behave as `citation-number' otherwise.

`as-retrieved' - use the natural order of BibTeX entry in the buffer

If the value of `refuse-list-style' is one of the
'unordered' choices, this variable will have no effect."
  :group 'refuse
  :type '(radio
          (const :tag "Citation number" citation-number)
          (const :tag "Alphanumerical citation number" citation-number-alnum)
          (const :tag "As retrieved" as-retrieved)))

(defcustom refuse-citekey-format "cite:%s"
  "Format of the citekey for insertion into Org mode buffer."
  :group 'refuse
  :type 'string)

(defcustom refuse-export-options
  '((org (heading "References (extracted by Refuse)"
                   :property-drawer (("REFUSE_TYPE")
                                     ("REFUSE_SOURCE")
                                     ("REFUSE_DATE")))))
  "Options for automatic export of references extracted by Refuse.
This variable is an association list of the form
\(TYPE . ((TARGET LOCATION PROPERTIES))).

TYPE is the type of the exported data, one of the symbols `txt',
`bib' or `org'.  The data will only be exported if its
corresponding symbol is present on the list.

TARGET must be one of the symbols `heading' or `path'.  The
symbol `heading' means export the data under a heading in the
buffer of origin, the Org-mode buffer where the Refuse
process was started.  The symbol `path' means export the data to
another file.  It is possible to specify both export targets
simultaneously for a given export TYPE or multiple targets of the
same type.

Example:
\(setq refuse-export-options
      '((org (heading HEADLINE PROPERTIES)
             (path LOCATION PROPERTIES))))

LOCATION (HEADLINE) is a string specifying location of the TARGET.

- If TARGET is `heading', the supplied string will be used as
headline text.  The data will be exported slightly differently
depending on TYPE.  Text references will be exported as is.
BibTeX references will be put into an Org-mode source code block.
Org-mode references, if grouped under different headings, will be
exported with the headings demoted by one level.

- If TARGET is `path', the supplied string will be used as a
filesystem target.  The path can be absolute or relative, in the
latter case it will be relative to the directory of the buffer of
origin.  If the path (absolute or relative) is an *existing*
directory, the full path to the target file will be constructed
from the supplied string as a directory name, the #+ROAM_KEY:
property in the buffer of origin as a file name and TYPE as the
file's extension.  If the path is not an existing directory, it
will be treated as a file name, and the data will be exported
there.  The file will be created if it does not exist:

> \"~/org/references.org\"           - absolute path
> \"this-note's-bib-references.bib\" - path relative to the buffer of origin
> \"~/refuse-references/\" - absolute directory path.

In the latter case, if the directory exists, the extracted data
will be put into a file \"Doe2020.bib\", assuming the #+ROAM_KEY:
property is \"Doe2020\" and TYPE is `bib'.  If the directory does
not exist, the extracted data will be put into a (newly created)
file \"~/refuse-references/\".

Example:
\(setq refuse-export-options
      '((org (heading \"Org references\" PROPERTIES))
        (txt (path \"text-references/\" PROPERTIES))))

PROPERTIES is a property list providing additional export
specifications.  Some properties are specific to only certain
export TYPEs or TARGETs.

`:placement' property allows to specify placement of the exported
data.  It can be a symbol `prepend' or `append'.

When TARGET is `path', text data is simply put at the beginning
or end of the target file accordingly to the value of the
`:placement' property.  Org-mode data is placed before the first
or after the last heading, respectively.  Similarly, BibTeX data
is placed or before the first or after the last entry, comments
and @String entries ignored.

When TARGET is `heading', this property specifies whether the
parent heading should be put before or after other headings.

When TARGET is `path' and LOCATION is an Org-mode file, the value
of the `:placement' property can also be a list of the form
\(heading HEADLINE PROPERTIES).  In this case the data will be
put in the target file under a heading with HEADLINE as the
headline text.  PROPERTIES are additional export options as
described here and below.  The `heading' value of the
`:placement' property cannot be used recursively in this case.

Example:
\(setq refuse-export-options
      '((org (path \"references.org\" :placement append))
        (bib (path \"references.bib\" :placement prepend))
        (txt (path \"references.txt\"
                   :placement (headline \"References\" :placement append)))))

If placement is not specified, the data is appended by default.

`:property-drawer' property allows to supply a heading with some
properties.  The value of this property is list with
elements (PROPERTY_NAME . PROPERTY_VALUE) or PROPERTY_NAME, the
latter form being treated as (PROPERTY_NAME . nil).

PROPERTY_NAME must be a string, it will be used as a property
name.  PROPERY_VALUE can be a string, in which case it will be
used as the value of the property.  It can also be a function
name as an unquoted symbol, in which case this function will be
called to get the value of the property.  The return value must
be a string.

The following properties are recognized internally and will be
supplied with automatically generated values if PROPERTY_VALUE is
nil:

> REFUSE_TYPE   - TYPE of the export data
> REFUSE_SOURCE - name of the PDF file the data were extracted from
> REFUSE_DATE   - time and date the data were exported

Example:
\(setq refuse-export-options
      '((org (heading \"Org references\" PROPERTIES)))
        (txt (heading \"Text references\"
                       :property-drawer
                       '((\"REFUSE_TYPE\" . \"text\")
                         \"REFUSE_DATE\"
                         \"REFUSE_SOURCE\"
                         (\"PROPERTY_1\" . \"VALUE_1\")
                         (\"PROPERTY_2\" . my-function))))))

`:filter-bib-entries' property controls filtering of exported
BibTeX entries.  If the value of this property is non-nil and
TARGET is a BibTeX file, only the entries that are not already
present in this file will be exported.  The value can also be a
string or a list of strings specifying BibTeX file(s), or a
variable as an unquoted symbol holding a string or a list of
strings specifying BibTeX file(s), in which cases the entries
will be filtered also against this/these file(s) in *addition* to
the TARGET file.  In such instances, filtering will also be
applied to entries exported to an Org-mode heading.

Example:
\(setq refuse-export-options
      '((bib (path \"references.bib\" :filter-bib-entries t
             (heading \"BibTeX references\"
                       :filter-bib-entries bibtex-completion-bibliography))))."

  :group 'refuse
  :risky t
  :type '(repeat list))

(defcustom refuse-set-fields
  '(("author" refuse--invalidate-nil-value)
    ("editor" refuse--invalidate-nil-value
     "book" "collection")
    ("title" refuse--invalidate-nil-value)
    ("journal" refuse--invalidate-nil-value
     "article")
    ("date" refuse--invalidate-nil-value)
    ("volume" refuse--invalidate-nil-value
     "article" "incollection")
    ("pages" refuse--fix-or-invalidate-range
     "article" "incollection"))
  "BibTeX fields to set during key generation.
A list in which each element is a list of the form (FIELD FUNCTION . TYPES).

FIELD is a BibTeX field name to be set.

FUNCTION is a function that will be called to generate the value,
it takes one argument ENTRY, which is the current entry.

TYPES is a list of strings corresponding to BibTeX entry types
for which the FIELD should be set.  If it is nil, set the FIELD
for all entry types."
  :risky t
  :type '(repeat
          (list :tag "Item"
                (string :tag "Field")
                (function :tag "Function")
                (repeat :tag "Entry types" :inline t
                        (string :tag "Type"))))
  :group 'refuse)

(defcustom refuse-invalid-key-pattern "\\`.*N/A.*\\'"
  "Regexp to match an invalid key."
  :type 'regexp
  :group 'refuse)


;; ============================================================================
;;; Helper functions: general utils
;; ============================================================================

(defun refuse-buffer-string (&optional start end)
  "Retun buffer (sub)string with no text porperties.
Like `buffer-substring-no-properties' but START and END are
optional and equal to (`point-min') and (`point-max'),
respectively, if nil."
  (buffer-substring-no-properties (or start (point-min))
                                  (or end (point-max))))


;; ============================================================================
;;;; Helper functions: temporary files
;; ============================================================================

;;;;; Code in this section was adopted from ob-core.el
;;
;; Copyright (C) 2009-2020 Free Software Foundation, Inc.
;;
;; Authors: Eric Schulte
;;          Dan Davison

(defvar refuse--temp-dir)
(unless (or noninteractive (boundp 'refuse--temp-dir))
  (defvar refuse--temp-dir
    (or (and (boundp 'refuse--temp-dir)
             (file-exists-p refuse--temp-dir)
             refuse--temp-dir)
        (make-temp-file "refuse-" t))
"Directory to hold temporary files created during reference parsing.
Used by `refuse-temp-file'.  This directory will be removed on Emacs
shutdown."))

(defun refuse-temp-file (prefix &optional suffix)
  "Create a temporary file in the `refuse--temp-dir'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of variable `temporary-file-directory' temporarily set to
the value of `refuse--temp-dir'."
  (let ((temporary-file-directory
         (or (and (boundp 'refuse--temp-dir)
                  (file-exists-p refuse--temp-dir)
                  refuse--temp-dir)
             temporary-file-directory)))
    (make-temp-file prefix nil suffix)))

(defun refuse--remove-temp-dir ()
  "Remove `refuse--temp-dir' on Emacs shutdown."
  (when (and (boundp 'refuse--temp-dir)
             (file-exists-p refuse--temp-dir))
    ;; taken from `delete-directory' in files.el
    (condition-case nil
        (progn
          (mapc (lambda (file)
                  ;; This test is equivalent to
                  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
                  ;; but more efficient
                  (if (eq t (car (file-attributes file)))
                      (delete-directory file)
                    (delete-file file)))
                (directory-files refuse--temp-dir 'full
                                 directory-files-no-dot-files-regexp))
          (delete-directory refuse--temp-dir))
      (error
       (message "Failed to remove temporary Org-roam-bibtex directory %s"
                (if (boundp 'refuse--temp-dir)
                    refuse--temp-dir
                  "[directory not defined]"))))))

(add-hook 'kill-emacs-hook 'refuse--remove-temp-dir)

;;;;; End of code adopted from ob-core.el


;; ============================================================================
;;; Helper functions: BibTeX buffer and citekey-related routines
;; ============================================================================

(defvar refuse--refs nil
  "Internal list with cell format (CITEKEY ENTRY . VALIDP).")

(defun refuse--invalidate-nil-value (field entry)
  "Return value of FIELD or `bibkey-empty-field-token' if it is nil.
ENTRY is a BibTeX entry."
  (bibkey-get-value field entry bibkey-empty-field-token))

(defun refuse--fix-or-invalidate-range (field entry)
  "Replace missing or non-standard delimiter between two strings with \"--\".
FIELD is the name of a BibTeX field from ENTRY.  Return
`bibkey-empty-field-token' if the value is nil.

This function is primarily intended for fixing anystyle parsing
artefacts such as those often encountered in \"pages\" field,
where two numbers have only spaces between them."
  (replace-regexp-in-string "\\`[[:alnum:]]*?\\([- –]+\\)[[:alnum:]]*\\'"
                            "--"
                            (bibkey-get-value
                             field entry bibkey-empty-field-token)
                            nil nil 1))

(defun refuse--get-entry-info (entry &optional collect-only)
  "Collect information about ENTRY.
Take a bibtex entry as returned by `bibtex-completion-get-entry'
and return a plist with the following keys set:

:key            |string | citekey generated with `bibkey-generate-key'
:validp         |boolean| according to `refuse-invalid-key-pattern'
:set-fields     |(cons) | as per `refuse-set-fields'

Each element of `:set-fields' list is a a cons cell (FIELD . VALUE).

If optional COLLECT-ONLY is non-nil, do not generate the key,
`:set-fields' is set to nil."
  (let ((type (bibkey-get-value "=type=" entry))
        ;; return values
        key validp fields-to-set
        ;; internal variable
        fields)
    ;; when requested to collect keys, just do that
    (if collect-only
        (setq key (bibkey-get-value "=key=" entry)
              fields entry)
      ;; otherwise
      ;; prepare fields for setting
      (dolist (field-to-set refuse-set-fields)
        (let ((field-name (car field-to-set))
              (types-to-set (cddr field-to-set)))
          ;; push the field for setting only when entry type is one of the
          ;; specified types or nil, which means set the field regardless of
          ;; entry type
          (when (or (not types-to-set)
                    (member type types-to-set))
            (push (cons field-name
                        ;; call the function if provided
                        (if-let ((fn (cadr field-to-set)))
                            (funcall fn field-name entry)
                          ;; otherwise get the value from current entry
                          (bibkey-get-value field-name entry "")))
                  fields-to-set))))
      ;; prioritize fields from fields-to-set over entry fields
      ;; for autokey generation
      (let ((-compare-fn (lambda (x y)
                           (string= (car x) (car y)))))
        (setq fields (-union fields-to-set entry)
              key (bibkey-generate-key fields))))
    ;; validate the new shiny key (or the old existing one)
    ;; not sure if save-match-data is needed here
    ;; but it seems to be always a good choice
    (save-match-data
      (setq validp (and (not (string-match-p
                              refuse-invalid-key-pattern key))
                        t)))
    ;; return the entry
    (list :key key
          :validp validp
          :set-fields fields-to-set)))

(defun refuse--update-record-at-point
    (&optional collect-only natural-order)
  "Generate citation key and update the BibTeX record at point.
Calls `refuse--get-entry-info' to get information about
BibTeX record at point and updates it accordingly.  If optional
COLLECT-ONLY is non-nil, do not generate the key and do not set
the fields.

If optional argument NATURAL-ORDER is non-nil, set the field
'natural-order' of the returned entry to its value.

This is an auxiliary function for command
`refuse-generate-keys'."
  (let* ((entry (parsebib-read-entry (parsebib-find-next-item)))
         (key-plist (refuse--get-entry-info entry collect-only))
         (new-key (plist-get key-plist :key))
         (validp (plist-get key-plist :validp))
         (fields-to-set (plist-get key-plist :set-fields)))
    (unless collect-only
      (save-excursion
        ;; update citekey
        ;; adjusted from bibtex-clean-entry
        (bibtex-beginning-of-entry)
        (re-search-forward bibtex-entry-maybe-empty-head)
        (if (match-beginning bibtex-key-in-head)
            (delete-region (match-beginning bibtex-key-in-head)
                           (match-end bibtex-key-in-head)))
        (insert new-key)
        ;; set the bibtex fields
        (when fields-to-set
          (save-excursion
            (dolist (field fields-to-set)
              (let ((name (car field))
                    (value (cdr field))
                    bound)
                ;; (bibtex-set-field name value)
                (bibtex-beginning-of-entry)
                (when (setq bound (bibtex-search-forward-field name t))
                  (goto-char (car (cdr bound)))
                  (bibtex-kill-field nil t))
                (bibtex-make-field (list name "" value) t t)))
            ;; Some hard-set cosmetic changes
            (let ((bibtex-entry-format
                   '(whitespace realign unify-case braces sort-fields))
                  (bibtex-field-indentation 2)
                  (fill-column 160))
              (bibtex-clean-entry))))))
    ;; return the result ((NEW-KEY . ENTRY) . VALIDP)
    ;; TODO: for testing until implemented
    (when natural-order
      (cl-pushnew `("natural-order" . ,natural-order) entry))
    (cons new-key (cons entry validp))))

(defun refuse--sort-refs (refs)
  "Sort references REFS.
Auxiliary function for `refuse-generate-keys'.
REFS should be an alist of form ((CITEKEY . FORMATTED-ENTRY) . VALIDP).

References validated by `refuse-keygen-function' function
are further sorted into four groups:

'in-roam - available in the `org-roam' database;
'in-bib  - available in `bibtex-completion-bibliography' file(s);
'valid   - marked valid by the keygen function but are not
           available in user database(s);
'invalid - marked invalid by the keygen function."
  (let* ((bibtex-completion-bibliography (refuse--get :global-bib))
         ;; When using a quoted list here, sorted-refs is not erased in
         ;; consecutive runs
         (sorted-refs (list (list 'in-roam) (list 'in-bib)
                            (list 'valid) (list 'invalid))))
    (dolist (ref refs)
      (cond ((org-roam-db-query [:select [ref]
                                 :from refs
                                 :where (= ref $s1)]
                                (format "%s" (car ref)))
             (push ref (cdr (assoc 'in-roam sorted-refs))))
            ((bibtex-completion-get-entry (car ref))
             (push ref (cdr (assoc 'in-bib sorted-refs))))
            ((cddr ref)
             (push ref (cdr (assoc 'valid sorted-refs))))
            (t
             (push ref (cdr (assoc 'invalid sorted-refs))))))
    sorted-refs))


;; ============================================================================
;;; Helper functions: Org buffer-related routines
;; ============================================================================

(defun refuse--get-reference-number
    (entry &optional numbering-source)
  "ENTRY NUMBERING-SOURCE."
  (let ((numbering-source
         (or numbering-source refuse-reference-numbers)))
    (cl-case numbering-source
      (citation-number
       (--> (bibkey-get-value "citation-number" entry nil)
            (when (and it (string-match ".*?\\([0-9]+\\).*?" it))
              (match-string 1 it))))
      (citation-number-alnum
       (--> (bibkey-get-value "citation-number" entry nil)
         (when (and it (string-match "\
[^[:alnum:]]?\\([[:digit:]]*\\)\\([^[:alnum:]]*\\)\\([[:alpha:]]*\\)" it))
              (concat (match-string 1 it) (match-string 3 it)))))
      (as-retrieved
       (bibkey-get-value "natural-order" entry ""))
      (t (user-error "Unsupported reference numbers source: %s"
                     numbering-source)))))

(defun refuse--insert-org-as-list (ref-alist)
  "Insert REF-ALIST as Org-mode list."
  (let* ((numbering-source
          (if (and (eq refuse-reference-numbers
                       'citation-number-alnum)
                   (not (stringp refuse-list-style)))
              'citation-number
            refuse-reference-numbers))
         (leader
          (cl-case refuse-list-style
            (ordered-point "%s. ")
            (ordered-parenthesis "%s) ")
            (unordered-hyphen "- ")
            (unordered-plus "+ ")
            (unordered-asterisk "* ")
            (t (if (stringp refuse-list-style)
                   refuse-list-style
                 (user-error "REFUSE: Unrecognized list style %s requested"
                             refuse-list-style)))))
         (unorderedp
          (memq refuse-list-style
                '(unordered-hyphen unordered-plus unordered-asterisk)))
         (fallback (if unorderedp leader "- ")))
    (dolist (ref ref-alist)
      (let* ((citekey (format refuse-citekey-format (car ref)))
             (entry (cadr ref))
             (number (unless unorderedp
                       (refuse--get-reference-number
                        entry numbering-source))))
        (insert (refuse-anystyle-format leader `(,number . ,fallback)) citekey "\n")))))

(defun refuse--get-export-value (field entry)
  "Get FIELD value from ENTRY.
Similar to `bibkey-get-value' but does some additional cleaning."
  ;; list fields for org export
  (let* ((field (or (car (rassoc field orb-bibtex-field-aliases))
                    field))
         (value (bibkey-get-value field entry "")))
    ;; truncate author list to first three names, append et.al instead
    ;; of the remaining names
    ;; This is a hard-coded "reasonable default"
    ;; and it may be replaced with something more
    ;; flexible in the future
    (cond
     ((member field '("author" "editor"))
      (--> value
           (split-string it " and " t "[ ,.;:-]+")
           (if (> (length it) 3)
               (append (-take 3 it) '("et.al."))
             it)
           (concat (mapconcat #'identity it "; "))))
     ((string= field "citation-number")
      (refuse--get-reference-number entry))
     ((string= field "=key=")
       (format refuse-citekey-format value))
     (t value))))

(defun refuse--insert-org-as-table (ref-alist)
  "Insert REF-ALIST as Org-mode table."
  (insert
   (format "|%s\n" (mapconcat #'identity
                              refuse-table-export-fields "|")))
  (forward-line -1)
  (org-table-insert-hline)
  (forward-line 2)
  (let ((table ""))
    (dolist (ref ref-alist)
      (setq table
            (format "%s|%s|\n" table
                    (mapconcat
                     (lambda (field)
                       (refuse--get-export-value field (cadr ref)))
                     refuse-table-export-fields "|"))))
    (insert table))
  (forward-line -1)
  (org-table-align))

(defun refuse--insert-refs ()
  "Insert the references list as org structure.
If `refuse-group-references' is non-nil, sort the references into
categories `in-roam', `in-bib', `valid', `invalid'.  Make a plain
list otherwise."
  (cond
   (refuse-group-references
    (dolist (ref-group
             (refuse--sort-refs refuse--refs))
      (when-let* ((group (car ref-group))
                  (refs (cdr ref-group))
                  (heading
                   (cdr (assoc group
                               refuse-grouped-export)))
                  (title (car heading))
                  (type (cadr heading))
                  (pos (make-marker)))
        ;; NOTE: Investigate
        ;; The behaviour of org-insert-heading has changed at some point:
        ;; If in an empty buffer, e.g. temp-buffer, the function fails messaging "beginning of buffer"
        (org-N-empty-lines-before-current 1)
        (org-insert-heading '(16) nil t)
        ;; insert heading
        (insert (format "%s\n" title))
        (org-N-empty-lines-before-current 1)
        ;; insert references
        (insert (format "#+name: %s\n" group))
        (set-marker pos (point))
        (set-marker-insertion-type pos t)
        (cl-case type
          ('table
           (refuse--insert-org-as-table refs))
          (t
           (refuse--insert-org-as-list refs)))
        (goto-char pos))))
   (t
    (insert "\n")
    (let ((refs (nreverse refuse--refs)))
      (cl-case refuse-ungrouped-export
        ('table
         (refuse--insert-org-as-table refs))
        (t
         (refuse--insert-org-as-list refs))))))
  (goto-char (point-max))
  (org-N-empty-lines-before-current 0))


;; ============================================================================
;;; Helper functions: Dispatcher
;; ============================================================================

(defvar refuse--plist nil
  "Communication channel for Refuse.")

(defvar refuse--buffer "*Refuse*"
  "Refuse special buffer.")

(defmacro refuse-with-buffer (&rest body)
  "Execute BODY with `refuse--buffer' as current.
If the buffer does not exist it will be created."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (set-buffer (get-buffer-create refuse--buffer))
     ,@body))

(defmacro refuse-with-current-context (context &rest body)
  "Execute BODY if CONTEXT is current context.
Run `refuse-keygen-function' with `error' context
otherwise.  If CONTEXT is a list then current context must be a
member of that list."
  (declare (indent 1) (debug t))
  `(if (not (refuse--current-context-p ,context))
       (refuse-dispatcher 'error)
     ,@body))

(defmacro refuse-with-progress (message &rest body)
  "Wrap BODY into a simple progress reporter form.
MESSAGE is passed to `make-progress-reporter'.
Return result of evaluating the BODY."
  (declare (indent 1) (debug (stringp &rest form)))
  (let ((reporter (gensym "rfs")))
    `(let ((,reporter (make-progress-reporter ,message)))
       ,@body
       (progress-reporter-done ,reporter))))

(defun refuse--current-context-p (context)
  "Return t if CONTEXT is current context.
CONTEXT can also be a list, in which case t is returned when
current context is its memeber."
  (if (listp context)
      (memq (refuse--get :context) context)
    (eq (refuse--get :context) context)))

(defun refuse--refresh-mode (mode)
  "Restart `refuse-mode' with new major MODE."
  (cl-case mode
    ('txt
     (text-mode)
     (refuse--put :callee 'edit-bib
                            :context 'start
                            :caller 'edit-txt))
    ('bib
     (bibtex-mode)
     ;; anystyle uses biblatex dialect
     (bibtex-set-dialect 'biblatex t)
     (refuse--put :callee 'edit-org
                            :context 'start
                            :caller 'edit-bib))
    ('org
     (org-mode)
     (refuse--put :callee 'checkout
                            :context 'start
                            :caller 'edit-org))
    ('xml
     (xml-mode)
     (cl-case (refuse--get :context)
       ;; since :callee is not used in training session, we set :callee here to
       ;; the original :caller, so that we can return to the editing mode we
       ;; were called from if the training session is to be cancelled
       ('start
        (refuse--put :callee (refuse--get :caller)
                               :context 'edit
                               :caller 'edit-xml))))
    ('train
     (fundamental-mode)
     (cl-case (refuse--get :context)
       ('train
        (refuse--put :context 'train
                               :caller 'train))
       ;; Since the session was not cancelled, we return to text, as everything
       ;; else should be regenerated anyway.
       ('finished
        (refuse--put :callee 'edit-txt
                               :context 'continue
                               :caller 'train))))
    (t
     (unwind-protect
         (error "Oops...something went wrong.  \
Pressing the RED button, just in case")
       (refuse-dispatcher 'error))))
  (set-buffer-modified-p nil)
  (setq mark-active nil)
  (refuse-mode -1)
  (refuse-mode +1)
  (goto-char (point-min)))

(defun refuse--edit-txt ()
  "Edit text references in `refuse--buffer'."
  ;; callee will be overridden in case of error
  (cl-case (refuse--get :context)
    ;; parse pdf file and switch to text editing mode
    ('start
     (let ((temp-txt (refuse-temp-file "refuse-" ".txt"))
           (pdf-file (refuse--get :pdf-file)))
       (refuse--put :temp-txt temp-txt)
       (let ((same-window-buffer-names (list refuse--buffer)))
         (pop-to-buffer refuse--buffer))
       (setq buffer-file-name nil)
       (refuse-with-progress (format "Scrapping %s.pdf" (f-base pdf-file))
         (erase-buffer)
         (refuse-anystyle 'find
           :format 'ref
           :layout nil
           :finder-model refuse-anystyle-finder-model
           :input pdf-file
           :stdout t
           :buffer refuse--buffer))
       (setq buffer-undo-list nil)
       (refuse--refresh-mode 'txt)))
    ;; read the previously generated text file
    ('continue
     (if-let ((temp-txt (refuse--get :temp-txt))
              (f-exists? temp-txt))
         (progn
           (pop-to-buffer refuse--buffer)
           (erase-buffer)
           (insert-file-contents temp-txt)
           (setq buffer-undo-list (refuse--get :txt-undo-list))
           (refuse--refresh-mode 'txt))
       (refuse-dispatcher 'error)))
    (t
     (refuse-dispatcher 'error))))

(defun refuse--edit-bib ()
  "Generate and edit BibTeX data in `refuse--buffer'."
  (pop-to-buffer refuse--buffer)
  (cl-case (refuse--get :context)
    ('start
     (let* ((temp-bib (or (refuse--get :temp-bib)
                          (refuse-temp-file "refuse-" ".bib"))))
       (refuse--put :temp-bib temp-bib)
       ;; save previous progress in txt buffer
       (write-region (refuse-buffer-string)
                     nil (refuse--get :temp-txt) nil -1)
       (refuse--put :txt-undo-list (copy-tree buffer-undo-list))
       (refuse-with-progress "Generating BibTeX data"
         ;; Starting from Emacs 27, whether shell-command erases buffer
         ;; is controlled by `shell-command-dont-erase-buffer', so we
         ;; make sure the buffer is clean
         (erase-buffer)
         (refuse-anystyle 'parse
           :format 'bib
           :parser-model refuse-anystyle-parser-model
           :input (refuse--get :temp-txt)
           :stdout t
           :buffer refuse--buffer)
         (write-region (refuse-buffer-string) nil temp-bib nil -1))
       (setq buffer-undo-list nil))
     (refuse--refresh-mode 'bib))
    ('continue
     (if-let ((temp-bib (refuse--get :temp-bib))
              (f-exists? temp-bib))
         (progn
           (erase-buffer)
           (insert-file-contents temp-bib)
           (setq buffer-undo-list (refuse--get :bib-undo-list))
           (refuse--refresh-mode 'bib))
       (refuse-dispatcher 'error)))
    (t
     (refuse-dispatcher 'error))))

(defun refuse--edit-org ()
  "Edit generated Org-mode data."
  (pop-to-buffer refuse--buffer)
  (cl-case (refuse--get :context)
    ('start
     ;; if the BibTeX buffer was modified, save it and maybe generate keys
     (refuse-generate-keys
      nil
      (cl-case refuse-prompt-to-generate-keys
        ('when-buffer-modified
         (if (buffer-modified-p)
             ;; TODO: it's clumsy
             ;; not "yes" means generate
             ;; not "no" means collect only
             (not (y-or-n-p "The buffer contents has changed.  \
Generate BibTeX keys? "))
           t))
        ;; do not prompt
        (nil t)
        ;; always prompt
        (t
         (not (y-or-n-p "Generate BibTeX keys? ")))))
     (when (> (cl-random 100) 98)
       (refuse-with-progress "Pressing the RED button"))
     (write-region (refuse-buffer-string)
                   nil (refuse--get :temp-bib) nil 1)
     (refuse--put :bib-undo-list (copy-tree buffer-undo-list))
     ;; generate Org-mode buffer
     (let* ((temp-org (or (refuse--get :temp-org)
                          (refuse-temp-file "refuse-" ".org"))))
       (refuse--put :temp-org temp-org
                              :caller 'edit-org)
       ;; we must change the mode in the beginning to get all the Org-mode
       ;; facilities
       (refuse--refresh-mode 'org)
       (refuse-with-progress "Generating Org data"
         (erase-buffer)
         (refuse--insert-refs)
         (write-region (refuse-buffer-string) nil temp-org nil -1)
         (setq buffer-undo-list nil)
         (set-buffer-modified-p nil)
         (goto-char (point-min)))))
    ('continue
     (if-let ((temp-org (refuse--get :temp-org))
              (f-exists? temp-org))
         (progn
           (erase-buffer)
           (insert-file-contents temp-org)
           (setq buffer-undo-list (refuse--get :org-undo-list))
           (refuse--refresh-mode 'org))
       (refuse-dispatcher 'error)))))

(defun refuse--edit-xml ()
  "Edit XML data."
  (pop-to-buffer refuse--buffer)
  (cl-case (refuse--get :context)
    ('start
     (let* ((temp-xml (or (refuse--get :temp-xml)
                          (refuse-temp-file "refuse-" ".xml"))))
       (refuse--put :temp-xml temp-xml)
       (refuse-with-progress "Generating XML data"
         ;; save progress in text mode when called from there if called from
         ;; anywhere else, text mode progress is already saved, other data will
         ;; be re-generated anyway
         (when (eq (refuse--get :caller) 'edit-txt)
           (write-region (refuse-buffer-string)
                         nil (refuse--get :temp-txt) nil -1)
           (refuse--put :txt-undo-list (copy-tree buffer-undo-list)))
         (erase-buffer)
         (refuse-anystyle 'parse
           :format 'xml
           :parser-model refuse-anystyle-parser-model
           :input (refuse--get :temp-txt)
           :stdout t
           :buffer refuse--buffer)
         (write-region (refuse-buffer-string) nil temp-xml nil -1)
         (setq buffer-undo-list nil)
         (refuse--refresh-mode 'xml))))
    ('edit-master
     (progn
       (erase-buffer)
       (insert-file-contents refuse-anystyle-parser-training-set)
       ;; we allow the user to see which file they are editing
       (setq buffer-file-name refuse-anystyle-parser-training-set)
       (setq buffer-undo-list nil)
       (refuse--refresh-mode 'xml)))
    (t
     (refuse-dispatcher 'error))))

(defun refuse--update-master-file ()
  "Append generated XML data to `refuse-anystyle-parser-training-set'."
  (refuse-with-buffer
    (refuse-with-progress (format "Appending to master training set %s"
                                refuse-anystyle-parser-training-set)
      ;; save any progress in XML mode
      (write-region (refuse-buffer-string) nil
                    (refuse--get :temp-xml) nil -1)
      (let (new-data)
        ;; strip down the header and footer tokens from our data
        (save-excursion
          (save-match-data
            (let* (beg end)
              (goto-char (point-min))
              (re-search-forward "\\(^[ \t]*<dataset>[ \t]*\n\\)" nil t)
              (setq beg (or (match-end 1)
                            (point-min)))
              (re-search-forward "\\(^[ \t]*</dataset>[ \t]*\n\\)" nil t)
              (setq end (or (match-beginning 1)
                            (point-max)))
              (setq new-data (refuse-buffer-string beg end)))))
        ;; append our data to the master file
        (with-temp-buffer
          (insert-file-contents refuse-anystyle-parser-training-set)
          ;; backup the master file
          (let ((master-backup (concat refuse-anystyle-parser-training-set ".back")))
            (refuse--put :master-backup master-backup)
            (rename-file refuse-anystyle-parser-training-set master-backup t))
          (goto-char (point-max))
          (forward-line -1)
          (insert new-data)
          (f-touch refuse-anystyle-parser-training-set)
          (write-region (refuse-buffer-string) nil
                        refuse-anystyle-parser-training-set nil -1))))))

(defun refuse--train (&optional review)
  "Update parser training set and run anystyle train.
If optional REVIEW is non-nil, run `refuse--edit-xml'
in `:edit-master' context."
  (pop-to-buffer refuse--buffer)
  ;; edit the master file or proceed to training
  (if review
      ;; we've been requested to review the master file
      (progn
        (refuse--update-master-file)
        (refuse--put :context 'edit-master)
        (refuse--edit-xml))
    ;; start the training process otherwise
    (refuse--update-master-file)
    (message "Training anystyle parser model...")
    (when buffer-file-name
      (save-buffer))
    (setq buffer-file-name nil)
    (erase-buffer)
    (refuse--put :context 'train)
    (refuse--refresh-mode 'train)
    (insert (format "\
This can take several minutes depending on the size of your training set.
You can continue your work meanwhile and return here later.\n
Training set => %s
Parser model => %s\n
anystyle output:
=====================\n"
                    refuse-anystyle-parser-training-set
                    (or refuse-anystyle-parser-model "none")))
    (goto-char (point-min))
    ;; normally, anystyle runs with `shell-command', anystyle train, however,
    ;; can take minutes on large files, so it runs in a shell sub-process
    (let ((training-process
           (refuse-anystyle 'train
             :stdout t
             :overwrite t
             :input refuse-anystyle-parser-training-set
             :output refuse-anystyle-parser-model
             :buffer refuse--buffer)))
      (refuse--put :training-process training-process)
      ;; finalize
      (set-process-sentinel
       training-process
       (lambda (_p result)
         (refuse-with-buffer
           (if (string= result "finished\n")
               (refuse-with-buffer
                 (goto-char (point-max))
                 (insert "=====================\n\nDone!\n\n")
                 (if refuse-anystyle-parser-model
                     (insert (format "Parser model update: %s"
                                          refuse-anystyle-parser-model))
                   (insert
                    (format "Parser model created: %s\n"
                            (concat
                             (or (file-name-directory
                                  refuse-anystyle-parser-training-set)
                                 (file-name-as-directory
                                  refuse-anystyle-user-directory))
                             "parser.mod"))
                    "To use the model, \
set `refuse-anystyle-parser-model' variable to the above path."))
                 (message "Training anystyle parser model...done")
                 (refuse--put :context 'finished
                                        :training-process nil)
                 (refuse--refresh-mode 'train))
             (refuse--put :context 'error
                                    :training-process nil))))))))


;; ============================================================================
;;; Helper functions: Export of extracted references
;; ============================================================================

(defun refuse--export-get-point (type placement)
  "In current buffer, go to the point where data should be placed.
TYPE is target type, one of the symbols `txt', `bib' or `org'.
PLACEMENT is placement type, one of the symbols `append' or `prepend'.

Return the point."
  ;; for Org export go to the first or last heading, for BibTeX export
  ;; go to the first or last entry rather than the beginning or end of
  ;; buffer, respectively.
  (cl-case placement
    ('prepend
     (cl-case type
       (bib
        (let ((bibtex-sort-ignore-string-entries t))
          (bibtex-beginning-of-first-entry)))
       (org
        (goto-char (point-min))
        (when (org-before-first-heading-p)
          (org-get-next-sibling)))
       (t
        (goto-char (point-min)))))
    (t
     (cl-case type
       (bib
        (let ((bibtex-sort-ignore-string-entries t))
          (bibtex-end-of-entry)))
       (org
        (goto-char (point-max))
        (org-end-of-subtree)
        (forward-line))
       (t
        (goto-char (point-max))))))
  (point))

(defun refuse--export-insert-temp-data (type properties)
  "Insert data from temporary file at point.
TYPE is type of data.  PROPERTIES are additional export properties."
  (let* ((temp-file (refuse--get
                     (intern (format ":temp-%s" type))))
         (filter (plist-get properties :filter-bib-entries))
         ;; inline subroutine to filter BibTeX entries
         (insert-filtered-bib-entries
          (lambda (temp-file filter)
            (when (symbolp filter)
              (setq filter (symbol-value filter)))
            (let ((sources (cond
                            ((stringp filter) (list filter buffer-file-name))
                            ((listp filter) (append filter buffer-file-name))
                            (t (list buffer-file-name))))
                  keys buf-data)
              (save-excursion
                (dolist (source sources)
                  (when source
                    (let ((buffer-visisted-p (find-buffer-visiting source)))
                      (find-file source)
                      (when (eq major-mode 'bibtex-mode)
                        (maphash (lambda (key _val)
                                   (push key keys))
                                 (car (funcall refuse-parse-buffer-function))))
                      (unless buffer-visisted-p
                        (kill-buffer (current-buffer)))))))
              (with-temp-buffer
                (insert-file-contents temp-file)
                (goto-char (point-min))
                (let ((bibtex-sort-ignore-string-entries t))
                  (bibtex-set-dialect 'biblatex t)
                  (bibtex-map-entries
                   (lambda (key _beg _end)
                     (when (member key keys)
                       (bibtex-kill-entry)))))
                (setq buf-data (refuse-buffer-string)))
              (insert buf-data)))))
    (cl-case type
      (bib
       (if filter
           (funcall insert-filtered-bib-entries temp-file filter)
         (insert-file-contents temp-file)))
      (t
       (insert-file-contents temp-file)))))

(defun refuse--export-to-heading (type name properties)
  "Description TYPE NAME PROPERTIES."
  (let ((drawer-props (plist-get properties :property-drawer))
        (placement (plist-get properties :placement))
        (end (make-marker))
        beg data)
    ;; Make the heading in a temporary buffer
    (with-temp-buffer
      ;; get the desired position
      ;; insert parent heading
      ;; NOTE: Investigate
      ;; The behaviour of org-insert-heading has changed at some point:
      ;; If in an empty buffer, e.g. temp-buffer, the function fails messaging "beginning of buffer"
      (org-N-empty-lines-before-current 1)
      (org-insert-heading nil nil t)
      (insert name)
      ;; insert properties
      (dolist (prop drawer-props)
        (let ((prop-name (or (car-safe prop) prop))
              (value (cdr-safe prop))
              prop-value)
          (cond
           ;; call user function if provided
           ((functionp value)
            (setq prop-value (funcall value))
            (unless (stringp prop-value)
              (user-error "Function %s must return a string.  \
Check `refuse-export-options'" value)))
           ;; provide some values for select properties - if the name was
           ;; specified but not a value;
           ;; NOTE: rather a placeholder for future elaboration
           ((null value)
            (cond
             ((string= "REFUSE_TYPE" prop-name)
              (setq prop-value (format "%s" type)))
             ((string= "REFUSE_SOURCE" prop-name)
              (setq prop-value
                    (f-filename (refuse--get :pdf-file))))
             ((string= "REFUSE_DATE" prop-name)
              (setq prop-value (org-timestamp-format
                                (org-timestamp-from-time
                                 (current-time) 'with-time)
                                "%Y-%m-%d %a %H:%M")))))
           ;; insert the user value
           (t (setq prop-value value)))
          ;; insert the property
          (org-set-property prop-name prop-value)))
      (org-end-of-meta-data)
      (insert "\n")
      (setq beg (point))
      (set-marker end beg)
      (set-marker-insertion-type end t)
      (refuse--export-insert-temp-data type properties)
      ;; do some type-specific stuff
      ;;
      ;; Org: demote group headings which are to become subheadings of the
      ;; newly created heading.
      ;;
      ;; BibTeX: insert into a language source block
      (cl-case type
        (org
         (org-mode)
         (goto-char beg)
         (while (re-search-forward org-heading-regexp nil t)
           (org-demote)))
        (bib
         (goto-char beg)
         (insert "#+begin_src bibtex\n")
         (goto-char end)
         (insert "#+end_src\n")))
      (setq data (refuse-buffer-string))
      (set-marker end nil))
    ;; insert the data
    (refuse--export-get-point 'org placement)
    (insert data "\n")))

(defun refuse--export-to-file (type location properties)
  "Export data generated by Refuse to a file.
TYPE is a symbol identifying type of data to be exported, one of
`org', `txt', or `bib'.

LOCATION is a string specifying the location of the target file.
It can be a relative or an absolute file path.  If the file does
not exist, it will be created.  It can also be a relative or an
absolute path to an existing directory.  In this case the data
will be exported to a file in that directory with the citation
key associated with the buffer of origin (extracted from its
#+ROAM_KEY: property) as the filename and TYPE as the extension.

PROPERTIES is a property list with additional export properties.
See `refuse-export-options' for details."
  (let* ((current-dir (file-name-directory
                       (buffer-file-name
                        (refuse--get :original-buffer))))
         (current-key
          (refuse--get :current-key))
         ;; this is a sort of cond, but execute all clauses sequentially
         (path (--> location
                    ;; if location is non-nil and it is a relative filename,
                    ;; expand it within the original buffer's directory
                    (when it
                      (if (f-relative? it) (f-join current-dir it) it))
                    ;; if location is nil assume current directory
                    (if (null it) current-dir it)
                    ;; if location is a directory, make a file with citekey as
                    ;; the file name and type as the extension the location
                    ;; otherwise return the location
                    (if (f-dir? it)
                        (f-join it (format "%s.%s" current-key type))
                      it)))
         ;; file extension if any
         (ext (f-ext path))
         (buffer-visited-p (find-buffer-visiting path))
         target-type buf)
      (find-file path)
      (setq buf (current-buffer))
      ;; type of the target file; try to determine it from the major mode;
      ;; assume TYPE otherwise.
      (setq target-type
            (pcase major-mode
              ('org-mode 'org)
              ('bibtex-mode 'bib)
              ((or 'text-mode 'fundamental-mode) 'txt)
              (_ type)))
      (save-mark-and-excursion
        (pcase (plist-get properties :placement)
          ('prepend
           (refuse--export-get-point target-type 'prepend)
           (refuse--export-insert-temp-data type properties)
           (when (memq type '(org txt))
             (insert "\n")))
          (`(heading ,headline . ,heading-properties)
           (if (string= ext "org")
               ;; NOTE: heading-properties take precendence over path
               ;; properties
               (refuse--export-to-heading
                type headline (append heading-properties properties))
             (user-error "Heading placement only possible in ORG files")))
          ;; defaults to append
          (_
           (refuse--export-get-point target-type 'append)
           (when (memq type '(bib txt))
             (insert "\n"))
           (refuse--export-insert-temp-data type properties))))
      (save-buffer buf)
    (unless buffer-visited-p
      (kill-buffer buf))))

(defun refuse--export (type)
  "Export the extracted and/or generated data.
TYPE is a symbol identifying type of data to be exported, one
of `txt', `bib' or `org'.

The user variable `refuse-export-options' controls
export options."
  ;; there may be several targets for a given TYPE, export to all of them
  (cl-loop
   for (target location . properties)
   in (cdr (assoc type refuse-export-options))
   do (cl-case target
        (heading
         (refuse--export-to-heading type location properties))
        (path
         (refuse--export-to-file type location properties)))))

(defun refuse--checkout ()
  "Finalize Refuse process.
Insert the extracted and generated data according to the settings
of `refuse-export-options'."
  (cl-case (refuse--get :context)
    ('start
     (pop-to-buffer (refuse--get :original-buffer))
     ;; export the extracted/generated data
     (dolist (type (mapcar #'car refuse-export-options))
       (refuse--export type))
     ;; NOTE: "break point" for ease of debugging
     ;; (user-error "Halt")
     (refuse-dispatcher 'kill))
    (t
     (refuse-dispatcher 'error))))

(defun refuse--cleanup ()
  "Clean up before and after Refuse process."
  (setq refuse--refs ())
  (dolist (prop (list :running :callee :context :caller
                      :current-key :prevent-concurring
                      :temp-txt :temp-bib :temp-org :temp-xml
                      :pdf-file :global-bib :master-backup
                      :txt-undo-list :bib-undo-list :org-undo-list
                      :training-process :window-conf :original-buffer))
    (refuse--put prop nil)))


;; ============================================================================
;;; Minor mode
;; ============================================================================

;;; Code in this section was adopted from org-capture.el
(defvar refuse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-k" #'refuse-kill)
    (define-key map [remap save-buffer] #'refuse-save)
    (define-key map [remap write-file] #'refuse-save-as)
    map)
  "Keymap for `refuse-mode' minor mode.
The keymap is updated automatically according to the Refuse
process context.  It is not supposed to be modified directly by
user." )

(defcustom refuse-mode-hook nil
  "Hook for the `refuse-mode' minor mode."
  :type 'hook
  :group 'refuse)

(define-minor-mode refuse-mode
  "Minor mode for special key bindings in a refuse buffer.
Turning on this mode runs the normal hook `refuse-mode-hook'."
  :init-value nil
  :lighter : " RFM"
  (when refuse-mode
    (refuse--update-keymap)
    (setq-local
     header-line-format
     (refuse--format-header-line))))

(defun refuse--put (&rest props)
  "Add properties PROPS to `refuse--plist'.
Returns the new plist."
  (while props
    (setq refuse--plist
          (plist-put refuse--plist
                     (pop props)
                     (pop props)))))

(defun refuse--get (prop)
  "Get PROP from `refuse--plist'."
  (plist-get refuse--plist prop))
;;;
;;; End of code adopted from org-capture.el


;; TODO combine `refuse--format-header-line'
;; and `refuse--update-keymap' into one
;; function and use a macro to generate each entry
(defun refuse--format-header-line ()
  "Return formatted buffer header line depending on context."
  (substitute-command-keys
   (format "\\<refuse-mode-map>Refuse: %s.  %s"
           (refuse--get :current-key)
           (cl-case (refuse--get :caller)
             ('edit-txt
              "\
Generate BibTeX `\\[refuse-dispatcher]', \
sanitize text `\\[refuse-sanitize-text]', \
train parser `\\[refuse-training-session]', \
abort `\\[refuse-kill]'.")
             ('edit-bib
              "\
Generate Org `\\[refuse-dispatcher]', \
generate keys `\\[refuse-generate-keys]', \
return to text `\\[refuse-cancel]', \
train parser `\\[refuse-training-session], \
abort `\\[refuse-kill]'.")
             ('edit-org
              "\
Finish `\\[refuse-dispatcher]', \
return to BibTeX `\\[refuse-cancel]', \
abort `\\[refuse-kill]'.")
             ('edit-xml
              (cl-case (refuse--get :context)
                ('edit
                 (format "\
Train `\\[refuse-training-session]', \
review %s `\\[refuse-review-master-file]', \
cancel `\\[refuse-cancel], \
abort `\\[refuse-kill]'."
                      (file-name-nondirectory
                       refuse-anystyle-parser-training-set)))
                ('edit-master
                 "\
Train `\\[refuse-training-session]', \
cancel `\\[refuse-cancel], \
abort `\\[refuse-kill]'.")))
             ('train
              (cl-case (refuse--get :context)
                ('train
                 "\
Abort `\\[refuse-kill]'.")
                ('continue
                 "\
Finish `\\[refuse-dispatcher]', \
abort `\\[refuse-kill]'.")))
             (t
              "\
Press the RED button `\\[refuse-kill]'.")))))

(defun refuse--update-keymap ()
  "Update `refuse-mode-map' according to current editing mode.
Context is read from `refuse--plist' property `:context'."
  (let ((map refuse-mode-map))
    (cl-case (refuse--get :caller)
      ;;
      ('edit-txt
       (define-key map "\C-c\C-c" #'refuse-dispatcher)
       (define-key map "\C-c\C-u" #'refuse-sanitize-text)
       (define-key map "\C-C\C-t" #'refuse-training-session)
       (define-key map "\C-c\C-r" nil))
      ;;
      ('edit-bib
       (define-key map "\C-c\C-c" #'refuse-dispatcher)
       (define-key map "\C-c\C-u" #'refuse-generate-keys)
       (define-key map "\C-C\C-t" #'refuse-training-session)
       (define-key map "\C-c\C-r" #'refuse-cancel))
      ;;
      ('edit-org
       (define-key map "\C-c\C-c" #'refuse-dispatcher)
       (define-key map "\C-c\C-u" nil)
       (define-key map "\C-C\C-t" nil)
       (define-key map "\C-c\C-r" #'refuse-cancel))
      ('edit-xml
       (cl-case (refuse--get :context)
         ('edit
          (define-key map "\C-c\C-c" #'refuse-training-session)
          (define-key map "\C-c\C-u" nil)
          (define-key map "\C-C\C-t" #'refuse-review-master-file)
          (define-key map "\C-c\C-r" #'refuse-cancel))
         ('edit-master
          (define-key map "\C-c\C-c" #'refuse-training-session)
          (define-key map "\C-c\C-u" nil)
          (define-key map "\C-C\C-t" nil)
          (define-key map "\C-c\C-r" #'refuse-cancel))))
      ('train
       (cl-case (refuse--get :context)
         ('train
          (define-key map "\C-c\C-c" nil)
          (define-key map "\C-c\C-r" nil)
          (define-key map "\C-c\C-u" nil)
          (define-key map "\C-c\C-t" nil))
         ('continue
          (define-key map "\C-c\C-c" #'refuse-dispatcher))))
      (t
       (define-key map "\C-c\C-u" nil)
       (define-key map "\C-c\C-t" nil)
       (define-key map "\C-c\C-r" nil)))))


;; ============================================================================
;;; Interactive functions
;; ============================================================================

(defun refuse-generate-keys (&optional at-point collect-only)
  "Generate BibTeX citation keys in the current buffer.
\\<refuse-mode-map>
While the Refuse interactive process, when editing
BibTeX data, press \\[refuse-generate-keys] to generate
citation keys using the function specified in
`refuse-keygen-function'.  When called interactively
with a \\[universal-argument] prefix argument AT-POINT, generate
key only for the record at point.

When called from Lisp, if optional COLLECT-ONLY is non-nil, do
not generate the key and update the records, just collect records
for future use."
  (interactive "P")
  (refuse-with-progress "Generating citation keys"
    (let ((bibtex-help-message nil)
          (bibtex-contline-indentation 2)
          (bibtex-text-indentation 2))
      (save-excursion
        (if (equal at-point '(4))
            ;; generate key at point
            (progn
              (bibtex-beginning-of-entry)
              (let* ((old-key (save-excursion
                                (re-search-forward
                                 bibtex-entry-maybe-empty-head)
                                (bibtex-key-in-head)))
                     (old-ref (assoc old-key refuse--refs))
                     (new-ref (refuse--update-record-at-point
                               collect-only)))
                (if old-ref
                    (setf (car old-ref) (car new-ref)
                          (cdr old-ref) (cdr new-ref))
                  (cl-pushnew new-ref refuse--refs :test 'equal))))
          ;; generate keys in the buffer otherwise
          (let ((refs ())
                (natural-order 1))
            (goto-char (point-min))
            (bibtex-skip-to-valid-entry)
            (while (not (eobp))
              (cl-pushnew (refuse--update-record-at-point
                           collect-only (format "%s" natural-order))
                          refs)
              (bibtex-skip-to-valid-entry)
              (setq natural-order (1+ natural-order)))
            (setq refuse--refs refs)))))
    (write-region (refuse-buffer-string) nil
                  (refuse--get :temp-bib) nil -1)
    (set-buffer-modified-p nil)))

(defun refuse-sanitize-text (&optional contents)
  "Run string processing in current buffer.
Try to get every reference onto newline.  Return this buffer's
contents (`refuse-buffer-string').

If optional string CONTENTS was specified, run processing on this
string instead.  Return modified CONTENTS."
  (interactive)
  (let* ((rx1 '(and "(" (** 1 2 (any "0-9")) ")"))
         (rx2 '(and "[" (** 1 2 (any "0-9")) "]"))
         (rx3 '(and "(" (any "a-z") (opt (any space)) ")"))
         (rx4 '(and " " (any "a-z") ")"))
         (regexp (rx-to-string
                  `(group-n 1 (or (or (and ,rx1 " " ,rx3)
                                      (and ,rx2 " " ,rx3))
                                  (or (and ,rx1 " " ,rx4)
                                      (and ,rx2 " " ,rx4))
                                  (or ,rx1 ,rx2)
                                  (or ,rx3 ,rx4))) t)))
    (if contents
        (--> contents
             (s-replace "\n" " " it)
             (s-replace-regexp regexp "\n\\1" it))
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
        (replace-match " " nil nil))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match "\n\\1" nil nil))
      (goto-char (point-min))
      (refuse-buffer-string))))

(defun refuse-training-session (&optional context)
  "Run training session subroutines depending on CONTEXT.
If context is not provided, it will be read from
`refuse--plist''s `:context'."
  (interactive)
  (pop-to-buffer refuse--buffer)
  (let ((context (or context (refuse--get :context))))
    (refuse--put :context context)
    (cl-case context
      ('start
       ;; generate xml
       (refuse--edit-xml))
      ((edit edit-master)
       (refuse--train nil))
      ('finished
       (refuse-dispatcher 'edit-txt 'continue))
      (t (refuse-dispatcher 'error)))))

(defun refuse-review-master-file ()
  "Review parser training set (master file)."
  (interactive)
  (refuse--train t))

(defun refuse-dispatcher (&optional callee context)
  "Call Refuse subroutine CALLEE in context CONTEXT.
CALLEE and CONTEXT can be passed directly as optional variables,
or they will be read from `refuse--plist''s
respectively `:collee' and `:context' properties.

Recognized CALLEEs are:
==========
'edit-txt - `refuse--edit-txt'
'edit-bib - `refuse--edit-bib'
'edit-org - `refuse--edit-org'
'train    - `refuse-training-session'
'checkout - `refuse--checkout'

Passing or setting any other CALLEE will kill the process.

This function also checks `:prevent-concurring' property in
`refuse--plist' and will suggest to restart the process
if its value is non-nil."
  ;; TODO: check for whether the user killed any of the buffers
  (interactive)
  (let ((callee (or callee (refuse--get :callee)))
        (context (or context (refuse--get :context))))
    ;; in case context was passed as an argument
    (refuse--put :callee callee
                           :context context)
    (if
        ;; Prevent another Refuse process from running
        ;; Ask user whether to kill the currently running process
        (refuse--get :prevent-concurring)
        (if (y-or-n-p
             (format "Another Refuse process is running: %s.  \
Kill it and start a new one %s? "
                     (refuse--get :current-key)
                     (refuse--get :new-key)))
            ;; Kill the process and start a new one
            (progn
              (refuse-with-progress "Killing current process"
                (refuse--cleanup))
              (refuse-run (refuse--get :new-key)))
          ;; go to the Scrapper buffer
          (pop-to-buffer refuse--buffer)
          ;; reset the concurring flag set by `refuse-run'
          (refuse--put :prevent-concurring nil))
      ;; Finilize the requested context otherwise
      (cl-case callee
        ('edit-txt
         (refuse--edit-txt))
        ('edit-bib
         (refuse--edit-bib))
        ;; edit org
        ('edit-org
         (refuse--edit-org))
        ('checkout
         ;; currently, this is unnecessary but may be useful
         ;; if some recovery options are implemented
         (refuse-with-buffer
           (write-region (refuse-buffer-string)
                         nil (refuse--get :temp-org) nil 1))
         (refuse--checkout))
        (t
         ;; 1 in 100 should not be too annoying
         (when (> (cl-random 100) 98)
           (message "Oops...")
           (sleep-for 1)
           (message "Oops...Did you just ACCIDENTALLY press the RED button?")
           (sleep-for 1)
           (message "Activating self-destruction subroutine...")
           (sleep-for 1)
           (message "Activating self-destruction subroutine...Bye-bye")
           (sleep-for 1))
         (let ((kill-buffer-query-functions nil))
           (and (get-buffer refuse--buffer)
                (kill-buffer refuse--buffer)))
         (set-window-configuration (refuse--get :window-conf))
         (refuse--cleanup))))))

(defun refuse-cancel ()
  "Discard edits and return to previous editing mode."
  (interactive)
  (cl-case (refuse--get :caller)
    ('edit-bib
     (refuse-with-buffer
       (refuse--put :bib-undo-list nil))
     (refuse-dispatcher 'edit-txt 'continue))
    ('edit-org
     (refuse-dispatcher 'edit-bib 'continue))
    ('edit-xml
     (when-let ((master-backup (refuse--get :master-backup)))
       (rename-file master-backup refuse-anystyle-parser-training-set t)
       (setq buffer-file-name nil))
     (refuse-dispatcher (refuse--get :callee) 'continue))
    (t
     (refuse-dispatcher 'error))))

(defun refuse-kill ()
  "Kill the interactive Refuse process."
  (interactive)
  (when-let (process (refuse--get :training-process))
    (kill-process process))
  (refuse-dispatcher 'kill))

(defun refuse-save ()
  "Save current Refuse buffer in the respective temp file.
This command shadows `save-buffer' when `refuse-mode' is active."
  (interactive)
  (let ((temp-file
         (cl-case (refuse--get :caller)
           ('edit-txt (refuse--get :temp-txt))
           ('edit-bib (refuse--get :temp-bib))
           ('edit-org (refuse--get :temp-org))
           ('edit-xml (refuse--get :temp-xml))
           (t nil))))                   ; fallback flag
    (cond
     ;; Refuse buffers do not have file names
     ((and (not buffer-file-name) temp-file)
      (write-region (refuse-buffer-string) nil temp-file nil -1)
      (set-buffer-modified-p nil))
     ((save-buffer)))))

(defun refuse-save-as ()
  "Export current Refuse buffer to a file.
This command shadows `write-file' when `refuse-mode' is active."
  (interactive)
  ;; Refuse buffers do not have file names
  (cond
   ((not buffer-file-name)
    (call-interactively #'write-file)
    (set-visited-file-name nil)
    (rename-buffer refuse--buffer))
   ((call-interactively #'write-file))))


;; ============================================================================
;;; Entry point
;; ============================================================================

;;;###autoload
(defun refuse-run (key)
  "Run Refuse interactive process.
KEY is note's citation key."
  (interactive)
  (if (refuse--get :running)
      (progn
        (refuse--put :prevent-concurring t
                               :new-key key)
        (refuse-dispatcher))
    ;; in case previous process was not killed properly
    (refuse--cleanup)
    (refuse--put :callee 'edit-txt
                           :context 'start
                           :caller 'run
                           :current-key key
                           :new-key nil
                           :pdf-file (file-truename
                                      (orb-get-attached-file key))
                           :running t
                           :prevent-concurring nil
                           :global-bib bibtex-completion-bibliography
                           :original-buffer (current-buffer)
                           :window-conf (current-window-configuration))
    (refuse-dispatcher)))

(provide 'refuse)

;;; refuse.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
