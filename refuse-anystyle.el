;;; refuse-anystyle.el --- RefUse: Elisp wrapper for anystyle-cli -*- lexical-binding: t -*-

;; Copyright © 2022 Mykhailo Shevchuk

;; Author: Mykhailo Shevchuk <mail+dev@mshevchuk.com>
;; Created: 05 January 2022

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
;; This library provides a convenience Elisp wrapper for anystyle-cli
;; anystyle-cli must be installed separately: https://github.com/inukshuk/anystyle-cli

;;; Code:
;;
;; This library used to be a part of Org Roam BibTeX (ORB).  Older commits can
;; be tracked here: https://github.com/org-roam/org-roam-bibtex
;;
;; N.B. This file contains code snippets adopted from other open-source
;; projects. These snippets are explicitly marked as such in place. They are
;; not subject to the above copyright and authorship claims.

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'dash)

(require 'cl-lib)
(eval-when-compile
  (require 'subr-x))

;; * Customize definitions

(defcustom refuse-anystyle-executable "anystyle"
  "Anystyle executable path or program name."
  :type '(choice (const "anystyle")
                 (file :tag "Path to executable" :must-match t))
  :group 'refuse-anystyle)

(defcustom refuse-anystyle-pdfinfo-executable nil
  "Path to pdfinfo executable to be passed to anystyle.
When this is nil, anystyle will look for it in the system path."
  :type '(choice
          (file :tag "Path to executable")
          (const nil))
  :group 'refuse-anystyle)

(defcustom refuse-anystyle-pdftotext-executable nil
  "Path to pdftotext executable to be passed to anystyle.
When this is nil, anystyle will look for it in the system path."
  :type '(choice
          (file :tag "Path to executable")
          (const nil))
  :group 'refuse-anystyle)

(defcustom refuse-anystyle-parser-model nil
  "Path to anystyle custom parser model."
  :type '(choice
          (file :tag "Path to file" :must-match t)
          (const :tag "Built-in" nil))
  :group 'refuse-anystyle)

(defcustom refuse-anystyle-finder-model nil
  "Path to anystyle custom finder model."
  :type '(choice
          (file :tag "Path to file" :must-match t)
          (const :tag "Built-in" nil))
  :group 'refuse-anystyle)

;; --crop is currently broken upstream

(defcustom refuse-anystyle-find-crop nil
  "Crop value in pt to be passed to `anystyle find'.
An integer or a conc cell of integers."
  :type '(choice (integer :tag "Top and bottom")
                 (cons :tag "Top, bottom, left and right"
                       (integer :tag "Top and bottom")
                       (integer :tag "Left and right"))
                 (const :tag "Do not crop" nil))
  :group 'refuse-anystyle)

(defcustom refuse-anystyle-find-solo nil
  "Non-nil to pass the `--solo' flag."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'refuse-anystyle)

(defcustom refuse-anystyle-find-layout nil
  "Non-nil to pass the `--layout' flag."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'refuse-anystyle)

(defcustom refuse-anystyle-default-buffer "*Refuse Anystyle Output*"
  "Default buffer name for anystyle output."
  :type 'string
  :group 'refuse-anystyle)

(defcustom refuse-anystyle-user-directory
  (concat (file-name-as-directory user-emacs-directory) "refuse-anystyle")
  "Directory to keep anystyle user files."
  :type 'directory
  :group 'refuse-anystyle)

(defcustom refuse-anystyle-parser-training-set
  (concat (file-name-as-directory refuse-anystyle-user-directory) "core.xml")
  "XML file containing parser training data."
  :type '(file :must-match t)
  :group 'anystyle)

(defcustom refuse-anystyle-finder-training-set
  (expand-file-name (file-name-as-directory refuse-anystyle-user-directory) "ttx/")
  "Directory containing finder training data (.ttx files)."
  :type 'directory
  :group 'anystyle)


;; ============================================================================
;;; Helper functions
;; ============================================================================

(defun refuse-anystyle-format (&rest args)
  "Format ARGS conditionally and return a string.
ARGS must be a plist, whose keys are `format' control strings and
values are `format' objects.  Thus only one object per control
string is allowed.  The result will be concatenated into a single
string.

In the simplest case, it behaves as a sort of interleaved `format':
==========

\(refuse-anystyle-format \"A: %s\" 'hello
            \" B: %s\" 'world
            \" C: %s\" \"!\")

  => 'A: hello B: world C: !'

If format object is nil, it will be formatted as empty string:
==========

\(refuse-anystyle-format \"A: %s\" 'hello
            \" B: %s\" nil
            \" C: %s\" \"!\")
  => 'A: hello C: !'

Object can also be a cons cell.  If its car is nil then its cdr
will be treated as default value and formatted as \"%s\":
==========

\(refuse-anystyle-format \"A: %s\" 'hello
            \" B: %s\" '(nil . dworl)
            \" C: %s\" \"!\")
  => 'A: hellodworl C: !'

Finally, if the control string is nil, the object will be formatted as \"%s\":
==========

\(refuse-anystyle-format \"A: %s\" 'hello
            \" B: %s\" '(nil . \" world\")
             nil \"!\")
=> 'A: hello world!'."
  (let ((res ""))
    (while args
      (let ((str (pop args))
            (obj (pop args)))
        (unless (consp obj)
          (setq obj (cons obj nil)))
        (setq res
              (concat res
                      (format (or (and (car obj) str) "%s")
                              (or (car obj) (cdr obj) ""))))))
    res))


;; ============================================================================
;;; Helper functions
;; ============================================================================

;;;###autoload
(cl-defun refuse-anystyle (command
                        &key (exec refuse-anystyle-executable)
                        verbose help version adapter
                        ((:finder-model fmodel) refuse-anystyle-finder-model)
                        ((:parser-model pmodel) refuse-anystyle-parser-model)
                        (pdfinfo refuse-anystyle-pdfinfo-executable)
                        (pdftotext refuse-anystyle-pdftotext-executable)
                        format stdout overwrite
                        (crop refuse-anystyle-find-crop)
                        (solo refuse-anystyle-find-solo)
                        (layout refuse-anystyle-find-layout)
                        input output
                        (buffer refuse-anystyle-default-buffer))
  "Run anystyle COMMAND with `shell-command'.
ARGS is a plist with the following recognized keys:

Anystyle CLI options
==========
1) EXEC :exec      => string (valid executable)
- default value can be set through `refuse-anystyle-executable'

2) COMMAND :command   => symbol or string
- valid values: find parse help check license train

3) Global options can be passed with the following keys.

FMODEL    :finder-model => string (valid file path)
PMODEL    :parser-model => string (valid file path)
PDFINFO   :pdfinfo      => string (valid executable)
PDFTOTEXT :pdftotext    => string (valid executable)
ADAPTER   :adapter      => anything
STDOUT    :stdout       => boolean
HELP      :help         => boolean
VERBOSE   :verbose      => boolean
VERSION   :version      => boolean
OVERWRITE :overwrite    => boolean
FORMAT    :format       => string, symbol or list of unquoted symbols

- FORMAT must be one or more output formats accepted by anystyle commands:
  parse => bib csl json ref txt xml
  find  => bib csl json ref txt ttx xml
- string must be space- or comma-separated, additional spaces are
  ignored

Default values for some of these options can be set globally via
the following variables: `refuse-anystyle-finder-model',
`refuse-anystyle-parser-model', `refuse-anystyle-pdfinfo-executable',
`refuse-anystyle-pdftotext-executable'.

4) Command options can be passed with the following keys:

CROP   :crop         => integer or cons cell of integers
LAYOUT :layout       => boolean
SOLO   :solo         => boolean

- Command options are ignored for commands other than find
- anystyle help -c flag is not supported

Default values for these options can be set globally via the
following variables: `refuse-anystyle-find-crop',
`refuse-anystyle-find-layout', `refuse-anystyle-find-solo'.

5) INPUT  :input   => string (file path)

6) OUTPUT :output  => string (file path)

`shell-command'-related options
==========

7) BUFFER :buffer  => buffer-or-name

- `shell-command''s OUTPUT-BUFFER
- can be a cons cell (OUTPUT-BUFFER . ERROR-BUFFER)
- when nil, defaults to `refuse-anystyle-default-buffer'

anystyle CLI command synopsis:
anystyle [global options] command [command options] [arguments...].

Homepage: https://anystyle.io
Github: https://github.com/inukshuk/anystyle-cli
Courtesy of its authors."
  (declare (indent 1))
  (let* ((commands '(list find parse check train help license))
         (exec (executable-find exec))
         (buf (if (consp buffer) buffer (list buffer)))
         ;; '(a b c) => "a,b,c"
         (to-string (lambda (str)
                      (--reduce-from
                       (format "%s,%s" acc it)
                       (car str) (cdr str))))
         ;; debug
         ;; (anystyle-run (lambda (str)
         ;;              (message "command: %s \nbuffers: %s and %s" str (car buf) (cdr buf))))
         (anystyle-run (lambda (str)
                         (if (eq command 'train)
                             ;; train can take minutes, so run it in a sub-process
                             (start-process-shell-command
                              "anystyle" (car buf) str)
                           (shell-command str
                                          (car buf) (cdr buf)))))
         global-options command-options anystyle)
    ;; executable is a must
    (unless exec
      (user-error "Anystyle executable not found!  \
Install anystyle-cli before running `refuse-anystyle'"))
    ;; we process :version and :help before checking command
    ;; since with this global flag command is not required
    (cond
     ;; help flag takes priority
     (help
      (setq global-options " --help"
            command-options ""
            input nil
            output nil))
     ;; anystyle ignores everything with --version flag except the
     ;; --help flag, which we've just resolved above
     (version
      (setq global-options "--version"
            command nil
            command-options ""
            input nil
            output nil))
     ;; otherwise command is a must
     ((not command)
      (user-error "Anystyle command required: \
find, parse, check, train, help or license")))
    (when (stringp command)
      (setq command (intern command)))
    ;; command must be a valid command
    (unless (memq command commands)
      (user-error "Invalid command %s.  Valid commands are \
find, parse, check, train, help and license" command))
    ;;
    ;; command specific arguments
    (cl-case command
      ('help
       (when (stringp input)
         (setq input (intern input)))
       (unless (or (and global-options
                        (string= global-options " --help"))
                   (memq input commands))
         (user-error "Invalid input %s.  Valid input for 'anystyle help': \
find, parse, check, train, help or license" input)))
      ('license
       (setq input nil
             output nil
             global-options ""
             command-options ""))
      ('check
       (setq output nil))
      ('find
       ;; pdfinfo and pdftotext must be present in the system
       (when (and pdfinfo (not (executable-find pdfinfo)))
         (user-error "Executable not found: pdfinfo, %s" pdfinfo))
       (when (and pdftotext (not (executable-find pdftotext)))
         (user-error "Executable not found: pdftotext, %s" pdftotext))
       (setq global-options
             (refuse-anystyle-format "%s" global-options
                          " --pdfinfo=\"%s\"" pdfinfo
                          " --pdftotext=\"%s\"" pdftotext))
       ;; Command options
       ;; N.B. Help command accepts a command option -c but it's totally
       ;; irrelevant for us:
       ;;
       ;; [COMMAND OPTIONS]
       ;; -c - List commands one per line, to assist with shell completion
       ;; so we do not implement it
       ;;
       ;; :crop value should be integer; if no value was explicitly supplied,
       ;; use the default from `refuse-anystyle-find-crop'
       (when crop
         (unless (consp crop)
           (setq crop (list crop)))
         (let ((x (car crop))
               (y (or (cdr crop) 0)))
           (unless (and (integerp x)
                        (integerp y))
             (user-error "Invalid value %s,%y.  Number expected" x y))
           (setq crop (format "%s,%s" x y))))
       ;; parse only accepts --[no]-layout, so we ignore the rest
       ;; append command options to command
       (setq command-options
             (refuse-anystyle-format " --crop=%s" crop
                          " --layout" (cons layout " --no-layout")
                          " --solo" (cons solo " --no-solo"))))
      ('train
       (unless output
         (setq output
               (concat (or (file-name-directory refuse-anystyle-parser-training-set)
                           (file-name-as-directory refuse-anystyle-user-directory))
                       "parser.mod")))))
    ;; Arguments relevant for more than one command
    ;;
    ;; find, parse:
    ;; format option should be one of accepted types if present
    (when (and (memq command '(find parse))
               format)
      (when (stringp format)
        (setq format
              (-map #'intern
                    (split-string (string-trim format)
                                  "[, ]" t " "))))
      (unless (listp format)
        (setq format (list format)))
      (let ((accepted-formats
             (cl-case command
               ('find '(bib csl json ref txt ttx xml))
               ('parse '(bib csl json ref txt xml)))))
        (when (--none? (memq it accepted-formats) format)
          (user-error
           "Invalid format(s) %s.  Valid formats for command %s: %s"
           (funcall to-string format)
           command
           (funcall to-string accepted-formats)))
        ;; convert format to a comma-separated string and append
        ;; it to global options
        (setq global-options
              (refuse-anystyle-format "%s" global-options
                           " -f %s" (funcall to-string format)))))
    ;; find, parse, check accept
    ;; finder and parser models
    (when (memq command '(find parse check))
      (when (and fmodel (not (file-exists-p fmodel)))
        (display-warning 'org-roam-bibtex
                         "Finder model file not found: %s, \
using the default one" fmodel)
        (setq fmodel nil))
      (when (and pmodel (not (file-exists-p pmodel)))
        (display-warning 'org-roam-bibtex
                         "Finder model file not found: %s, \
using the default one" pmodel)
        (setq pmodel nil))
      (setq global-options (refuse-anystyle-format "%s" global-options
                                        " -F \"%s\"" fmodel
                                        " -P \"%s\"" pmodel)))
    ;; find, train, parse and check:
    ;; 1) require input, which should be a valid path
    ;; 2) something called ruby adapter, probably a right place here
    ;; 3) --verbose, --stdout, --overwrite if non-nil
    (when (memq command '(find train parse check))
      (unless input
        (user-error "Input required for command %s" command))
      (unless (and (stringp input) (file-exists-p input))
        (user-error "Invalid input file or directory %s" input))
      (setq global-options
            (refuse-anystyle-format
             "%s" global-options
             " --verbose" (cons verbose " --no-verbose")
             ;; this flag does nothing for check
             " --stdout" (cons stdout " --no-stdout")
             " --adapter=\"%s\"" adapter
             " --overwrite" (cons overwrite " --no-overwrite"))))
    ;; Set arguments and run the program
    ;;
    (setq anystyle (refuse-anystyle-format "%s" exec
                                "%s" global-options
                                " %s" command
                                "%s" command-options
                                " \"%s\"" (when input (file-truename input))
                                " \"%s\"" (when output (file-truename output))))
    (funcall anystyle-run anystyle)))

(provide 'refuse-anystyle)

;;; refuse-anystyle.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
