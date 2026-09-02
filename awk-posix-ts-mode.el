;;; awk-posix-ts-mode.el --- Tree-sitter mode for POSIX awk  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/awk-posix-ts-mode
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Tree-sitter major mode for POSIX awk.

;;; Code:

(require 'treesit)

(defgroup awk-posix-ts nil
  "Tree-sitter mode for POSIX awk."
  :group 'languages)

(defconst awk-posix-ts-mode--grammar-sources
  '((posix-awk "https://github.com/konomanoasa/tree-sitter-posix-awk"
               :revision "v0.7.0"))
  "Tree-sitter grammar sources for POSIX awk.")

;;;; Context

(defun awk-posix-ts-mode--ancestor-p (node type)
  "Return non-nil when NODE has an ancestor of TYPE."
  (let (found)
    (while (and node (not found))
      (when (equal (treesit-node-type node) type)
        (setq found t))
      (setq node (treesit-node-parent node)))
    found))

(defun awk-posix-ts-mode--delimiter-p (node)
  "Return non-nil when NODE is a structural delimiter."
  (let* ((type (treesit-node-type node))
         (parent (treesit-node-parent node))
         (parent-type (and parent (treesit-node-type parent))))
    (and parent
         (not (equal parent-type "ERROR"))
         (not (awk-posix-ts-mode--ancestor-p parent "ere"))
         (or (not (member type '("{" "}")))
             (equal parent-type "action")))))

;;;; Syntax

(defvar awk-posix-ts-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?\" ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `awk-posix-ts-mode'.")

(defvar awk-posix-ts-mode-syntax--query-cache nil
  "Cached syntax query.")

;;;;; Syntax Queries

(defun awk-posix-ts-mode-syntax--query ()
  "Return the cached syntax query."
  (or awk-posix-ts-mode-syntax--query-cache
      (setq awk-posix-ts-mode-syntax--query-cache
            (treesit-query-compile
             'posix-awk
             '((comment) @comment
               ((_ ["(" ")" "[" "]" "{" "}"] @delimiter)
                (:pred awk-posix-ts-mode--delimiter-p @delimiter)))
             t))))

(defun awk-posix-ts-mode-syntax--captures (parser &optional start end)
  "Return syntax captures from PARSER between START and END."
  (with-current-buffer (treesit-parser-buffer parser)
    (save-restriction
      (widen)
      (let ((start (or start (point-min)))
            (end (or end (point-max)))
            (query (awk-posix-ts-mode-syntax--query))
            captures)
        (dolist (capture
                 (treesit-query-capture
                  (treesit-parser-root-node parser)
                  query start end))
          (let ((node (cdr capture)))
            (push (list (car capture)
                        (treesit-node-start node)
                        (treesit-node-end node))
                  captures)))
        (sort captures
              (lambda (left right) (< (nth 1 left) (nth 1 right))))))))

;;;;; Propertization

(defun awk-posix-ts-mode-syntax--delimiter-syntax (position)
  "Return syntax-table syntax for the delimiter at POSITION."
  (pcase (char-after position)
    (?\( (string-to-syntax "()"))
    (?\) (string-to-syntax ")("))
    (?\[ (string-to-syntax "(]"))
    (?\] (string-to-syntax ")["))
    (?{ (string-to-syntax "(}"))
    (?} (string-to-syntax "){"))))

(defun awk-posix-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (remove-text-properties (point-min) start '(syntax-table nil))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (dolist (capture (awk-posix-ts-mode-syntax--captures
                        treesit-primary-parser start end))
        (let* ((name (car capture))
               (position (if (eq name 'comment)
                             (nth 1 capture)
                           (1- (nth 2 capture)))))
          (put-text-property
           position (1+ position) 'syntax-table
           (if (eq name 'comment)
               (string-to-syntax "<")
             (awk-posix-ts-mode-syntax--delimiter-syntax position))))))))

;;;;; Setup

(defun awk-posix-ts-mode-syntax-setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'awk-posix-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[[:blank:]]*")
  (setq-local comment-use-syntax t))

;;;; Font Lock

;;;;; Features

(defconst awk-posix-ts-mode-font-lock--feature-list
  '((comment)
    (function parameter builtin keyword string variable)
    (number escape)
    (operator delimiter punctuation bracket regexp))
  "Font-lock features by decoration level.")

;;;;; Settings

(defun awk-posix-ts-mode-font-lock--settings ()
  "Return the font-lock settings."
  (treesit-font-lock-rules
   :default-language 'posix-awk

   :feature 'comment
   '((comment) @font-lock-comment-face)

   :feature 'function
   '((item
      name: [(name) (func_name)] @font-lock-function-name-face)
     (func_name) @font-lock-function-call-face)

   :feature 'parameter
   '((param_list
      (name) @font-lock-variable-name-face))

   :feature 'builtin
   '((builtin_func_name) @font-lock-builtin-face)

   :feature 'keyword
   '([(begin_keyword) (break_keyword) (continue_keyword) (delete_keyword)
      (do_keyword) (else_keyword) (end_keyword) (exit_keyword) (for_keyword)
      (function_keyword) (getline_keyword) (if_keyword) (in_keyword)
      (next_keyword) (nextfile_keyword) (print_keyword) (printf_keyword)
      (return_keyword) (while_keyword)]
     @font-lock-keyword-face)

   :feature 'escape
   '((string
      (escape_sequence) @font-lock-escape-face))

   :feature 'string
   :override 'keep
   '((string "\"" @font-lock-string-face)
     (string_content) @font-lock-string-face
     (string
      (escape_sequence) @font-lock-string-face))

   :feature 'variable
   '((name) @font-lock-variable-use-face)

   :feature 'number
   '((number) @font-lock-number-face)

   :feature 'delimiter
   '((ere "/" @font-lock-delimiter-face))

   :feature 'regexp
   '((quoted_character) @font-lock-escape-face
     (ordinary_character) @font-lock-regexp-face
     (collating_element) @font-lock-constant-face
     (wildcard "." @font-lock-constant-face)
     (class_name) @font-lock-constant-face
     (meta_character) @font-lock-regexp-face
     (dup_count) @font-lock-number-face
     (start_range "-" @font-lock-operator-face)
     (range_expression "-" @font-lock-constant-face)
     (bracket_list "-" @font-lock-constant-face)
     (bracket_expression
      ["[" "]"] @font-lock-bracket-face)
     (nonmatching_list "^" @font-lock-negation-char-face)
     (collating_symbol
      ["[" "]"] @font-lock-bracket-face)
     (collating_symbol
      "." @font-lock-punctuation-face)
     (equivalence_class
      ["[" "]"] @font-lock-bracket-face)
     (equivalence_class
      "=" @font-lock-punctuation-face)
     (character_class
      ["[" "]"] @font-lock-bracket-face)
     (character_class
      ":" @font-lock-punctuation-face)
     (ere_expression
      ["(" ")"] @font-lock-bracket-face)
     (extended_reg_exp
      operator: "|" @font-lock-operator-face)
     [(left_anchor) (right_anchor)] @font-lock-operator-face
     (ere_dupl_symbol
      ["*" "+" "?"] @font-lock-operator-face)
     (ere_dupl_symbol
      ["{" "}"] @font-lock-bracket-face)
     (ere_dupl_symbol
      "," @font-lock-punctuation-face)
     (repetition_modifier "?" @font-lock-operator-face))

   :feature 'regexp
   :override t
   '((ordinary_character
      (escape_sequence) @font-lock-escape-face)
     (one_char_or_coll_elem_ere
      (escape_sequence) @font-lock-escape-face)
     (collating_element
      (escape_sequence) @font-lock-escape-face)
     (collating_symbol
      (meta_character) @font-lock-constant-face)
     (collating_symbol
      (collating_element) @font-lock-constant-face)
     (equivalence_class
      (collating_element) @font-lock-constant-face)
     (escaped_delimiter) @font-lock-escape-face)

   :feature 'operator
   '([(add_assign) (and) (append) (decr) (div_assign)
      (eq) (ge) (incr) (le) (mod_assign) (mul_assign)
      (ne) (no_match) (or) (pow_assign) (sub_assign)
      "!" "$" "%" "*" "+" "/" ":" "<" "=" ">"
      "?" "^" "|" "~" "-"]
     @font-lock-operator-face)

   :feature 'punctuation
   '(["," ";"] @font-lock-punctuation-face
     (line_continuation) @font-lock-punctuation-face)

   :feature 'bracket
   '(((["(" ")" "[" "]" "{" "}"]
       @font-lock-bracket-face)
      (:pred awk-posix-ts-mode--delimiter-p @font-lock-bracket-face)))))

;;;;; Setup

(defun awk-posix-ts-mode-font-lock-setup ()
  "Configure font locking for the current buffer."
  (setq-local treesit-font-lock-feature-list
              awk-posix-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (awk-posix-ts-mode-font-lock--settings)))

;;;; Navigation

(defconst awk-posix-ts-mode--item-regexp
  "^item$"
  "Regexp matching POSIX awk items.")

(defun awk-posix-ts-mode--function-item-p (node)
  "Return non-nil when NODE is a POSIX awk function item."
  (and (treesit-node-match-p node awk-posix-ts-mode--item-regexp)
       (treesit-node-child-by-field-name node "name")
       (treesit-node-child-by-field-name node "body")))

(defconst awk-posix-ts-mode-thing-settings
  `((posix-awk
     (sexp ,awk-posix-ts-mode--item-regexp)
     (defun (,awk-posix-ts-mode--item-regexp
             . awk-posix-ts-mode--function-item-p))))
  "Tree-sitter thing definitions for POSIX awk.")

(defun awk-posix-ts-mode-navigation-setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings awk-posix-ts-mode-thing-settings))

;;;; Imenu

(defconst awk-posix-ts-mode-imenu-settings
  `((nil ,awk-posix-ts-mode--item-regexp awk-posix-ts-mode--function-item-p nil))
  "Tree-sitter Imenu settings for POSIX awk.")

(defun awk-posix-ts-mode--defun-name (node)
  "Return the name of the function item NODE."
  (when (awk-posix-ts-mode--function-item-p node)
    (let ((name (treesit-node-child-by-field-name node "name")))
      (when (member (treesit-node-type name) '("name" "func_name"))
        (treesit-node-text name t)))))

(defun awk-posix-ts-mode-imenu-setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function #'awk-posix-ts-mode--defun-name)
  (setq-local treesit-simple-imenu-settings awk-posix-ts-mode-imenu-settings))

;;;; Indentation

(defcustom awk-posix-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'awk-posix-ts)

(defconst awk-posix-ts-mode-indent-rules
  '((posix-awk
     ((node-is "}") parent-bol 0)
     ((node-is "else_keyword") parent-bol 0)
     ((node-is "while_keyword") parent-bol 0)
     ((field-is "body") parent-bol awk-posix-ts-mode-indent-offset)
     ((field-is "consequence") parent-bol awk-posix-ts-mode-indent-offset)
     ((field-is "alternative") parent-bol awk-posix-ts-mode-indent-offset)
     ((parent-is "terminated_statement_list") first-sibling 0)
     ((parent-is "unterminated_statement_list") first-sibling 0)
     ((parent-is "program") column-0 0)
     ((parent-is "item_list") column-0 0)))
  "Tree-sitter indentation rules for POSIX awk.")

(defun awk-posix-ts-mode-indent-setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              awk-posix-ts-mode-indent-rules))

;;;; Mode

(defun awk-posix-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language awk-posix-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (treesit-ensure-installed language)))

(defun awk-posix-ts-mode--setup ()
  "Configure `awk-posix-ts-mode' in the current buffer."
  (unless (awk-posix-ts-mode--ensure-grammar 'posix-awk)
    (user-error "Tree-sitter grammar `posix-awk' is unavailable"))
  (setq-local treesit-primary-parser (treesit-parser-create 'posix-awk))
  (awk-posix-ts-mode-syntax-setup)
  (awk-posix-ts-mode-font-lock-setup)
  (awk-posix-ts-mode-navigation-setup)
  (awk-posix-ts-mode-imenu-setup)
  (awk-posix-ts-mode-indent-setup)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode awk-posix-ts-mode prog-mode "Awk-POSIX-TS"
  "Major mode for editing POSIX awk."
  :syntax-table awk-posix-ts-mode-syntax-table
  :group 'awk-posix-ts
  (awk-posix-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.awk\\'" . awk-posix-ts-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("awk" . awk-posix-ts-mode))

(provide 'awk-posix-ts-mode)

;;; awk-posix-ts-mode.el ends here
