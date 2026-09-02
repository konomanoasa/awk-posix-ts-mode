;;; awk-posix-ts-mode-test.el --- Tests for awk-posix-ts-mode  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'awk-posix-ts-mode)

(defun awk-posix-ts-mode-test--require-grammar ()
  (unless (treesit-ready-p 'posix-awk t)
    (ert-skip "The posix-awk grammar is unavailable")))

(defun awk-posix-ts-mode-test--grammar-source (configured)
  (let ((treesit-language-source-alist configured)
        (ensure (symbol-function 'treesit-ensure-installed))
        source)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (installed-language)
                  (setq source
                        (assq installed-language
                              treesit-language-source-alist))
                  t))
          (should (awk-posix-ts-mode--ensure-grammar 'posix-awk))
          source)
      (fset 'treesit-ensure-installed ensure))))

(defun awk-posix-ts-mode-test--fontify (level lines)
  (let ((treesit-font-lock-level level))
    (insert (mapconcat #'identity lines "\n"))
    (awk-posix-ts-mode)
    (font-lock-ensure)))

(defun awk-posix-ts-mode-test--position-in-line (line fragment)
  (let (line-start)
    (save-excursion
      (goto-char (point-min))
      (while (and (not line-start) (not (eobp)))
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (if (equal line (buffer-substring-no-properties start end))
              (setq line-start start)
            (forward-line 1)))))
    (unless line-start
      (ert-fail (format "Test line not found: %s" line)))
    (let ((offset (string-search fragment line)))
      (unless offset
        (ert-fail (format "Fragment %s not found in test line: %s"
                          fragment line)))
      (+ line-start offset))))

(defun awk-posix-ts-mode-test--face-in-line (line fragment &optional offset)
  (get-text-property
   (+ (awk-posix-ts-mode-test--position-in-line line fragment)
      (or offset 0))
   'face))

(defun awk-posix-ts-mode-test--comment-in-line-p (line fragment &optional offset)
  (syntax-propertize (point-max))
  (nth 4 (syntax-ppss
          (+ (awk-posix-ts-mode-test--position-in-line line fragment)
             (or offset 0)))))

(defun awk-posix-ts-mode-test--syntax-class-in-line
    (line fragment &optional offset)
  (syntax-propertize (point-max))
  (syntax-class
   (syntax-after
    (+ (awk-posix-ts-mode-test--position-in-line line fragment)
       (or offset 0)))))

(defun awk-posix-ts-mode-test--should-fontify (cases)
  (pcase-dolist (`(,line ,fragment ,face) cases)
    (should (equal (list line fragment
                         (awk-posix-ts-mode-test--face-in-line line fragment))
                   (list line fragment face)))))

(defun awk-posix-ts-mode-test--face-at-next (text)
  (search-forward text)
  (get-text-property (1- (point)) 'face))

(defun awk-posix-ts-mode-test--should-have-next-faces (cases)
  (dolist (case cases)
    (should (eq (awk-posix-ts-mode-test--face-at-next (car case))
                (nth 1 case)))))

(defun awk-posix-ts-mode-test--indent (source &optional offset)
  (with-temp-buffer
    (insert source)
    (awk-posix-ts-mode)
    (setq-local indent-tabs-mode nil)
    (when offset
      (setq-local awk-posix-ts-mode-indent-offset offset))
    (indent-region (point-min) (point-max))
    (let ((indented (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) indented))
      indented)))

(ert-deftest awk-posix-ts-mode-starts-a-posix-awk-parser ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "BEGIN { print \"hello\" }\n")
    (awk-posix-ts-mode)
    (should (eq major-mode 'awk-posix-ts-mode))
    (should
     (equal (treesit-node-type (treesit-buffer-root-node 'posix-awk))
            "program"))))

(ert-deftest awk-posix-ts-mode-provides-a-default-grammar-source ()
  (should
   (equal
    (awk-posix-ts-mode-test--grammar-source nil)
    '(posix-awk "https://github.com/konomanoasa/tree-sitter-posix-awk"
                :revision "v0.7.0"))))

(ert-deftest awk-posix-ts-mode-preserves-a-user-grammar-source ()
  (let ((custom '(posix-awk . ("custom-source"))))
    (should (equal (awk-posix-ts-mode-test--grammar-source (list custom))
                   custom))))

(ert-deftest awk-posix-ts-mode-classifies-only-cst-delimiters-as-parens ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((header "function f(a) {")
          (statement "  if ((a[1])) print f(a)")
          (regexp "  if (/([b]){2}/) print \"()[]{}\"")
          (closing "}"))
      (insert header "\n" statement "\n" regexp "\n" closing "\n")
      (awk-posix-ts-mode)
      (dolist (character '(?\( ?\) ?\[ ?\] ?{ ?}))
        (should (eq (char-syntax character) ?.)))
      (dolist (expectation
               `((,header "f(" 1 4)
                 (,header "a)" 1 5)
                 (,header "{" 0 4)
                 (,statement "if (" 3 4)
                 (,statement "(a[" 0 4)
                 (,statement "a[" 1 4)
                 (,statement "1]" 1 5)
                 (,statement "a[1])" 4 5)
                 (,statement "]))" 2 5)
                 (,statement "f(" 1 4)
                 (,statement "a)" 1 5)
                 (,regexp "if (" 3 4)
                 (,regexp "/)" 1 5)
                 (,closing "}" 0 5)))
        (pcase-let ((`(,line ,fragment ,offset ,class) expectation))
          (should
           (= (awk-posix-ts-mode-test--syntax-class-in-line
               line fragment offset)
              class))))
      (dolist (expectation
               `((,regexp "/(" 1)
                 (,regexp "([" 0)
                 (,regexp "[b" 0)
                 (,regexp "b]" 1)
                 (,regexp "])" 1)
                 (,regexp "{2" 0)
                 (,regexp "2}" 1)
                 (,regexp "()" 0)
                 (,regexp "()" 1)
                 (,regexp "[]" 0)
                 (,regexp "[]" 1)
                 (,regexp "{}" 0)
                 (,regexp "{}" 1)))
        (pcase-let ((`(,line ,fragment ,offset) expectation))
          (should
           (= (awk-posix-ts-mode-test--syntax-class-in-line
               line fragment offset)
              1)))))))

(ert-deftest awk-posix-ts-mode-reclassifies-delimiters-after-edits ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "BEGIN {\nprint\n}\n")
    (awk-posix-ts-mode)
    (should (= (awk-posix-ts-mode-test--syntax-class-in-line "}" "}") 5))
    (goto-char (point-min))
    (search-forward "{")
    (delete-char -1)
    (should (= (awk-posix-ts-mode-test--syntax-class-in-line "}" "}") 1))
    (goto-char (point-min))
    (search-forward "BEGIN ")
    (insert "{")
    (should (= (awk-posix-ts-mode-test--syntax-class-in-line "}" "}") 5))))

(ert-deftest awk-posix-ts-mode-configures-posix-awk-line-comments ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-posix-ts-mode)
    (should (equal comment-start "# "))
    (should (equal comment-end ""))
    (should (equal comment-start-skip "#[[:blank:]]*"))
    (should comment-use-syntax)
    (should (eq (char-syntax ?#) ?.))
    (should (eq (char-syntax ?\") ?.))
    (should (eq (char-syntax ?\\) ?.))
    (should (eq (char-syntax ?\n) ?>))))

(ert-deftest awk-posix-ts-mode-keeps-comments-open-through-buffer-end ()
  (awk-posix-ts-mode-test--require-grammar)
  (dolist (source '("# note" "#"))
    (with-temp-buffer
      (insert source)
      (awk-posix-ts-mode)
      (syntax-propertize (point-max))
      (should (nth 4 (syntax-ppss (point-max)))))))

(ert-deftest awk-posix-ts-mode-recognizes-only-tree-sitter-comments ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((top "# top")
          (string "BEGIN { print \"# string\" }")
          (regexp "/#regexp/ { print } # trailing")
          (continued "# backslash \\")
          (next "BEGIN { print 2 }")
          (recovery-start "BEGIN { print \"open")
          (recovered "# recovered"))
      (insert (mapconcat #'identity
                         (list top string regexp continued next
                               recovery-start recovered)
                         "\n")
              "\n")
      (awk-posix-ts-mode)
      (should (awk-posix-ts-mode-test--comment-in-line-p top "top"))
      (should-not (awk-posix-ts-mode-test--comment-in-line-p string "string"))
      (should-not (awk-posix-ts-mode-test--comment-in-line-p regexp "regexp"))
      (should (awk-posix-ts-mode-test--comment-in-line-p regexp "trailing"))
      (should (awk-posix-ts-mode-test--comment-in-line-p continued "backslash"))
      (should-not (awk-posix-ts-mode-test--comment-in-line-p next "print"))
      (should (awk-posix-ts-mode-test--comment-in-line-p recovered "recovered")))))

(ert-deftest awk-posix-ts-mode-recomputes-comment-syntax-after-edits ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((commented "BEGIN { print x # note"))
      (insert commented "\n}\n")
      (awk-posix-ts-mode)
      (should (awk-posix-ts-mode-test--comment-in-line-p commented "note"))
      (goto-char (awk-posix-ts-mode-test--position-in-line commented "#"))
      (let ((quote-position (point))
            (string "BEGIN { print x \"# note"))
        (insert "\"")
        (should-not (awk-posix-ts-mode-test--comment-in-line-p string "note"))
        (delete-region quote-position (1+ quote-position))
        (should (awk-posix-ts-mode-test--comment-in-line-p commented "note"))))))

(ert-deftest awk-posix-ts-mode-propertizes-comments-hidden-before-a-narrowing ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "# first\n# second\n")
    (awk-posix-ts-mode)
    (goto-char (point-min))
    (forward-line 1)
    (narrow-to-region (point) (point-max))
    (should (nth 4 (syntax-ppss (+ (point-min) 2))))
    (widen)
    (should (nth 4 (syntax-ppss (+ (point-min) 2))))))

(ert-deftest awk-posix-ts-mode-propertizes-comments-at-the-end-of-wide-programs ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (dotimes (number 10000)
      (insert (format "value%d = %d # comment%d\n"
                      number number number)))
    (awk-posix-ts-mode)
    (goto-char (point-max))
    (search-backward "# comment9999")
    (let ((comment-start (point)))
      (funcall syntax-propertize-function comment-start (point-max))
      (should-not (nth 4 (syntax-ppss (1- comment-start))))
      (should (nth 4 (syntax-ppss (+ comment-start 2)))))))

(ert-deftest awk-posix-ts-mode-uses-standard-tree-sitter-navigation ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-posix-ts-mode)
    (should (equal (alist-get 'posix-awk treesit-thing-settings)
                   (alist-get 'posix-awk awk-posix-ts-mode-thing-settings)))
    (should (treesit-thing-defined-p 'sexp 'posix-awk))
    (should-not (treesit-thing-defined-p 'sentence 'posix-awk))
    (should (eq forward-sexp-function #'treesit-forward-sexp))
    (should (eq beginning-of-defun-function
                #'treesit-beginning-of-defun))
    (should (eq end-of-defun-function #'treesit-end-of-defun))))

(ert-deftest awk-posix-ts-mode-navigates-items-and-functions ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((begin "BEGIN { print 1 }")
          (rule "value { print value }")
          (first "function first(value) { return value }")
          (second "function second (value) { return value }")
          (end "END { print 2 }"))
      (insert begin "\n" rule "\n" first "\n" second "\n" end "\n")
      (awk-posix-ts-mode)
      (goto-char (point-min))
      (dolist (line (list begin rule first second end))
        (forward-sexp)
        (should (= (point)
                   (+ (awk-posix-ts-mode-test--position-in-line line line)
                      (length line)))))
      (goto-char (point-max))
      (beginning-of-defun)
      (should (looking-at-p "function second"))
      (beginning-of-defun)
      (should (looking-at-p "function first"))
      (end-of-defun)
      (should (eq (char-before) ?\n))
      (should (eq (char-before (1- (point))) ?})))))

(ert-deftest awk-posix-ts-mode-uses-standard-tree-sitter-imenu ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-posix-ts-mode)
    (should (equal treesit-simple-imenu-settings
                   awk-posix-ts-mode-imenu-settings))
    (should (eq treesit-defun-name-function
                #'awk-posix-ts-mode--defun-name))
    (should (eq imenu-create-index-function #'treesit-simple-imenu))))

(ert-deftest awk-posix-ts-mode-indexes-only-function-items ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "BEGIN { print 1 }\n"
            "function first(value) { return value }\n"
            "value { print value }\n"
            "function second (value) { return value }\n"
            "END { print 2 }\n")
    (awk-posix-ts-mode)
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car index) '("first" "second")))
      (should
       (equal
        (mapcar (lambda (entry) (marker-position (cdr entry))) index)
        (list
         (awk-posix-ts-mode-test--position-in-line
          "function first(value) { return value }" "function")
         (awk-posix-ts-mode-test--position-in-line
          "function second (value) { return value }" "function")))))
    (let ((function (treesit-thing-next (point-min) 'defun))
          (root (treesit-buffer-root-node 'posix-awk)))
      (should (equal (treesit-defun-name function) "first"))
      (should-not (treesit-defun-name root)))))

(ert-deftest awk-posix-ts-mode-rebuilds-imenu-after-edits ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "function first() { return 1 }\n")
    (awk-posix-ts-mode)
    (should (equal (mapcar #'car (funcall imenu-create-index-function))
                   '("first")))
    (goto-char (point-max))
    (insert "function second() { return 2 }\n")
    (should (equal (mapcar #'car (funcall imenu-create-index-function))
                   '("first" "second")))))

(ert-deftest awk-posix-ts-mode-uses-standard-tree-sitter-indentation ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-posix-ts-mode)
    (should (eq indent-line-function #'treesit-indent))
    (should (eq indent-region-function #'treesit-indent-region))
    (should (equal (alist-get 'posix-awk treesit-simple-indent-rules)
                   (alist-get 'posix-awk awk-posix-ts-mode-indent-rules)))))

(ert-deftest awk-posix-ts-mode-indents-structural-rules ()
  (awk-posix-ts-mode-test--require-grammar)
  (should
   (equal
    (awk-posix-ts-mode-test--indent
     "BEGIN {\nprint 1\nif (ready)\nprint 2\nelse\nprint 3\n}\n")
    "BEGIN {\n  print 1\n  if (ready)\n    print 2\n  else\n    print 3\n}\n")))

(ert-deftest awk-posix-ts-mode-indent-offset-controls-rules ()
  (awk-posix-ts-mode-test--require-grammar)
  (should
   (equal
    (awk-posix-ts-mode-test--indent
     "BEGIN {\nprint 1\n}\n" 4)
    "BEGIN {\n    print 1\n}\n")))

(ert-deftest awk-posix-ts-mode-indents-do-while-closers ()
  (awk-posix-ts-mode-test--require-grammar)
  (should
   (equal
    (awk-posix-ts-mode-test--indent
     "BEGIN {\ndo\nprint\n        while (0)\n}\n")
    "BEGIN {\n  do\n    print\n  while (0)\n}\n")))

(ert-deftest awk-posix-ts-mode-selects-awk-files ()
  (awk-posix-ts-mode-test--require-grammar)
  (let (patterns)
    (dolist (entry auto-mode-alist)
      (when (eq (cdr entry) 'awk-posix-ts-mode)
        (push (car entry) patterns)))
    (should (equal (nreverse patterns) '("\\.awk\\'"))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example.awk")
    (set-auto-mode)
    (should (eq major-mode 'awk-posix-ts-mode))))

(ert-deftest awk-posix-ts-mode-generates-mode-and-file-association-autoloads ()
  (require 'loaddefs-gen)
  (let ((output (make-temp-file "awk-posix-ts-mode-loaddefs-"))
        (directory
         (file-name-directory (locate-library "awk-posix-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'awk-posix-ts-mode"
                            "(add-to-list 'auto-mode-alist"
                            "(add-to-list 'interpreter-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

(ert-deftest awk-posix-ts-mode-selects-awk-interpreters ()
  (awk-posix-ts-mode-test--require-grammar)
  (should (equal (alist-get "awk" interpreter-mode-alist nil nil #'equal)
                 'awk-posix-ts-mode))
  (dolist (shebang '("#!/usr/bin/awk -f\n"
                     "#!/usr/bin/env awk -f\n"
                     "#!/usr/bin/env -S awk -f\n"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/example")
      (insert shebang "{ print }\n")
      (set-auto-mode)
      (should (eq major-mode 'awk-posix-ts-mode)))))

(ert-deftest awk-posix-ts-mode-fontifies-posix-awk-syntax ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((comment "# total values")
          (definition "function total(value, scale) {")
          (spaced "function half (value) { return value / 2 }")
          (calculation "  result = sqrt(value) + scale")
          (condition "  if (result >= 10) {")
          (statements "  first = 1; second = 2")
          (output "    printf \"%g\\n\", result")
          (call "BEGIN { total(4, 2) }")
          (division "BEGIN { value = 8 / 2 }")
          (range "NR == 1, NR == 3 { print }")
          (field "BEGIN { print $3, $name, $(1 + 1) }")
          (continuation "BEGIN { value = 1 + \\"))
      (awk-posix-ts-mode-test--fontify
       4
       (list comment definition calculation condition statements output
             "  }" "  return result" "}" spaced call division range field
             continuation "2 }"))
      (awk-posix-ts-mode-test--should-fontify
       `((,comment "total" font-lock-comment-face)
         (,definition "total" font-lock-function-name-face)
         (,definition "value" font-lock-variable-name-face)
         (,definition "scale" font-lock-variable-name-face)
         (,definition "(" font-lock-bracket-face)
         (,definition "," font-lock-punctuation-face)
         (,definition "{" font-lock-bracket-face)
         (,calculation "result" font-lock-variable-use-face)
         (,calculation "=" font-lock-operator-face)
         (,calculation "sqrt" font-lock-builtin-face)
         (,calculation "value" font-lock-variable-use-face)
         (,calculation "+" font-lock-operator-face)
         (,condition "10" font-lock-number-face)
         (,output "\"" font-lock-string-face)
         (,output "%g" font-lock-string-face)
         (,output "\\n" font-lock-escape-face)
         (,output "," font-lock-punctuation-face)
         (,statements ";" font-lock-punctuation-face)
         (,spaced "half" font-lock-function-name-face)
         (,call "total" font-lock-function-call-face)
         (,call "4" font-lock-number-face)
         (,division "/" font-lock-operator-face)
         (,range "," font-lock-punctuation-face)
         (,field "$3" font-lock-operator-face)
         (,field "3" font-lock-number-face)
         (,field "$name" font-lock-operator-face)
         (,field "name" font-lock-variable-use-face)
         (,field "$(1 + 1)" font-lock-operator-face)
         (,continuation "\\" font-lock-punctuation-face))))))

(ert-deftest awk-posix-ts-mode-fontifies-awk-delimiters ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-posix-ts-mode-test--fontify
     4 '("function f(a) { if ((a[1])) { print f(a) } }"))
    (goto-char (point-min))
    (awk-posix-ts-mode-test--should-have-next-faces
     '(("f(" font-lock-bracket-face)
       ("{" font-lock-bracket-face)
       ("if (" font-lock-bracket-face)
       ("(" font-lock-bracket-face)
       ("[" font-lock-bracket-face)
       ("]" font-lock-bracket-face)
       ("{" font-lock-bracket-face)
       ("f(" font-lock-bracket-face)))))

(ert-deftest awk-posix-ts-mode-keeps-static-ere-delimiter-faces ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-posix-ts-mode-test--fontify
     4 '("BEGIN { if (/((a)[b])/) print $(x) }"))
    (goto-char (point-min))
    (awk-posix-ts-mode-test--should-have-next-faces
     '(("{" font-lock-bracket-face)
       ("if (" font-lock-bracket-face)
       ("/(" font-lock-bracket-face)
       ("[" font-lock-bracket-face)
       ("$" font-lock-operator-face)
       ("(" font-lock-bracket-face)))))

(ert-deftest awk-posix-ts-mode-fontifies-every-posix-awk-keyword ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((header "function walk(items, key) {")
          (loop "  for (key in items) delete items[key]")
          (branch "  if (key) next; else nextfile")
          (repeat "  do break; while (key)")
          (tail "  return")
          (start "BEGIN { getline; print 1; printf \"%d\", 1; exit 0 }")
          (finish "END { while (0) continue }"))
      (awk-posix-ts-mode-test--fontify
       4 (list header loop branch repeat tail "}" start finish))
      (pcase-dolist (`(,line . ,keyword)
                     `((,header . "function") (,loop . "for")
                       (,loop . "in") (,loop . "delete")
                       (,branch . "if") (,branch . "next")
                       (,branch . "else") (,branch . "nextfile")
                       (,repeat . "do") (,repeat . "break")
                       (,repeat . "while") (,tail . "return")
                       (,start . "BEGIN") (,start . "getline")
                       (,start . "print") (,start . "printf")
                       (,start . "exit") (,finish . "END")
                       (,finish . "continue")))
        (should (eq (awk-posix-ts-mode-test--face-in-line line keyword)
                    'font-lock-keyword-face))))))

(ert-deftest awk-posix-ts-mode-fontifies-every-posix-awk-named-operator ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((assignments "  a += 1; a -= 1; a *= 2; a /= 2; a %= 2; a ^= 2")
          (steps "  a++; a--")
          (comparisons "  if (a == 1 && a != 2 || a <= 3 && a >= 4) a = 5")
          (matches "  if (a !~ /x/) print a >> \"log\""))
      (awk-posix-ts-mode-test--fontify
       4 (list "BEGIN {" assignments steps comparisons matches "}"))
      (pcase-dolist (`(,line . ,operator)
                     `((,assignments . "+=") (,assignments . "-=")
                       (,assignments . "*=") (,assignments . "/=")
                       (,assignments . "%=") (,assignments . "^=")
                       (,steps . "++") (,steps . "--")
                       (,comparisons . "==") (,comparisons . "&&")
                       (,comparisons . "!=") (,comparisons . "||")
                       (,comparisons . "<=") (,comparisons . ">=")
                       (,matches . "!~") (,matches . ">>")))
        (should (eq (awk-posix-ts-mode-test--face-in-line line operator)
                    'font-lock-operator-face))))))

(ert-deftest awk-posix-ts-mode-fontifies-and-without-another-operator-token ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-posix-ts-mode-test--fontify 4 '("BEGIN { if (left && right) print }"))
    (goto-char (point-min))
    (search-forward "&&")
    (should (eq (get-text-property (1- (point)) 'face)
                'font-lock-operator-face))))

(ert-deftest awk-posix-ts-mode-fontifies-posix-ere-components ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((line "/^a\\.([[:alpha:]x-z]{2,3}|b+).*$/ { print }")
          (control-escape "/a\\nb/ { print }")
          (octal-escape "/\\141/ { print }")
          (undefined-escape "/\\q/ { print }")
          (delimiter-escape "/a\\/b/ { print }")
          (quoted-backslash "/a\\\\b/ { print }")
          (trailing-hyphen "/[a-]/ { print }")
          (plus-hyphen "/[+-]/ { print }")
          (literal-meta "/[a^]/ { print }")
          (negated "/[^a[.b.]]/ { print }")
          (collating-meta "/[[.^.]]/ { print }")
          (range-plus "/[+--]/ { print }")
          (range-letter "/[a--]/ { print }")
          (range-close "/[]--]/ { print }")
          (digit-range "/[0-9]/ { print }")
          (bare "/[a-z0]/ { print }")
          (equivalence "/[[=c=]]/ { print }")
          (bracket-escapes "/[\\q\\/\\\\]/ { print }")
          (modifier "/x{2,3}?/ { print }"))
      (awk-posix-ts-mode-test--fontify
       4 (list line control-escape octal-escape undefined-escape
               delimiter-escape quoted-backslash trailing-hyphen literal-meta
               plus-hyphen negated collating-meta equivalence bracket-escapes
               range-plus range-letter range-close digit-range bare modifier))
      (awk-posix-ts-mode-test--should-fontify
       `((,line "/" font-lock-delimiter-face)
         (,line "^" font-lock-operator-face)
         (,line "a" font-lock-regexp-face)
         (,line "\\." font-lock-escape-face)
         (,line "(" font-lock-bracket-face)
         (,line "[" font-lock-bracket-face)
         (,line "[:" font-lock-bracket-face)
         (,line ":]" font-lock-punctuation-face)
         (,line "alpha" font-lock-constant-face)
         (,line "x-z" font-lock-constant-face)
         (,line "-z" font-lock-operator-face)
         (,line "z]" font-lock-constant-face)
         (,line "]{" font-lock-bracket-face)
         (,line "{" font-lock-bracket-face)
         (,line "2,3" font-lock-number-face)
         (,line ",3" font-lock-punctuation-face)
         (,line "|" font-lock-operator-face)
         (,line "b+" font-lock-regexp-face)
         (,line "+)" font-lock-operator-face)
         (,line ")" font-lock-bracket-face)
         (,line ".*" font-lock-constant-face)
         (,line "*$" font-lock-operator-face)
         (,line "$" font-lock-operator-face)
         (,control-escape "\\n" font-lock-escape-face)
         (,octal-escape "\\141" font-lock-escape-face)
         (,undefined-escape "\\q" font-lock-escape-face)
         (,delimiter-escape "\\/" font-lock-escape-face)
         (,quoted-backslash "\\\\" font-lock-escape-face)
         (,digit-range "0" font-lock-constant-face)
         (,digit-range "9" font-lock-constant-face)
         (,bare "a-z" font-lock-constant-face)
         (,bare "-z" font-lock-operator-face)
         (,bare "z0" font-lock-constant-face)
         (,bare "0]" font-lock-constant-face)
         (,trailing-hyphen "-]" font-lock-constant-face)
         (,plus-hyphen "-]" font-lock-constant-face)
         (,literal-meta "^]" font-lock-constant-face)
         (,negated "^" font-lock-negation-char-face)
         (,negated "a[." font-lock-constant-face)
         (,negated "[." font-lock-bracket-face)
         (,negated ".]" font-lock-punctuation-face)
         (,negated "b" font-lock-constant-face)
         (,collating-meta "^" font-lock-constant-face)
         (,equivalence "[=" font-lock-bracket-face)
         (,equivalence "=]" font-lock-punctuation-face)
         (,equivalence "c" font-lock-constant-face)
         (,bracket-escapes "\\q" font-lock-escape-face)
         (,bracket-escapes "\\/" font-lock-escape-face)
         (,bracket-escapes "\\\\" font-lock-escape-face)
         (,modifier "?/" font-lock-operator-face))))))

(ert-deftest awk-posix-ts-mode-distinguishes-minus-from-range-endpoints ()
  (awk-posix-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((arithmetic "BEGIN { value = -left - right }")
          (ranges '("/[+--]/ { print }"
                    "/[a--]/ { print }"
                    "/[]--]/ { print }")))
      (awk-posix-ts-mode-test--fontify 4 (cons arithmetic ranges))
      (let ((unary-position
             (awk-posix-ts-mode-test--position-in-line arithmetic "-left"))
            (binary-position
             (awk-posix-ts-mode-test--position-in-line arithmetic "- right")))
        (should (eq (get-text-property unary-position 'face)
                    'font-lock-operator-face))
        (should (eq (get-text-property binary-position 'face)
                    'font-lock-operator-face)))
      (dolist (range ranges)
        (let ((range-position
               (awk-posix-ts-mode-test--position-in-line range "--")))
          (should (eq (get-text-property range-position 'face)
                      'font-lock-operator-face))
          (should (eq (get-text-property (1+ range-position) 'face)
                      'font-lock-constant-face)))))))

(ert-deftest awk-posix-ts-mode-fontifies-bracket-collating-elements-as-constants ()
  (awk-posix-ts-mode-test--require-grammar)
  (dolist (entry '(("/[.a.]/ { print }" "a.]") ("/[=b=]/ { print }" "b=]")))
    (with-temp-buffer
      (awk-posix-ts-mode-test--fontify 4 (list (car entry)))
      (should (eq (awk-posix-ts-mode-test--face-in-line (car entry) (cadr entry))
                  'font-lock-constant-face)))))

(ert-deftest awk-posix-ts-mode-fontifies-by-font-lock-level ()
  (awk-posix-ts-mode-test--require-grammar)
  (pcase-dolist (`(,level ,comment-face ,keyword-face ,string-face
                          ,escape-face ,number-face ,regexp-face
                          ,operator-face ,continuation-face)
                 '((1 font-lock-comment-face nil nil nil nil nil nil nil)
                   (2 font-lock-comment-face font-lock-keyword-face
                      font-lock-string-face font-lock-string-face nil nil nil nil)
                   (3 font-lock-comment-face font-lock-keyword-face
                      font-lock-string-face font-lock-escape-face
                      font-lock-number-face nil nil nil)
                   (4 font-lock-comment-face font-lock-keyword-face
                      font-lock-string-face font-lock-escape-face
                      font-lock-number-face font-lock-regexp-face
                      font-lock-operator-face font-lock-punctuation-face)))
    (with-temp-buffer
      (let ((note "# note")
            (program "BEGIN { if (\"a\\tb\" ~ /c/) print 1 + 2 }")
            (continued "BEGIN { value = 1 + \\")
            (tail "2 }"))
        (awk-posix-ts-mode-test--fontify level (list note program continued tail))
        (awk-posix-ts-mode-test--should-fontify
         `((,note "note" ,comment-face)
           (,program "BEGIN" ,keyword-face)
           (,program "\"a" ,string-face)
           (,program "\\t" ,escape-face)
           (,program "1" ,number-face)
           (,program "c" ,regexp-face)
           (,program "+" ,operator-face)
           (,continued "\\" ,continuation-face)))))))

(ert-deftest awk-posix-ts-mode-keeps-font-lock-levels-independent-between-buffers ()
  (awk-posix-ts-mode-test--require-grammar)
  (let ((line "BEGIN { print 1 }")
        (detailed (generate-new-buffer "awk-posix-ts-mode-test-detailed"))
        (plain (generate-new-buffer "awk-posix-ts-mode-test-plain")))
    (unwind-protect
        (progn
          (with-current-buffer detailed
            (awk-posix-ts-mode-test--fontify 4 (list line)))
          (with-current-buffer plain
            (awk-posix-ts-mode-test--fontify 1 (list line))
            (should-not (awk-posix-ts-mode-test--face-in-line line "{")))
          (with-current-buffer detailed
            (font-lock-flush)
            (font-lock-ensure)
            (should (eq (awk-posix-ts-mode-test--face-in-line line "{")
                        'font-lock-bracket-face))))
      (kill-buffer detailed)
      (kill-buffer plain))))

(ert-deftest awk-posix-ts-mode-reports-an-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_) nil))
          (with-temp-buffer
            (let ((error-data (should-error (awk-posix-ts-mode) :type 'user-error)))
              (should (string-match-p
                       "posix-awk" (error-message-string error-data)))
              (should-not (treesit-parser-list nil nil t)))))
      (fset 'treesit-ensure-installed ensure))))

(provide 'awk-posix-ts-mode-test)

;;; awk-posix-ts-mode-test.el ends here
