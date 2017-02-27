;; Based on Doxymacs.

(defconst doxymacs-doxygen-keywords
  (list
   (list
    ;; One shot keywords that take no arguments
    (concat "\\([@\\\\]\\(brief\\|li\\|\\(end\\)?code\\|sa"
	    "\\|note\\|\\(end\\)?verbatim\\|return\\|arg\\|fn"
	    "\\|hideinitializer\\|showinitializer"
	    ;; FIXME
	    ;; How do I get & # < > % to work?
	    ;;"\\|\\\\&\\|\\$\\|\\#\\|<\\|>\\|\\%"
	    "\\|\\$"
	    "\\|internal\\|nosubgrouping\\|author\\|date\\|endif"
	    "\\|invariant\\|post\\|pre\\|remarks\\|since\\|test\\|version"
	    "\\|\\(end\\)?htmlonly\\|\\(end\\)?latexonly\\|f\\$\\|file"
	    "\\|\\(end\\)?xmlonly\\|\\(end\\)?manonly\\|property"
	    "\\|mainpage\\|name\\|overload\\|typedef\\|deprecated\\|par"
	    "\\|addindex\\|line\\|skip\\|skipline\\|until\\|see"
	    "\\|endlink\\|callgraph\\|endcond\\|else\\)\\)\\>")
    '(0 font-lock-keyword-face prepend))
   ;; attention, warning, etc. given a different font
   (list
    "\\([@\\\\]\\(attention\\|warning\\|todo\\|bug\\)\\)\\>"
    '(0 font-lock-warning-face prepend))
   ;; keywords that take a variable name as an argument
   (list
    (concat "\\([@\\\\]\\(param\\(?:\\s-*\\[\\(?:in\\|out\\|in,out\\)\\]\\)?"
	    "\\|tparam\\|a\\|namespace\\|relates\\(also\\)?"
	    "\\|var\\|def\\)\\)\\s-+\\(\\sw+\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-variable-name-face prepend))
   ;; keywords that take a type name as an argument
   (list
    (concat "\\([@\\\\]\\(class\\|struct\\|union\\|exception\\|enum"
	    "\\|throw\\|interface\\|protocol\\)\\)\\s-+\\(\\(\\sw\\|:\\)+\\)")
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-type-face prepend))
   ;; keywords that take a function name as an argument
   (list
    "\\([@\\\\]retval\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-function-name-face prepend))
   ;; bold
   (list
    "\\([@\\\\]b\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 (quote bold) prepend))
   ;; code
   (list
    "\\([@\\\\][cp]\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 (quote underline) prepend))
   ;; italics/emphasised
   (list
    "\\([@\\\\]e\\(m\\)?\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 (quote italic) prepend))
   ;; keywords that take a list
   (list
    "\\([@\\\\]ingroup\\)\\s-+\\(\\(\\sw+\\s-*\\)+\\)\\s-*$"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend))
   ;; one argument that can contain arbitrary non-whitespace stuff
   (list
    (concat "\\([@\\\\]\\(link\\|copydoc\\|xrefitem"
	    "\\|if\\(not\\)?\\|elseif\\)\\)"
	    "\\s-+\\([^ \t\n]+\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-string-face prepend))
   ;; one optional argument that can contain arbitrary non-whitespace stuff
   (list
    "\\([@\\\\]\\(cond\\|dir\\)\\(\\s-+[^ \t\n]+\\)?\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; one optional argument with no space between
   (list
    "\\([@\\\\]\\(~\\)\\([^ \t\n]+\\)?\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; one argument that has to be a filename
   (list
    (concat "\\([@\\\\]\\(example\\|\\(dont\\)?include\\|includelineno"
	    "\\|htmlinclude\\|verbinclude\\)\\)\\s-+"
	    "\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-string-face prepend))
   ;; dotfile <file> ["caption"]
   (list
    (concat "\\([@\\\\]dotfile\\)\\s-+"
	    "\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)\\(\\s-+\"[^\"]+\"\\)?")
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; image <format> <file> ["caption"] [<sizeindication>=<size>]
   (list
    "\\([@\\\\]image\\)\\s-+\\(html\\|latex\\)\\s-+\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)\\(\\s-+\"[^\"]+\"\\)?\\(\\s-+\\sw+=[0-9]+\\sw+\\)?"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend)
    '(3 font-lock-string-face prepend)
    '(4 font-lock-string-face prepend t)
    '(5 font-lock-string-face prepend t))
   ;; one argument that has to be a word
   (list
    (concat "\\([@\\\\]\\(addtogroup\\|defgroup\\|weakgroup"
	    "\\|page\\|anchor\\|ref\\|section\\|subsection"
	    "\\)\\)\\s-+\\(\\sw+\\)")
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend))))

(defun doxymacs-font-lock ()
  "Turn on font-lock for Doxygen keywords."
  ;; FIXME How do I turn *off* font-lock for Doxygen keywords?
  (interactive)
  (if (functionp 'font-lock-add-keywords)
      ;; Use new (proper?) font-lock-add-keywords function
      (font-lock-add-keywords nil doxymacs-doxygen-keywords)
    ;; Use old-school way
    (let ((old (if (eq (car-safe font-lock-keywords) t)
		 (cdr font-lock-keywords)
	       font-lock-keywords)))
      (setq font-lock-keywords (append old doxymacs-doxygen-keywords)))))

(add-hook 'c-mode-common-hook 'doxymacs-font-lock)
(add-hook 'cc-mode-common-hook 'doxymacs-font-lock)
