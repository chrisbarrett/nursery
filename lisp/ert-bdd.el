;;; ert-bbd.el --- BBD-style syntax for ERT -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides a nicer syntax for defining tests to run via ERT. It uses similar
;; BDD-style tests to `buttercup', but since it uses ERT it is better-suited to
;; interactive development.
;;
;; The main functions are:
;;
;;   - `+describe' which encloses a series of tests, and
;;
;;   - `+it', which describes a test cases--usually a single assertion using
;;     ert's `should', `should-not', `should-error', etc.

;;; Code:

(require 'cl-lib)
(require 'ert)

;;; HACK: Initially define no-op so tests can be defined inline.

(cl-eval-when (compile load eval)
  (unless (macrop 'ert-bdd)
    (defmacro ert-bdd (&rest _))))


;; Since the macro is the primary autoloaded entrypoint, the functions it uses
;; must be evaluated when byte-compiling.

(eval-and-compile
  (defun ert-bdd--render-test-name (desc-stack)
    "Build a test name, as a symbol, from a stack of descriptions.

DESC-STACK is a list of descriptions collected from surrounding
`+describe' and `+it' forms."

    (when (null desc-stack)
      (error "Input must be non-empty"))

    (let* ((ordered (seq-reverse (seq-map (lambda (+it) (format "%s" +it)) desc-stack)))
           (transformed (string-replace " " "-"
                                        (string-join ordered "--"))))
      (intern transformed)))

  (defun ert-bdd-compile (form &optional inside-it-p)
    (cl-labels ((compile (form desc-stack inside-it-p)
                  (pcase form

                    (`(+it ,desc . ,body)

                     ;; (when inside-it-p
                     ;;   (error "Cannot write an `+it' inside another `+it'"))

                     (cl-assert (or (stringp desc) (symbolp desc)))

                     `(ert-deftest ,(ert-bdd--render-test-name (cons desc desc-stack)) ()
                        ,@(seq-map (lambda (it) (compile it nil t))
                                   body)))


                    (`(+describe ,desc . ,body)

                     ;; (when inside-it-p
                     ;;   (error "Cannot write a `+describe' inside an `+it'"))

                     (cl-assert (or (stringp desc) (symbolp desc)))

                     (let* ((new-stack (cons desc desc-stack))
                            (new-body (seq-map (lambda (it) (compile it new-stack nil)) body)))
                       (pcase new-body
                         (`()     nil)
                         (`(,x)   x)
                         (xs      `(progn ,@xs)))))

                    ((pred listp)
                     (seq-map (lambda (it) (compile it desc-stack inside-it-p))
                              form))

                    (_
                     form))))
      (compile form nil inside-it-p))))


;;;###autoload
(defmacro +describe (desc &rest forms)
  "Declare a suite of ERT tests using BDD syntax.

DESC is a description of the test suite--either a symbol or a
string.

Within FORMS, you may use additional BDD-style `+describe' forms
to build up a hierarchy of tests. Tests within these blocks are
declared using `+it'.

For example:

\(+describe \"arithmetic operations\"
  (let ((input 100))
    (+describe \"addition\"
      (+it \"has an identity (zero)\"
        (should (equal (+ 0 input) input))))

    ;; etc...
    ))

Tests will be excluded from byte-compiled output."
  (declare (indent 1))
  (unless load-file-name
    (ert-bdd-compile `(+describe ,desc ,@forms))))

;;;###autoload
(defmacro +it (desc &rest forms)
  "An ERT test case using BDD syntax.

DESC is a description of the test case--either a symbol or a
string. It will be concatenated with the descriptions from
enclosing `+describe' forms.

FORMS are the implementation of the test, and should use ert
macros like `should', `should-not' and `should-error'.

Tests will be excluded from byte-compiled output."
  (declare (indent 1))
  (unless load-file-name
    (ert-bdd-compile `(+it ,desc ,@forms) t)))


;;; Tests - nothing like a bit of dogfooding!

(+describe ert-bdd-render-test-name

  (+describe "empty stack"
    (+it "errors"
      (should-error (ert-bdd--render-test-name nil))))

  (+describe "one string element in stack"
    (+it "renders that element"
      (should (equal (ert-bdd--render-test-name '("input"))
                     'input))))

  (+describe "mix of symbols and strings in stack"
    (+it "renders those element"
      (should (equal (ert-bdd--render-test-name '("a" b "c"))
                     'c--b--a))))

  (+describe "input sanitisation"
    (+it "converts spaces to dashes"
      (should (equal (ert-bdd--render-test-name '("a b"))
                     'a-b)))))


(+describe ert-bdd-compile

  (+describe "input is a `+describe'"

    (+describe "no body forms"
      (+it "has no body forms in output"
        (should (equal (ert-bdd-compile
                        '(+describe test-name))
                       nil))))

    (+describe "one body form"
      (+it "outputs those forms"
        (should (equal (ert-bdd-compile
                        '(+describe test-name x))
                       'x))))

    (+describe "many body forms"
      (+it "outputs those forms"
        (should (equal (ert-bdd-compile
                        '(+describe test-name x y))
                       '(progn x y)))))

    (+describe "input contains no `+it' forms"
      (let ((input '(let* ((x 1)
                           (y 2))
                      x
                      y
                      (defun foo ()
                        (+ x y)))))
        (+it "does not transform its input"
          (should (equal (ert-bdd-compile input) input))))))


  (+describe "input is an `+it'"

    (+describe "no body forms"
      (+it "generated test has no body"
        (should (equal (ert-bdd-compile
                        '(+it test-name))
                       '(ert-deftest test-name ())))))

    (+describe "one body form"
      (+it "generated test has that form as its body"
        (should (equal (ert-bdd-compile
                        '(+it test-name x))
                       '(ert-deftest test-name ()
                          x)))))

    (+describe "many body forms"
      (+it "generated test has those body forms"
        (should (equal (ert-bdd-compile
                        '(+it test-name x y))
                       '(ert-deftest test-name ()
                          x
                          y)))))

    (+describe "test name is a symbol"
      (+it "produces the expected test"
        (should (equal
                 (ert-bdd-compile
                  '(+it test-name
                     (should (equal 1 2))))
                 '(ert-deftest test-name ()
                    (should (equal 1 2)))))))

    (+describe "test name is a string"
      (+it "produces the expected test"
        (should (equal
                 (ert-bdd-compile
                  '(+it "test name"
                     (should (equal 1 2))))
                 '(ert-deftest test-name ()
                    (should (equal 1 2))))))))


  (+describe "an +it is wrapped in another form"
    (+it "is still macro-expanded"
      (should (equal

               (ert-bdd-compile
                '(let ((x 1))
                   (+it "test name"
                     (should (= x 1)))))


               '(let ((x 1))
                  (ert-deftest test-name ()
                    (should (= x 1)))))))))


(provide 'ert-bdd)

;;; ert-bdd.el ends here
