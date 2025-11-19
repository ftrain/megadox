# Calc: Advanced Calculator

**Location**: `/home/user/emacs/lisp/calc/`
**Size**: 43 files, 55,552 lines of code
**Author**: David Gillespie
**Purpose**: Reverse Polish Notation (RPN) and algebraic calculator with arbitrary-precision arithmetic

## Overview

Calc is a comprehensive computer algebra system integrated into Emacs, providing sophisticated mathematical capabilities including arbitrary-precision arithmetic, symbolic manipulation, calculus, statistics, and unit conversions. It operates as both an RPN calculator and an algebraic calculator, with extensive support for various mathematical domains.

### Key Features

- **Arbitrary-precision arithmetic**: Integer, rational, floating-point, and complex numbers
- **Symbolic computation**: Algebraic manipulation, simplification, and solving
- **Calculus**: Derivatives, integrals, Taylor series
- **Linear algebra**: Matrices, vectors, determinants, eigenvalues
- **Statistics**: Mean, variance, regression, distributions
- **Financial calculations**: Present value, future value, amortization
- **Unit conversions**: Comprehensive physical units system
- **Multiple modes**: RPN, algebraic, embedded mode
- **Programmability**: Keyboard macros, user-defined functions, rewrite rules

## Architecture

### Core Module Structure

```
calc/
├── calc.el              Main entry point (3,532 lines)
├── calc-ext.el          Extension loader (3,434 lines)
├── calc-macs.el         Macros and fundamental definitions
│
├── Arithmetic & Algebra
│   ├── calc-arith.el    Arithmetic operations (3,067 lines)
│   ├── calc-math.el     Mathematical functions (2,094 lines)
│   ├── calc-alg.el      Algebraic functions (1,942 lines)
│   ├── calcalg2.el      Advanced algebra (3,682 lines)
│   ├── calcalg3.el      More algebra (1,942 lines)
│   ├── calc-frac.el     Fraction arithmetic
│   ├── calc-cplx.el     Complex numbers
│   └── calc-bin.el      Binary/octal/hex arithmetic
│
├── Calculus & Analysis
│   ├── calc-misc.el     Miscellaneous functions
│   ├── calc-funcs.el    Special functions
│   └── calc-poly.el     Polynomial operations
│
├── Linear Algebra
│   ├── calc-vec.el      Vector operations
│   ├── calc-mtx.el      Matrix operations
│   └── calc-map.el      Mapping functions
│
├── Statistics & Finance
│   ├── calc-stat.el     Statistical functions
│   ├── calc-fin.el      Financial calculations
│   ├── calc-nlfit.el    Nonlinear curve fitting
│   └── calc-comb.el     Combinatorics
│
├── Data & Units
│   ├── calc-units.el    Unit conversions (2,390 lines)
│   ├── calc-forms.el    Date/time, HMS, error forms (2,648 lines)
│   └── calc-store.el    Variable storage
│
├── User Interface
│   ├── calc-trail.el    Trail buffer management
│   ├── calc-embed.el    Embedded mode (1,767 lines)
│   ├── calc-yank.el     Copy/paste operations
│   ├── calc-sel.el      Selection mechanism
│   ├── calc-help.el     Help system
│   ├── calc-menu.el     Menu interface (1,914 lines)
│   └── calc-keypd.el    Keypad mode
│
├── Programming
│   ├── calc-prog.el     User programming (2,190 lines)
│   ├── calc-rewr.el     Rewrite rules (2,218 lines)
│   └── calc-rules.el    Rule definitions
│
├── Language & I/O
│   ├── calc-lang.el     Language modes (2,691 lines)
│   ├── calc-aent.el     Algebraic entry
│   ├── calccomp.el      Composition/formatting (1,935 lines)
│   └── calc-graph.el    GNUPLOT interface (1,729 lines)
│
└── Support
    ├── calc-mode.el     Mode management
    ├── calc-undo.el     Undo mechanism
    ├── calc-stuff.el    Utility functions
    └── calc-incom.el    Incomplete objects
```

### Lazy Loading Design

Calc uses a sophisticated lazy-loading architecture to minimize startup time:

```elisp
;; From calc.el, lines 25-30:
;; Calc is split into many files.  This file is the main entry point.
;; This file includes autoload commands for various other basic Calc
;; facilities.  The more advanced features are based in calc-ext, which
;; in turn contains autoloads for the rest of the Calc files.  This
;; odd set of interactions is designed to make Calc's loading time
;; be as short as possible when only simple calculations are needed.
```

**Loading Strategy**:
1. **calc.el**: Core functions, basic arithmetic, number normalization
2. **calc-ext.el**: Extension loader, autoloads advanced features on demand
3. **Specialized modules**: Loaded only when their functionality is accessed

## Data Representation

### Internal Number Format

Calc uses a normalized internal representation for all mathematical objects. From `/home/user/emacs/lisp/calc/calc.el` (lines 2548-2600):

```elisp
;;;; Arithmetic routines.
;;
;; An object as manipulated by one of these routines may take any of the
;; following forms:

;; integer                 An integer.

;; (frac NUM DEN)          A fraction.  NUM and DEN are integers.
;;                         Normalized, DEN > 1.

;; (float NUM EXP)         A floating-point number, NUM * 10^EXP;
;;                         NUM and EXP are integers.
;;                         Normalized, NUM is not a multiple of 10, and
;;                         abs(NUM) < 10^calc-internal-prec.
;;                         Normalized zero is stored as (float 0 0).

;; (cplx REAL IMAG)        A complex number; REAL and IMAG are any of above.
;;                         Normalized, IMAG is nonzero.

;; (polar R THETA)         Polar complex number.  Normalized, R > 0 and THETA
;;                         is neither zero nor 180 degrees (pi radians).

;; (vec A B C ...)         Vector of objects A, B, C, ...  A matrix is a
;;                         vector of vectors.

;; (hms H M S)             Angle in hours-minutes-seconds form.  All three
;;                         components have the same sign; H and M must be
;;                         numerically integers; M and S are expected to
;;                         lie in the range [0,60).

;; (date N)                A date or date/time object.  N is an integer to
;;                         store a date only, or a fraction or float to
;;                         store a date and time.

;; (sdev X SIGMA)          Error form, X +/- SIGMA.  When normalized,
;;                         SIGMA > 0.  X is any complex number and SIGMA
;;                         is real numbers; or these may be symbolic
;;                         expressions where SIGMA is assumed real.

;; (intv MASK LO HI)       Interval form.  MASK is 0=(), 1=(], 2=[), or 3=[].
;;                         LO and HI are any real numbers, or symbolic
;;                         expressions which are assumed real, and LO < HI.
;;                         For [LO..HI], if LO = HI normalization produces LO,
;;                         and if LO > HI normalization produces [LO..LO).
;;                         For other intervals, if LO > HI normalization
;;                         sets HI equal to LO.

;; (mod N M)               Number modulo M.  When normalized, 0 <= N < M.
;;                         N and M are real numbers.

;; (var V S)               Symbolic variable.  V is a Lisp symbol which
;;                         represents the variable's visible name.  S is
;;                         the symbol which actually stores the variable's
;;                         value:  (var pi var-pi).
```

### Type Code Notation

From lines 2604-2627:

```elisp
;; In the following comments, [x y z] means result is x, args must be y, z,
;; respectively, where the code letters are:
;;
;;    O  Normalized object (vector or number)
;;    V  Normalized vector
;;    N  Normalized number of any type
;;    N  Normalized complex number
;;    R  Normalized real number (float or rational)
;;    F  Normalized floating-point number
;;    T  Normalized rational number
;;    I  Normalized integer
;;    B  Normalized big integer
;;    S  Normalized small integer
;;    D  Digit (small integer, 0..999)
;;    L  normalized vector element list (without "vec")
;;    P  Predicate (truth value)
;;    X  Any Lisp object
;;    Z  "nil"
;;
;; Lower-case letters signify possibly un-normalized values.
;; "L.D" means a cons of an L and a D.
;; [N N; n n] means result will be normalized if argument is.
;; Also, [Public] marks routines intended to be called from outside.
```

### Examples of Data Representation

```elisp
;; Integers (native Lisp integers)
42                    ; Small integer
123456789012345678    ; Big integer (arbitrary precision)

;; Fractions
(frac 17 3)          ; 17/3
(frac -5 2)          ; -5/2

;; Floating-point
(float 314 -2)       ; 3.14 (314 × 10^-2)
(float 12345 0)      ; 12345.0
(float 0 0)          ; 0.0

;; Complex numbers (rectangular)
(cplx 2 4)           ; 2 + 4i
(cplx (frac 1 2) 3)  ; 1/2 + 3i

;; Complex numbers (polar)
(polar 5 (float 314159 -5))  ; r=5, θ=π (approximately)

;; Vectors
(vec 1 2 3)          ; [1, 2, 3]

;; Matrices (vectors of vectors)
(vec (vec 1 2) (vec 3 4))    ; [[1, 2], [3, 4]]

;; Error forms
(sdev 100 5)         ; 100 ± 5

;; Intervals
(intv 3 1 4)         ; [1..4] (closed interval)
(intv 0 1 4)         ; (1..4) (open interval)

;; HMS (hours-minutes-seconds)
(hms 2 30 0)         ; 2°30'0"

;; Modular forms
(mod 7 10)           ; 7 mod 10

;; Symbolic variables
(var x var-x)        ; Variable x
```

## Core Normalization

The `math-normalize` function is the heart of Calc's type system. From `/home/user/emacs/lisp/calc/calc.el` (lines 2636-2727):

```elisp
;;; Reduce an object to canonical (normalized) form.  [O o; Z Z] [Public]

(defvar math-normalize-error nil
  "Non-nil if the last call the `math-normalize' returned an error.")

(defun math-normalize (a)
  (setq math-normalize-error nil)
  (cond
   ((not (consp a)) a)
   ((eq (car a) 'float)
    (math-make-float (math-normalize (nth 1 a))
                     (nth 2 a)))
   ((or (memq (car a)
              '(frac cplx polar hms date mod sdev intv vec var quote
                     special-const calcFunc-if calcFunc-lambda
                     calcFunc-quote calcFunc-condition
                     calcFunc-evalto))
        (integerp (car a))
        (and (consp (car a))
             (not (eq (car (car a)) 'lambda))))
    (require 'calc-ext)
    (math-normalize-fancy a))
   (t
    (or (and calc-simplify-mode
             (require 'calc-ext)
             (math-normalize-nonstandard a))
        (let ((args (mapcar #'math-normalize (cdr a))))
          (or (condition-case err
                  (let ((func
                         (assq (car a) '( ( + . math-add )
                                          ( - . math-sub )
                                          ( * . math-mul )
                                          ( / . math-div )
                                          ( % . math-mod )
                                          ( ^ . math-pow )
                                          ( neg . math-neg )
                                          ( | . math-concat ) ))))
                    (or (and var-EvalRules
                             (progn
                               (or (eq var-EvalRules math-eval-rules-cache-tag)
                                   (progn
                                     (require 'calc-ext)
                                     (math-recompile-eval-rules)))
                               (and (or math-eval-rules-cache-other
                                        (assq (car a)
                                              math-eval-rules-cache))
                                    (math-apply-rewrites
                                     (cons (car a) args)
                                     (cdr math-eval-rules-cache)
                                     nil math-eval-rules-cache))))
                        (if func
                            (apply (cdr func) args)
                          (and (or (consp (car a))
                                   (fboundp (car a))
                                   (and (not (featurep 'calc-ext))
                                        (require 'calc-ext)
                                        (fboundp (car a))))
                               (apply (car a) args)))))
                (wrong-number-of-arguments
                 (setq math-normalize-error t)
                 (calc-record-why "*Wrong number of arguments"
                                  (cons (car a) args))
                 nil)
                (wrong-type-argument
                 (or calc-next-why
                     (calc-record-why "Wrong type of argument"
                                      (cons (car a) args)))
                 nil)
                (args-out-of-range
                 (setq math-normalize-error t)
                 (calc-record-why "*Argument out of range"
                                  (cons (car a) args))
                 nil)
                (inexact-result
                 (calc-record-why "No exact representation for result"
                                  (cons (car a) args))
                 nil)
                (math-overflow
                 (setq math-normalize-error t)
                 (calc-record-why "*Floating-point overflow occurred"
                                  (cons (car a) args))
                 nil)
                (math-underflow
                 (setq math-normalize-error t)
                 (calc-record-why "*Floating-point underflow occurred"
                                  (cons (car a) args))
                 nil)
                (void-variable
                 (setq math-normalize-error t)
                 (if (eq (nth 1 err) 'var-EvalRules)
                     (progn
                       (setq var-EvalRules nil)
                       (math-normalize (cons (car a) args)))
                   (calc-record-why "*Variable is void" (nth 1 err)))))
              (if (consp (car a))
                  (math-dimension-error)
                (cons (car a) args))))))))
```

**Normalization guarantees**:
- All results are in canonical form
- Fractions are reduced to lowest terms
- Floating-point mantissas don't end in zero
- Complex numbers with zero imaginary part become real
- Error conditions are properly signaled and recorded

## Stack-Based Calculator Model

### The Calculator Stack

From `/home/user/emacs/lisp/calc/calc.el` (lines 468-474):

```elisp
(defvar calc-stack '((top-of-stack 1 nil))
  "Calculator stack.
Entries are 3-lists:  Formula, Height (in lines), Selection (or nil).")

(defvar calc-stack-top 1
  "Index into `calc-stack' of \"top\" of stack.
This is 1 unless `calc-truncate-stack' has been used.")
```

**Stack entry structure**:
```elisp
(FORMULA HEIGHT SELECTION)
```
- `FORMULA`: The mathematical object (in normalized form)
- `HEIGHT`: Number of display lines (for line-breaking)
- `SELECTION`: Currently selected sub-expression, or nil

**Stack operations**:
```elisp
;; From calc.el, line 1752
(defun calc-stack-size ()
  (- (length calc-stack) calc-stack-top))
```

### RPN vs. Algebraic Mode

From the mode documentation (lines 1324-1329):

```elisp
(defun calc-mode ()
  "Calculator major mode.

This is a Reverse Polish notation (RPN) calculator featuring
arbitrary-precision integer, rational, floating-point, complex,
matrix, and symbolic arithmetic.

RPN calculation:  2 RET 3 +    produces 5.
Algebraic style:  ' 2+3 RET    produces 5.
```

**RPN Mode** (default):
- Operands pushed onto stack first
- Operators consume stack items
- Example: `2 RET 3 +` → pushes 2, pushes 3, adds (pops both, pushes 5)

**Algebraic Mode**:
- Expressions entered using `'` (quote) prefix
- Standard infix notation
- Example: `' 2+3 RET` → parses and evaluates expression

## Arithmetic Operations

### Basic Arithmetic (`calc-arith.el`)

The arithmetic module (`calc-arith.el`, 3,067 lines) implements fundamental operations with automatic type promotion:

```elisp
;; From calc.el, lines 2839-2863
;;; Compute the sum of A and B.  [O O O] [Public]
(defun math-add (a b)
  (or
   (and (not (or (consp a) (consp b)))
        (+ a b))
   (and (Math-zerop a) (not (eq (car-safe a) 'mod))
        (if (and (math-floatp a) (Math-ratp b)) (math-float b) b))
   (and (Math-zerop b) (not (eq (car-safe b) 'mod))
        (if (and (math-floatp b) (Math-ratp a)) (math-float a) a))
   (and (Math-objvecp a) (Math-objvecp b)
        (or
         (and (Math-ratp a) (Math-ratp b)
              (require 'calc-ext)
              (calc-add-fractions a b))
         (and (Math-realp a) (Math-realp b)
              (progn
                (or (and (consp a) (eq (car a) 'float))
                    (setq a (math-float a)))
                (or (and (consp b) (eq (car b) 'float))
                    (setq b (math-float b)))
                (math-add-float a b)))
         (and (require 'calc-ext)
              (math-add-objects-fancy a b))))
   (and (require 'calc-ext)
        (math-add-symb-fancy a b))))
```

**Type promotion hierarchy**:
1. Integer + Integer → Integer
2. Integer + Rational → Rational
3. Rational + Float → Float
4. Real + Complex → Complex
5. Scalar + Symbolic → Symbolic expression

**Floating-point addition** (lines 2865-2880):

```elisp
(defun math-add-float (a b)   ; [F F F]
  (let ((ediff (- (nth 2 a) (nth 2 b))))
    (if (>= ediff 0)
        (if (>= ediff (+ calc-internal-prec calc-internal-prec))
            a
          (math-make-float (math-add (nth 1 b)
                                     (if (eq ediff 0)
                                         (nth 1 a)
                                       (math-scale-left (nth 1 a) ediff)))
                           (nth 2 b)))
      (if (>= (setq ediff (- ediff))
              (+ calc-internal-prec calc-internal-prec))
          b
        (math-make-float (math-add (nth 1 a)
                                   (math-scale-left (nth 1 b) ediff))
                         (nth 2 a))))))
```

This implementation:
- Aligns exponents before adding mantissas
- Handles precision loss when exponents differ greatly
- Maintains arbitrary precision through integer mantissa arithmetic

### Mathematical Functions (`calc-math.el`)

The `calc-math.el` module (2,094 lines) provides transcendental functions:

**Precision-aware computation**:

```elisp
;; From calc-math.el, lines 37-82
(defvar math-emacs-precision
  (let* ((n 1)
         (x 9)
         (xx (+ x (* 9 (expt 10 (- n))))))
    (while (/= x xx)
      (progn
        (setq n (1+ n))
        (setq x xx)
        (setq xx (+ x (* 9 (expt 10 (- n)))))))
    (1- n))
  "The number of digits in an Emacs float.")

(defvar math-largest-emacs-expt
  (let ((x 1)
        (pow 1e2))
    ;; Find the largest power of 10 which is an Emacs float
    (while (and pow (< pow 1.0e+INF))
      (setq x (* 2 x))
      (setq pow (ignore-errors (expt 10.0 (* 2 x)))))
    (setq pow (ignore-errors (expt 10.0 (1+ x))))
    (while (and pow (< pow 1.0e+INF))
      (setq x (1+ x))
      (setq pow (ignore-errors (expt 10.0 (1+ x)))))
    (1- x))
  "The largest exponent which Calc will convert to an Emacs float.")

(defun math-use-emacs-fn (fn x)
  "Use the native Emacs function FN to evaluate the Calc number X.
If this can't be done, return NIL."
  (and
   (<= calc-internal-prec math-emacs-precision)
   (math-realp x)
   (let* ((xpon (+ (nth 2 x) (1- (math-numdigs (nth 1 x))))))
     (and (<= math-smallest-emacs-expt xpon)
          (<= xpon math-largest-emacs-expt)
          (ignore-errors
            (math-read-number
             (number-to-string
              (funcall fn
                       (string-to-number
                        (let ((calc-number-radix 10)
                              (calc-twos-complement-mode nil))
                          (math-format-number x)))))))))))
```

This code:
- Determines Emacs float precision at compile time
- Delegates to native Emacs functions when possible
- Falls back to arbitrary-precision algorithms when needed

## Algebraic Operations

### Simplification (`calc-alg.el`)

The algebraic module provides several levels of simplification:

```elisp
;; From calc.el, lines 721-729
(defcalcmodevar calc-simplify-mode 'alg
  "Type of simplification applied to results.
If `none', results are not simplified when pushed on the stack.
If `num', functions are simplified only when args are constant.
If nil, only limited simplifications are applied.
If `binary', `math-clip' is applied if appropriate.
If `alg', `math-simplify' is applied.
If `ext', `math-simplify-extended' is applied.
If `units', `math-simplify-units' is applied.")
```

**Simplification modes**:
- `none`: No automatic simplification
- `num`: Numeric simplification only
- `nil`: Basic simplification
- `binary`: Binary mode simplification
- `alg`: Algebraic simplification (default)
- `ext`: Extended simplification
- `units`: Unit-aware simplification

### Symbolic Manipulation

From `calc-alg.el` (lines 53-66):

```elisp
(defun calc-simplify ()
  (interactive)
  (calc-slow-wrapper
   (let ((top (calc-top-n 1)))
     (if (calc-is-inverse)
         (setq top
               (let ((calc-simplify-mode nil))
                 (math-normalize (math-trig-rewrite top)))))
     (if (calc-is-hyperbolic)
         (setq top
               (let ((calc-simplify-mode nil))
                 (math-normalize (math-hyperbolic-trig-rewrite top)))))
     (calc-with-default-simplification
      (calc-enter-result 1 "simp" (math-simplify top))))))
```

**Example simplifications**:
- `sin(x)^2 + cos(x)^2` → `1`
- `(x+1)^2` → `x^2 + 2x + 1`
- `sqrt(8)` → `2*sqrt(2)` (in symbolic mode)

## Calculus (`calcalg2.el`)

The calculus module (3,682 lines) provides differentiation and integration:

### Differentiation

```elisp
;; From calcalg2.el, lines 31-49
(defun calc-derivative (var num)
  (interactive "sDifferentiate with respect to: \np")
  (calc-slow-wrapper
   (when (< num 0)
     (error "Order of derivative must be positive"))
   (let ((func (if (calc-is-hyperbolic) 'calcFunc-tderiv 'calcFunc-deriv))
         n expr)
     (if (or (equal var "") (equal var "$"))
         (setq n 2
               expr (calc-top-n 2)
               var (calc-top-n 1))
       (setq var (math-read-expr var))
       (when (eq (car-safe var) 'error)
         (error "Bad format in expression: %s" (nth 1 var)))
       (setq n 1
             expr (calc-top-n 1)))
     (while (>= (setq num (1- num)) 0)
       (setq expr (list func expr var)))
     (calc-enter-result n "derv" expr))))
```

**Differentiation features**:
- Symbolic derivatives of elementary functions
- Chain rule, product rule, quotient rule
- Partial derivatives (multiple variables)
- Higher-order derivatives
- Total derivatives (`tderiv`)

### Integration

```elisp
;; From calcalg2.el, lines 51-65
(defun calc-integral (var &optional arg)
  (interactive "sIntegration variable: \nP")
  (if arg
      (calc-tabular-command 'calcFunc-integ "Integration" "intg" nil var nil nil)
    (calc-slow-wrapper
     (if (or (equal var "") (equal var "$"))
         (calc-enter-result 2 "intg" (list 'calcFunc-integ
                                           (calc-top-n 2)
                                           (calc-top-n 1)))
       (let ((var (math-read-expr var)))
         (if (eq (car-safe var) 'error)
             (error "Bad format in expression: %s" (nth 1 var)))
         (calc-enter-result 1 "intg" (list 'calcFunc-integ
                                           (calc-top-n 1)
                                           var)))))))
```

**Integration capabilities**:
- Symbolic integration of elementary functions
- Integration by parts
- Integration by substitution
- Definite integrals
- Numerical integration (`ninteg`)

## Vector and Matrix Operations

### Vector Representation (`calc-vec.el`)

Vectors and matrices use the `vec` form:

```elisp
;; Vector: (vec element1 element2 ...)
;; Matrix: (vec (vec row1-col1 row1-col2 ...)
;;              (vec row2-col1 row2-col2 ...)
;;              ...)
```

### Matrix Operations (`calc-mtx.el`)

From `calc-vec.el` documentation:

```elisp
;; From calc-vec.el, lines 611+
;;; Build a constant vector or matrix.  [Public]

;; From calc-vec.el, lines 910+
;;; Convert a scalar or vector into an NxN diagonal matrix.  [Public]

;; From calc-vec.el, lines 1072+
;;; Compute the row and column norms of a vector or matrix.  [Public]
```

**Matrix capabilities**:
- Matrix multiplication
- Matrix inversion
- Determinants
- LU decomposition
- Eigenvalues (via external tools)
- Row/column operations
- Transpose, trace, rank

## Units System (`calc-units.el`)

The units module (2,390 lines) provides comprehensive unit conversion. From the file header:

```elisp
;;; Units table updated 9-Jan-91 by Ulrich Müller (ulm@vsnhd1.cern.ch)
;;; with some additions by Przemek Klosowski (przemek@rrdstrad.nist.gov)
;;; Updated April 2002 by Jochen Küpper

;;; Updated August 2007, using
;;;     CODATA (https://physics.nist.gov/cuu/Constants/index.html)
;;;     NIST   (https://physics.nist.gov/Pubs/SP811/appenB9.html)
;;;     ESUWM  (Encyclopaedia of Scientific Units, Weights and
;;;             Measures, by François Cardarelli)
;;; All conversions are exact unless otherwise noted.

;; Updated November 2018 for the redefinition of the SI
;; https://www.bipm.org/en/committees/cg/cgpm/26-2018/resolution-1

;; CODATA values last updated June 2024, using 2022 adjustment:
;; P. J. Mohr, E. Tiesinga, D. B. Newell, and B. N. Taylor (2024-05-08)
```

Sample unit definitions (lines 53-100):

```elisp
(defvar math-standard-units
  '( ;; Length
    ( m       nil                    "*Meter" )
    ( in      "254*10^(-2) cm"       "Inch"  nil "2.54 cm")
    ( ft      "12 in"                "Foot")
    ( yd      "3 ft"                 "Yard" )
    ( mi      "5280 ft"              "Mile" )
    ( au      "149597870700 m"       "Astronomical Unit")
    ( lyr     "c yr"                 "Light Year" )
    ( pc      "(648000/pi) au"       "Parsec (**)")
    ( nmi     "1852 m"               "Nautical Mile" )

    ;; Area
    ( hect    "10000 m^2"            "*Hectare" )
    ( acre    "(1/640) mi^2"         "Acre" )

    ;; Volume
    ( L       "10^(-3) m^3"          "*Liter" )
    ( gal     "4 qt"                 "US Gallon" )

    ;; Time
    ( s       nil                    "*Second" )
    ( min     "60 s"                 "Minute" )
    ( hr      "60 min"               "Hour" )

    ;; Mass
    ( g       nil                    "*Gram" )
    ( lb      "16 oz"                "Pound (mass)" )

    ;; Force
    ( N       "m kg / s^2"           "*Newton" )
    ( dyn     "10^(-5) N"            "Dyne" )

    ;; Energy
    ( J       "N m"                  "*Joule" )
    ( eV      "ech V"                "Electron Volt" )
    ( cal     "4.184 J"              "Calorie" )

    ;; Power
    ( W       "J/s"                  "*Watt" )

    ;; And many more...
    ))
```

**Unit features**:
- SI units and common non-SI units
- Automatic unit conversion
- Unit simplification
- Dimensional analysis
- Physical constants (speed of light, Planck's constant, etc.)

## Statistics (`calc-stat.el`)

Statistical operations on vectors:

```elisp
;; From calc-stat.el, lines 31+
;;; Statistical operations on vectors.

(defun calc-vector-count (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-vector-op "coun" 'calcFunc-vcount arg)))

(defun calc-vector-sum (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-vector-op "vprd" 'calcFunc-vprod arg)
     (calc-vector-op "vsum" 'calcFunc-vsum arg))))

(defun calc-vector-mean (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (if (calc-is-inverse)
           (calc-vector-op "harm" 'calcFunc-vhmean arg)
         (calc-vector-op "medn" 'calcFunc-vmedian arg))
     (if (calc-is-inverse)
         (calc-vector-op "meae" 'calcFunc-vmeane arg)
       (calc-vector-op "mean" 'calcFunc-vmean arg)))))
```

**Statistical capabilities**:
- Descriptive statistics: mean, median, mode, variance, standard deviation
- Correlation and covariance
- Linear regression
- Curve fitting
- Probability distributions (normal, binomial, Poisson, etc.)
- Hypothesis testing

## Financial Functions (`calc-fin.el`)

Time-value-of-money calculations:

```elisp
;; From calc-fin.el, lines 31+
;;; Financial functions.

(defun calc-fin-pv ()
  (interactive)
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-enter-result 3 "pvl" (cons 'calcFunc-pvl (calc-top-list-n 3)))
     (let ((n (if (calc-is-option) 4 3)))
      (if (calc-is-inverse)
          (calc-enter-result n "pvb" (cons 'calcFunc-pvb (calc-top-list-n n)))
        (calc-enter-result n "pv" (cons 'calcFunc-pv (calc-top-list-n n))))))))

(defun calc-fin-npv (arg)
  (interactive "p")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-vector-op "npvb" 'calcFunc-npvb (1+ arg))
     (calc-vector-op "npv" 'calcFunc-npv (1+ arg)))))
```

**Financial calculations**:
- Present value (PV) and future value (FV)
- Net present value (NPV)
- Internal rate of return (IRR)
- Payment schedules (PMT)
- Loan amortization
- Depreciation

## User Interface

### Trail Buffer (`calc-trail.el`)

The trail maintains a history of all calculations:

```elisp
;; From calc-trail.el, lines 31+
;;; Trail commands.

(defun calc-trail-in ()
  (interactive)
  (let ((win (get-buffer-window (calc-trail-display t))))
    (and win (select-window win))))

(defun calc-trail-next (n)
  (interactive "p")
  (calc-with-trail-buffer
   (forward-line n)
   (calc-trail-here)))
```

**Trail features**:
- Records all stack operations
- Can recall previous results
- Searchable history
- Can save/load trail sessions

### Embedded Mode (`calc-embed.el`)

Embedded mode allows Calc to operate directly in any buffer:

```elisp
;; From calc-embed.el, lines 64+
(defconst calc-embedded-mode-vars '(("twos-complement" . calc-twos-complement-mode)
                                    ("precision" . calc-internal-prec)
                                    ("word-size" . calc-word-size)
                                    ("angles" . calc-angle-mode)
                                    ("symbolic" . calc-symbolic-mode)
                                    ("matrix" . calc-matrix-mode)
                                    ("fractions" . calc-prefer-frac)
                                    ("complex" . calc-complex-mode)
                                    ("simplify" . calc-simplify-mode)
                                    ("language" . the-language)
                                    ("plain" . calc-show-plain)
                                    ("break" . calc-line-breaking)
                                    ("justify" . the-display-just)
                                    ("left-label" . calc-left-label)
                                    ("right-label" . calc-right-label)
                                    ("radix" . calc-number-radix)
                                    ("leading-zeros" . calc-leading-zeros)))
```

**Embedded mode features**:
- Evaluates formulas in place
- Language-specific delimiters (LaTeX, C, Pascal, etc.)
- Automatic updates
- Mode annotations embedded in comments

Example usage in a LaTeX document:

```latex
The area of a circle is % Embed
% calc-language: latex
% calc-angles: rad
$$ A = \pi r^2 = 3.14159 $$ % 3.14159265358979
```

### Complex Numbers (`calc-cplx.el`)

Complex number support with multiple display formats:

```elisp
;; From calc-cplx.el, lines 59+
(defun calc-complex-notation ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-complex-format nil t)
   (message "Displaying complex numbers in (X,Y) format")))

(defun calc-i-notation ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-complex-format 'i t)
   (message "Displaying complex numbers in X+Yi format")))

(defun calc-j-notation ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-complex-format 'j t)
   (message "Displaying complex numbers in X+Yj format")))

(defun calc-polar-mode (n)
  (interactive "P")
  (calc-wrapper
   (if (if n
           (> (prefix-numeric-value n) 0)
         (eq calc-complex-mode 'cplx))
       (progn
         (calc-change-mode 'calc-complex-mode 'polar)
         (message "Preferring polar complex numbers"))
     (calc-change-mode 'calc-complex-mode 'cplx)
     (message "Preferring rectangular complex numbers"))))
```

**Display formats**:
- `(2, 4)`: Rectangular notation
- `2+4i`: Engineering notation (i)
- `2+4j`: Engineering notation (j)
- `(5; 1.107)`: Polar notation (magnitude; angle)

## Programming Features

### User-Defined Functions (`calc-prog.el`)

The `defmath` macro simplifies creating Calc functions:

```elisp
;; From calc.el, lines 3491-3504
;;;###autoload
(defmacro defmath (func args &rest body)   ;  [Public]
  "Define Calc function.

Like `defun' except that code in the body of the definition can
make use of the full range of Calc data types and the usual
arithmetic operations are converted to their Calc equivalents.

The prefix `calcFunc-' is added to the specified name to get the
actual Lisp function name.

See Info node `(calc)Defining Functions'."
  (declare (doc-string 3) (indent defun))
  (require 'calc-ext)
  (math-do-defmath func args body))
```

**Example**:

```elisp
(defmath mysum (a b c)
  "Compute a + b + c"
  (+ a b c))

;; Creates: calcFunc-mysum
;; Automatically handles Calc types
;; Available as 'mysum(a,b,c)' in algebraic mode
```

### Keyboard Macros

Calc integrates with Emacs keyboard macros for repetitive calculations.

### Rewrite Rules (`calc-rewr.el`)

Powerful pattern-matching rewrite system (2,218 lines):

```elisp
;; Example rewrite rules:
;; sin(x)^2 + cos(x)^2 := 1
;; x + 0 := x
;; x * 1 := x
;; log(a*b) := log(a) + log(b)
```

Users can define custom rewrite rules to automate algebraic transformations.

## Language Modes (`calc-lang.el`)

Calc can parse and format expressions in various languages (2,691 lines):

**Supported languages**:
- `normal`: Standard Calc notation
- `flat`: One-line format
- `big`: Large-character notation
- `unform`: Unformatted Lisp
- `c`: C/C++ syntax
- `pascal`: Pascal syntax
- `fortran`: Fortran syntax
- `tex`: TeX/LaTeX notation
- `latex`: LaTeX-specific
- `eqn`: Eqn (troff) notation
- `yacas`: Yacas CAS syntax
- `maxima`: Maxima syntax
- `giac`: Giac syntax
- `math`: Mathematica syntax
- `maple`: Maple syntax

Example in different languages:

```
Normal:   sqrt(x^2 + y^2)
Big:          __________
             / 2    2
            √ x  + y

TeX:      \sqrt{x^{2} + y^{2}}
C:        sqrt(x*x + y*y)
Fortran:  SQRT(X**2 + Y**2)
```

## Precision and Modes

### Precision Control

From `calc.el` (lines 737-738):

```elisp
(defcalcmodevar calc-internal-prec 12
  "Number of digits of internal precision for calc-mode calculations.")
```

Users can set precision from 3 to thousands of digits.

### Angular Modes

```elisp
(defcalcmodevar calc-angle-mode 'deg
  "If deg, angles are in degrees; if rad, angles are in radians.
If hms, angles are in degrees-minutes-seconds.")
```

**Angle modes**:
- `deg`: Degrees (default)
- `rad`: Radians
- `hms`: Hours-minutes-seconds (sexagesimal)

### Display Modes

From `calc.el` (lines 476-487):

```elisp
(defvar calc-display-sci-high 0
  "Floating-point numbers with this positive exponent or higher above the
current precision are displayed in scientific notation in `calc-mode'.")

(defvar calc-display-sci-low -3
  "Floating-point numbers with this negative exponent or lower are displayed
scientific notation in `calc-mode'.")
```

**Number display formats**:
- Normal: `12345.6789`
- Scientific: `1.23456789e4`
- Engineering: `12.3456789e3`
- Fixed-point: Always show decimals
- Binary, octal, hexadecimal
- Fractions: `17:3` (17/3)

## Advanced Features

### Arbitrary Precision

Calc uses Lisp integers for arbitrary-precision arithmetic:

```elisp
;; Examples:
12345678901234567890123456789012345678901234567890  ; Exact integer
(factorial 100)  ; 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```

### Symbolic Computation

Variables and symbolic expressions:

```elisp
;; From calc.el, lines 2596-2600
;; (var V S)               Symbolic variable.  V is a Lisp symbol which
;;                         represents the variable's visible name.  S is
;;                         the symbol which actually stores the variable's
;;                         value:  (var pi var-pi).
```

**Example symbolic operations**:
- Simplify: `(x+1)^2` → `x^2 + 2*x + 1`
- Factor: `x^2 - 1` → `(x-1)*(x+1)`
- Solve: `x^2 - 4 = 0` → `x = 2` or `x = -2`
- Differentiate: `d/dx sin(x^2)` → `2*x*cos(x^2)`
- Integrate: `∫ x*e^x dx` → `x*e^x - e^x`

### Error Forms and Intervals

**Error forms** (uncertainties):

```elisp
;; (sdev X SIGMA)          Error form, X +/- SIGMA.
100 +/- 5   ; Represented as (sdev 100 5)
```

Error propagation through calculations:
- `(100 ± 5) + (200 ± 3)` → `300 ± 5.83095` (√(5² + 3²))
- `(10 ± 0.1) * (20 ± 0.2)` → `200 ± 2.236` (propagated via calculus)

**Intervals**:

```elisp
;; (intv MASK LO HI)       Interval form.  MASK is 0=(), 1=(], 2=[), or 3=[].
[1 .. 4]    ; Closed interval
(1 .. 4)    ; Open interval
[1 .. 4)    ; Half-open interval
```

Interval arithmetic:
- `[1..2] + [3..4]` → `[4..6]`
- `[2..3] * [4..5]` → `[8..15]`

## Extension Points

### Custom Functions

Users can add custom functions via several mechanisms:

1. **defmath macro**: For Lisp programmers
2. **Keyboard macros**: For keyboard-driven function definition
3. **Rewrite rules**: For algebraic transformations
4. **External programs**: Via GNUPLOT or other tools

### Hooks

```elisp
(defvar calc-load-hook nil
  "Hook run when Calc is loaded.")

(defvar calc-mode-hook nil
  "Hook run when entering Calc mode.")
```

### Settings Persistence

```elisp
;; From calc.el, lines 232-235
(defcustom calc-settings-file
  (locate-user-emacs-file "calc.el" ".calc.el")
  "File in which to record permanent settings."
  :type '(file))
```

User customizations are automatically saved to `~/.emacs.d/calc.el`.

## Implementation Insights

### Performance Optimizations

1. **Lazy loading**: Modules loaded on-demand
2. **Native arithmetic**: Uses Lisp integers when possible
3. **Precision limiting**: Tracks precision to avoid unnecessary computation
4. **Caching**: Results cached for expensive operations
5. **Native function delegation**: Uses Emacs native functions when precision allows

Example from `calc-math.el` (lines 84-100):

```elisp
(defun math-use-emacs-fn (fn x)
  "Use the native Emacs function FN to evaluate the Calc number X.
If this can't be done, return NIL."
  (and
   (<= calc-internal-prec math-emacs-precision)
   (math-realp x)
   (let* ((xpon (+ (nth 2 x) (1- (math-numdigs (nth 1 x))))))
     (and (<= math-smallest-emacs-expt xpon)
          (<= xpon math-largest-emacs-expt)
          (ignore-errors
            (math-read-number
             (number-to-string
              (funcall fn
                       (string-to-number
                        (let ((calc-number-radix 10)
                              (calc-twos-complement-mode nil))
                          (math-format-number x)))))))))))
```

### Error Handling

Calc uses a sophisticated error recording system:

```elisp
;; Errors are recorded but don't necessarily abort
(calc-record-why "*Wrong number of arguments" expr)
(calc-record-why "Division by zero" expr)
(calc-record-why "*Floating-point overflow occurred" expr)
```

Errors can be reviewed with the `w` (why) command.

### Normalization Philosophy

All operations produce normalized results:
- Ensures canonical representation
- Simplifies equality testing
- Prevents error accumulation
- Makes pattern matching reliable

## Usage Examples

### Basic RPN Calculations

```
2 RET 3 +          → 5
10 RET 3 /         → 3.33333...
2 RET 3 RET 4 * +  → 14  (2 + 3*4)
```

### Algebraic Entry

```
' 2+3*4 RET        → 14
' sin(pi/4) RET    → 0.707106... (or sqrt(2)/2 in exact mode)
' integrate(x^2, x) RET  → x^3/3
```

### Matrix Operations

```
[[1, 2], [3, 4]] RET    ; Enter matrix
RET &                    ; Duplicate and invert
*                        ; Multiply by inverse → identity matrix
```

### Unit Conversions

```
100 u c              ; Convert 100 to specified unit
55 mph RET u c m/s   ; 55 mph → 24.5872 m/s
9.8 m/s^2 u c ft/s^2 ; Acceleration conversion
```

### Symbolic Computation

```
' x^2 - 4 RET a f    ; Factor → (x-2)*(x+2)
' sin(x) RET a d x   ; Differentiate → cos(x)
' x*e^x RET a i x    ; Integrate → x*e^x - e^x
```

## Integration with Emacs

### Embedding in Buffers

Calc can evaluate formulas directly in any buffer:

```
C-x * e    ; Activate embedded mode
```

Example in a text file:

```
The area of a circle with radius 5 is:
pi * 5^2 = 78.5398163397448
```

### Quick Calculations

```
C-x * q    ; Quick calc (minibuffer)
M-x quick-calc
```

### Graph Integration

Calc integrates with GNUPLOT for visualization:

```
' sin(x) RET        ; Define function
g f                 ; Fast plot
g a                 ; Add to plot
g p                 ; Print/save plot
```

## Design Philosophy

### Comprehensiveness

Calc aims to be a complete mathematical environment:
- "Do everything a scientific calculator can do, and much more"
- Support for diverse mathematical domains
- Extensible architecture for user additions

### Precision and Correctness

- Arbitrary precision by default
- Exact arithmetic when possible
- Clear distinction between exact and approximate
- Comprehensive error handling

### Integration

- Deep integration with Emacs
- Embedded mode for document calculations
- Trail for reproducibility
- Language modes for various syntaxes

### Discoverability

- Extensive help system (`h i` for manual)
- Tutorial mode
- Progressive disclosure (basic → advanced)
- Mnemonic key bindings

## Historical Note

From the file headers, Calc was created by David Gillespie while at Caltech, later at Synaptics. It represents one of the most comprehensive computer algebra systems available in any text editor, rivaling standalone systems like Mathematica, Maple, and Maxima in many capabilities.

The TODO section in `calc.el` (lines 47-137) reveals ongoing development priorities:
- Improved rewrite mechanisms
- Matrix eigenvalues and SVD
- Better numerical integration
- Enhanced TeX parsing
- More tutorial examples
- Spreadsheet-like features

## Summary

Calc demonstrates several advanced Emacs programming techniques:

1. **Modular architecture**: 43 files with clear separation of concerns
2. **Lazy loading**: Sophisticated autoload system for fast startup
3. **Type system**: Rich internal representation with normalization
4. **DSL integration**: Multiple external language syntaxes
5. **Symbolic computation**: Full algebraic manipulation
6. **Numerical methods**: Arbitrary precision with performance optimization
7. **User extensibility**: Multiple extension mechanisms
8. **Mode integration**: Stack, algebraic, embedded, and keypad modes

The codebase showcases Lisp's strengths in symbolic computation while maintaining practical performance through careful optimization. It remains one of Emacs's most sophisticated subsystems, providing professional-grade mathematical capabilities within the editor.

---

**Key Files Reference**:
- `/home/user/emacs/lisp/calc/calc.el` - Core system (3,532 lines)
- `/home/user/emacs/lisp/calc/calc-ext.el` - Extension loader (3,434 lines)
- `/home/user/emacs/lisp/calc/calc-arith.el` - Arithmetic (3,067 lines)
- `/home/user/emacs/lisp/calc/calcalg2.el` - Calculus (3,682 lines)
- `/home/user/emacs/lisp/calc/calc-units.el` - Units (2,390 lines)
- `/home/user/emacs/lisp/calc/calc-lang.el` - Languages (2,691 lines)
- `/home/user/emacs/lisp/calc/calc-prog.el` - Programming (2,190 lines)
