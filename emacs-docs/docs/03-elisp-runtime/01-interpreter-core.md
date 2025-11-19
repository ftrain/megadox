# Emacs Lisp Interpreter Core

**A Literate Programming Guide to the Emacs Lisp Runtime**

This document provides an in-depth exploration of the Emacs Lisp interpreter's core implementation, tracing how Lisp expressions are read, evaluated, and executed through three different execution models: interpreted code, bytecode, and native compilation.

---

## Table of Contents

1. [Fundamental Data Structures](#1-fundamental-data-structures)
2. [The Lisp Object System](#2-the-lisp-object-system)
3. [Reading Lisp Code](#3-reading-lisp-code)
4. [The Evaluation Engine](#4-the-evaluation-engine)
5. [Function Application](#5-function-application)
6. [Bytecode Execution](#6-bytecode-execution)
7. [Native Compilation](#7-native-compilation)
8. [Scoping and Closures](#8-scoping-and-closures)
9. [Special Forms and Macros](#9-special-forms-and-macros)
10. [Design Tradeoffs](#10-design-tradeoffs)

---

## 1. Fundamental Data Structures

### 1.1 Lisp_Object: The Universal Type

At the heart of Emacs Lisp is `Lisp_Object`, a tagged pointer that can represent any Lisp value. This is the fundamental type that flows through the entire interpreter.

**Location**: `/home/user/emacs/src/lisp.h:602-611`

```c
#ifdef CHECK_LISP_OBJECT_TYPE
typedef struct Lisp_Object { Lisp_Word i; } Lisp_Object;
# define LISP_OBJECT_IS_STRUCT
# define LISP_INITIALLY(w) {w}
#else
typedef Lisp_Word Lisp_Object;
# define LISP_INITIALLY(w) (w)
#endif
```

**Key Insight**: `Lisp_Object` is either a bare integer (`Lisp_Word`) or a struct wrapping it (when type checking is enabled). This allows maximum performance in production while enabling type safety during development.

### 1.2 Tagged Pointer Architecture

Emacs uses a sophisticated tagging scheme to encode type information in the low bits of pointers. With 8-byte alignment on modern systems, the bottom 3 bits are always zero in valid pointers, allowing us to store type tags there.

**Location**: `/home/user/emacs/src/lisp.h:499-536`

```c
/* Lisp_Object tagging scheme:
        Tag location
   Upper bits  Lower bits  Type        Payload
   000.......  .......000  symbol      offset from lispsym to struct Lisp_Symbol
   001.......  .......001  unused
   01........  ........10  fixnum      signed integer of FIXNUM_BITS
   110.......  .......011  cons        pointer to struct Lisp_Cons
   100.......  .......100  string      pointer to struct Lisp_String
   101.......  .......101  vectorlike  pointer to union vectorlike_header
   111.......  .......111  float       pointer to struct Lisp_Float  */

enum Lisp_Type
  {
    Lisp_Symbol = 0,
    Lisp_Type_Unused0 = 1,
    Lisp_Int0 = 2,
    Lisp_Int1 = USE_LSB_TAG ? 6 : 3,
    Lisp_String = 4,
    Lisp_Vectorlike = 5,
    Lisp_Cons = USE_LSB_TAG ? 3 : 6,
    Lisp_Float = 7
  };
```

**Design Tradeoff**: This scheme gives us:
- **Fast type checking**: Just mask and compare bits
- **Immediate integers**: Small integers don't require heap allocation
- **Compact representation**: No space overhead for type tags

The cost is that we lose 3 bits of address space (or integer range), but on 64-bit systems this is negligible.

### 1.3 The Symbol Structure

Symbols are fundamental to Lisp. They serve as variable names, function names, and keys in property lists.

**Location**: `/home/user/emacs/src/lisp.h:797-840`

```c
struct Lisp_Symbol
{
  union
  {
    struct
    {
      bool_bf gcmarkbit : 1;

      /* Indicates where the value can be found.  */
      ENUM_BF (symbol_redirect) redirect : 2;

      ENUM_BF (symbol_trapped_write) trapped_write : 2;

      /* Interned state of the symbol.  */
      ENUM_BF (symbol_interned) interned : 2;

      /* True means that this variable has been explicitly declared
         special (with `defvar' etc), and shouldn't be lexically bound.  */
      bool_bf declared_special : 1;

      /* The symbol's name, as a Lisp string.  */
      Lisp_Object name;

      /* Value of the symbol or Qunbound if unbound.  Which alternative of the
         union is used depends on the `redirect' field above.  */
      union {
        Lisp_Object value;
        struct Lisp_Symbol *alias;
        struct Lisp_Buffer_Local_Value *blv;
        lispfwd fwd;
      } val;

      /* Function value of the symbol or Qnil if not fboundp.  */
      Lisp_Object function;

      /* The symbol's property list.  */
      Lisp_Object plist;

      /* Next symbol in obarray bucket, if the symbol is interned.  */
      struct Lisp_Symbol *next;
    } s;
    GCALIGNED_UNION_MEMBER
  } u;
};
```

**Key Features**:
1. **Separate namespaces**: `function` vs `value` slots implement Lisp-2 semantics
2. **Property lists**: Extensible metadata via `plist`
3. **Symbol interning**: Hash table chaining via `next`
4. **Dynamic/forwarded variables**: The `redirect` field allows symbols to point to:
   - Regular Lisp values (`SYMBOL_PLAINVAL`)
   - Other symbols (aliases via `SYMBOL_VARALIAS`)
   - Buffer-local values (`SYMBOL_LOCALIZED`)
   - C variables (`SYMBOL_FORWARDED`)

---

## 2. The Lisp Object System

### 2.1 Type Predicates and Extraction

The tagged pointer system enables fast type checking through bit masking:

**Location**: `/home/user/emacs/src/lisp.h:399-417`

```c
#define lisp_h_CONSP(x) TAGGEDP (x, Lisp_Cons)
#define lisp_h_FLOATP(x) TAGGEDP (x, Lisp_Float)
#define lisp_h_NILP(x)  BASE_EQ (x, Qnil)
#define lisp_h_BARE_SYMBOL_P(x) TAGGEDP (x, Lisp_Symbol)

#define lisp_h_TAGGEDP(a, tag) \
   (! (((unsigned) (XLI (a) >> (USE_LSB_TAG ? 0 : VALBITS)) \
        - (unsigned) (tag)) \
       & ((1 << GCTYPEBITS) - 1)))

#define lisp_h_VECTORLIKEP(x) TAGGEDP (x, Lisp_Vectorlike)
#define lisp_h_XCAR(c) XCONS (c)->u.s.car
#define lisp_h_XCDR(c) XCONS (c)->u.s.u.cdr
```

**Example: Type Checking `(foo . bar)`**

```c
Lisp_Object cons = /* ... */;

// Fast inline check - just a bit mask and comparison
if (CONSP(cons)) {
    Lisp_Object car = XCAR(cons);  // No overhead, just pointer arithmetic
    Lisp_Object cdr = XCDR(cons);
}
```

### 2.2 Integer Representation

Fixnums (small integers) are represented directly in the `Lisp_Object`, using two tag values to gain an extra bit of range.

**Location**: `/home/user/emacs/src/lisp.h:402-406,432-433`

```c
#define lisp_h_FIXNUMP(x) \
   (! (((unsigned) (XLI (x) >> (USE_LSB_TAG ? 0 : FIXNUM_BITS)) \
        - (unsigned) (Lisp_Int0 >> !USE_LSB_TAG)) \
       & ((1 << INTTYPEBITS) - 1)))

#if USE_LSB_TAG
# define lisp_h_XFIXNUM_RAW(a) (XLI (a) >> INTTYPEBITS)
# define lisp_h_XTYPE(a) ((enum Lisp_Type) (XLI (a) & ~VALMASK))
#endif
```

On a 64-bit system with LSB tagging:
- **Fixnum range**: 61 bits (one sign bit + 60 value bits)
- **Tag bits**: 3 bits
- **Two tags**: `Lisp_Int0` (tag=2) and `Lisp_Int1` (tag=6) give us one extra bit

---

## 3. Reading Lisp Code

### 3.1 The Lisp Reader

The reader transforms textual S-expressions into internal Lisp_Object structures.

**Location**: `/home/user/emacs/src/lread.c:1-200`

The reader handles:
- **Symbols**: Interned into the obarray
- **Lists**: Cons cells chained together
- **Literals**: Numbers, strings, vectors
- **Special syntax**: `#n=` and `#n#` for circular structures, reader macros

**Key Variables**:
```c
/* The objects or placeholders read with the #n=object form. */
static Lisp_Object read_objects_map;

/* The recursive objects read with the #n=object form. */
static Lisp_Object read_objects_completed;
```

### 3.2 The Obarray: Symbol Interning

The obarray is a hash table that ensures symbol uniqueness - reading `'foo` twice yields the same symbol object.

**Location**: `/home/user/emacs/src/lread.c:4639-4706`

```c
static Lisp_Object initial_obarray;

/* Intern a symbol into the obarray */
static void
intern_sym (Lisp_Object sym, Lisp_Object obarray, Lisp_Object index)
{
  struct Lisp_Symbol *s = XBARE_SYMBOL (sym);
  s->u.s.interned = (BASE_EQ (obarray, initial_obarray)
                     ? SYMBOL_INTERNED_IN_INITIAL_OBARRAY
                     : SYMBOL_INTERNED);

  /* Keywords (symbols starting with ':') are self-evaluating */
  if (SREF (s->u.s.name, 0) == ':' && BASE_EQ (obarray, initial_obarray))
    {
      s->u.s.trapped_write = SYMBOL_NOWRITE;
      SET_SYMBOL_VAL (s, sym);
    }

  struct Lisp_Obarray *o = XOBARRAY (obarray);
  /* ... chain symbol into hash bucket ... */
}
```

**Process**:
1. Hash the symbol name
2. Look up in obarray bucket
3. If found, return existing symbol
4. If not found, create new symbol and intern it

---

## 4. The Evaluation Engine

### 4.1 The eval_sub Function

This is the core of the interpreter - the function that evaluates Lisp expressions.

**Location**: `/home/user/emacs/src/eval.c:2548-2767`

```c
/* Eval a sub-expression of the current expression (i.e. in the same
   lexical scope).  */
Lisp_Object
eval_sub (Lisp_Object form)
{
  if (SYMBOLP (form))
    {
      /* Look up its binding in the lexical environment.
         We do not pay attention to the declared_special flag here, since we
         already did that when let-binding the variable.  */
      Lisp_Object lex_binding
        = Fassq (form, Vinternal_interpreter_environment);
      return !NILP (lex_binding) ? XCDR (lex_binding) : Fsymbol_value (form);
    }

  if (!CONSP (form))
    return form;  // Self-evaluating: numbers, strings, vectors, etc.

  maybe_quit ();
  maybe_gc ();

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
        max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
        xsignal1 (Qexcessive_lisp_nesting, make_fixnum (lisp_eval_depth));
    }

  Lisp_Object original_fun = XCAR (form);
  Lisp_Object original_args = XCDR (form);
  CHECK_LIST (original_args);

  /* Record in backtrace for debugging */
  specpdl_ref count
    = record_in_backtrace (original_fun, &original_args, UNEVALLED);

  /* ... (continues with function dispatch) ... */
```

**Evaluation Steps**:

1. **Symbols**: Look up in lexical environment, then dynamic (symbol value cell)
2. **Self-evaluating**: Numbers, strings, keywords return themselves
3. **Lists**: Function application
   - Extract function and arguments
   - Resolve indirection (symbol to function)
   - Dispatch based on function type

### 4.2 Function Dispatch

**Location**: `/home/user/emacs/src/eval.c:2597-2767`

```c
retry:
  /* Optimize for no indirection.  */
  fun = original_fun;
  if (!SYMBOLP (fun))
    fun = Ffunction (list1 (fun));
  else if (!NILP (fun) && (fun = XSYMBOL (fun)->u.s.function, SYMBOLP (fun)))
    fun = indirect_function (fun);

  if (SUBRP (fun) && !NATIVE_COMP_FUNCTION_DYNP (fun))
    {
      /* Built-in function (implemented in C) */
      Lisp_Object args_left = original_args;
      ptrdiff_t numargs = list_length (args_left);

      /* Check arity */
      if (numargs < XSUBR (fun)->min_args
          || (XSUBR (fun)->max_args >= 0
              && XSUBR (fun)->max_args < numargs))
        xsignal2 (Qwrong_number_of_arguments, original_fun,
                  make_fixnum (numargs));

      else if (XSUBR (fun)->max_args == UNEVALLED)
        /* Special form - pass arguments UNEVALUATED */
        val = (XSUBR (fun)->function.aUNEVALLED) (args_left);
      else if (XSUBR (fun)->max_args == MANY
               || XSUBR (fun)->max_args > 8)
        {
          /* Evaluate all arguments into a vector */
          SAFE_ALLOCA_LISP (vals, numargs);

          while (CONSP (args_left) && argnum < numargs)
            {
              Lisp_Object arg = XCAR (args_left);
              args_left = XCDR (args_left);
              vals[argnum++] = eval_sub (arg);  // RECURSIVE CALL
            }

          val = XSUBR (fun)->function.aMANY (argnum, vals);
        }
      else
        {
          /* Fixed arity (0-8 args) - optimized path */
          int i, maxargs = XSUBR (fun)->max_args;

          for (i = 0; i < maxargs; i++)
            {
              argvals[i] = eval_sub (Fcar (args_left));  // RECURSIVE
              args_left = Fcdr (args_left);
            }

          switch (i)
            {
            case 0: val = (XSUBR (fun)->function.a0 ()); break;
            case 1: val = (XSUBR (fun)->function.a1 (argvals[0])); break;
            case 2: val = (XSUBR (fun)->function.a2 (argvals[0], argvals[1])); break;
            // ... cases 3-8 ...
            }
        }
    }
  else if (CLOSUREP (fun)
           || NATIVE_COMP_FUNCTION_DYNP (fun)
           || MODULE_FUNCTIONP (fun))
    return apply_lambda (fun, original_args, count);
  else
    {
      if (NILP (fun))
        xsignal1 (Qvoid_function, original_fun);
      if (!CONSP (fun))
        xsignal1 (Qinvalid_function, original_fun);

      Lisp_Object funcar = XCAR (fun);
      if (EQ (funcar, Qautoload))
        {
          Fautoload_do_load (fun, original_fun, Qnil);
          goto retry;
        }
      if (EQ (funcar, Qmacro))
        {
          /* Macro expansion */
          Lisp_Object exp = apply1 (Fcdr (fun), original_args);
          val = eval_sub (exp);  // Evaluate the expansion
        }
      else if (EQ (funcar, Qlambda))
        return apply_lambda (fun, original_args, count);
      else
        xsignal1 (Qinvalid_function, original_fun);
    }
```

**Design Insight**: The evaluation loop has multiple levels of optimization:

1. **Inline for common arities**: Functions with 0-8 args get specialized code paths
2. **UNEVALLED for special forms**: Skip argument evaluation
3. **Symbol indirection caching**: `indirect_function` to resolve aliases
4. **Tail call to apply_lambda**: Let lambda application handle its own evaluation

### 4.3 Example: Evaluating `(+ 1 2)`

Let's trace through the evaluation step by step:

```lisp
(+ 1 2)
```

1. **Enter eval_sub**:
   - `form = '(+ 1 2)` (a cons cell)
   - Not a symbol, not self-evaluating
   - It's a list, so it's a function call

2. **Extract components**:
   - `original_fun = '+`
   - `original_args = '(1 2)`

3. **Resolve function**:
   - `+` is a symbol
   - Look up its function cell: `XSYMBOL('+')->u.s.function`
   - Returns a SUBR (built-in function)

4. **Dispatch to SUBR**:
   - Count args: 2
   - Check arity: min=0, max=MANY (unlimited)
   - Path: MANY

5. **Evaluate arguments**:
   ```c
   vals[0] = eval_sub (1);  // Returns 1 (self-evaluating)
   vals[1] = eval_sub (2);  // Returns 2 (self-evaluating)
   ```

6. **Call C function**:
   ```c
   val = XSUBR(fun)->function.aMANY(2, vals);
   // Calls Fplus(2, {1, 2}) in src/data.c
   ```

7. **Return result**: `3`

---

## 5. Function Application

### 5.1 The DEFUN Macro

Built-in functions are defined with the `DEFUN` macro, which creates a `Lisp_Subr` structure.

**Location**: `/home/user/emacs/src/lisp.h:3458-3465`

```c
#define DEFUN(lname, fnname, sname, minargs, maxargs, intspec, doc) \
  SUBR_SECTION_ATTRIBUTE                                            \
  static union Aligned_Lisp_Subr sname =                            \
     {{{ PVEC_SUBR << PSEUDOVECTOR_AREA_BITS },                    \
       { .a ## maxargs = fnname },                                  \
       minargs, maxargs, lname, {intspec}, lisp_h_Qnil}};          \
   Lisp_Object fnname
```

**The Lisp_Subr Structure**:

**Location**: `/home/user/emacs/src/lisp.h:2186-2219`

```c
struct Lisp_Subr
  {
    union vectorlike_header header;
    union {
      Lisp_Object (*a0) (void);
      Lisp_Object (*a1) (Lisp_Object);
      Lisp_Object (*a2) (Lisp_Object, Lisp_Object);
      Lisp_Object (*a3) (Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*a4) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*a5) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*a6) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*a7) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*a8) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*aUNEVALLED) (Lisp_Object args);
      Lisp_Object (*aMANY) (ptrdiff_t, Lisp_Object *);
    } function;
    short min_args, max_args;
    const char *symbol_name;
    /* ... documentation, interactive spec, etc. ... */
  };
```

**Example: Defining `eq`**:

**Location**: `/home/user/emacs/src/data.c:168-176`

```c
DEFUN ("eq", Feq, Seq, 2, 2, 0,
       doc: /* Return t if the two args are the same Lisp object.  */
       attributes: const)
  (Lisp_Object obj1, Lisp_Object obj2)
{
  if (EQ (obj1, obj2))
    return Qt;
  return Qnil;
}
```

This expands to:
```c
static union Aligned_Lisp_Subr Seq =
  {{{ PVEC_SUBR << PSEUDOVECTOR_AREA_BITS },
    { .a2 = Feq },           // Function pointer for 2-arg function
    2, 2,                     // min_args=2, max_args=2
    "eq", {NULL}, Qnil}};

Lisp_Object Feq(Lisp_Object obj1, Lisp_Object obj2)
{
  if (EQ (obj1, obj2))
    return Qt;
  return Qnil;
}
```

### 5.2 Funcall: The Direct Call Mechanism

`funcall` calls a function with already-evaluated arguments, skipping the evaluation step.

**Location**: `/home/user/emacs/src/eval.c:3151-3184`

```c
DEFUN ("funcall", Ffuncall, Sfuncall, 1, MANY, 0,
       doc: /* Call first argument as a function, passing remaining arguments to it.
Return the value that function returns.
Thus, (funcall \\='cons \\='x \\='y) returns (x . y).
usage: (funcall FUNCTION &rest ARGUMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count;

  maybe_quit ();

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
        max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
        xsignal1 (Qexcessive_lisp_nesting, make_fixnum (lisp_eval_depth));
    }

  count = record_in_backtrace (args[0], &args[1], nargs - 1);

  maybe_gc ();

  if (debug_on_next_call)
    do_debug_on_call (Qlambda, count);

  Lisp_Object val = funcall_general (args[0], nargs - 1, args + 1);

  lisp_eval_depth--;
  if (backtrace_debug_on_exit (specpdl_ref_to_ptr (count)))
    val = call_debugger (list2 (Qexit, val));
  specpdl_ptr--;
  return val;
}
```

**funcall_general** dispatches based on the function type:

**Location**: `/home/user/emacs/src/eval.c:3115-3149`

```c
Lisp_Object
funcall_general (Lisp_Object fun, ptrdiff_t numargs, Lisp_Object *args)
{
  Lisp_Object original_fun = fun;
 retry:
  if (SYMBOLP (fun) && !NILP (fun)
      && (fun = XSYMBOL (fun)->u.s.function, SYMBOLP (fun)))
    fun = indirect_function (fun);

  if (SUBRP (fun) && !NATIVE_COMP_FUNCTION_DYNP (fun))
    return funcall_subr (XSUBR (fun), numargs, args);
  else if (CLOSUREP (fun)
           || NATIVE_COMP_FUNCTION_DYNP (fun)
           || MODULE_FUNCTIONP (fun))
    return funcall_lambda (fun, numargs, args);
  else
    {
      if (NILP (fun))
        xsignal1 (Qvoid_function, original_fun);
      if (!CONSP (fun))
        xsignal1 (Qinvalid_function, original_fun);
      Lisp_Object funcar = XCAR (fun);
      if (!SYMBOLP (funcar))
        xsignal1 (Qinvalid_function, original_fun);
      if (EQ (funcar, Qlambda))
        return funcall_lambda (fun, numargs, args);
      else if (EQ (funcar, Qautoload))
        {
          Fautoload_do_load (fun, original_fun, Qnil);
          fun = original_fun;
          goto retry;
        }
      else
        xsignal1 (Qinvalid_function, original_fun);
    }
}
```

### 5.3 Apply: Spreading Argument Lists

`apply` is like `funcall`, but its last argument is a list that gets spread out.

**Location**: `/home/user/emacs/src/eval.c:2769-2838`

```c
DEFUN ("apply", Fapply, Sapply, 1, MANY, 0,
       doc: /* Call FUNCTION with our remaining args, using our last arg as list of args.
Then return the value FUNCTION returns.
With a single argument, call the argument's first element using the
other elements as args.
Thus, (apply \\='+ 1 2 \\='(3 4)) returns 10.
usage: (apply FUNCTION &rest ARGUMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i, funcall_nargs;
  Lisp_Object *funcall_args = NULL;
  Lisp_Object spread_arg = args[nargs - 1];  // Last arg is the list
  Lisp_Object fun = args[0];
  USE_SAFE_ALLOCA;

  ptrdiff_t numargs = list_length (spread_arg);

  if (numargs == 0)
    return Ffuncall (max (1, nargs - 1), args);
  else if (numargs == 1)
    {
      args [nargs - 1] = XCAR (spread_arg);
      return Ffuncall (nargs, args);
    }

  numargs += nargs - 2;  // Total args = direct args + spread list length

  /* ... optimization for SUBRs with fixed max_args ... */

  SAFE_ALLOCA_LISP (funcall_args, 1 + numargs);
  funcall_nargs = 1 + numargs;

  memcpy (funcall_args, args, nargs * word_size);

  /* Spread the last arg */
  i = nargs - 1;
  while (!NILP (spread_arg))
    {
      funcall_args [i++] = XCAR (spread_arg);
      spread_arg = XCDR (spread_arg);
    }

  Lisp_Object retval = Ffuncall (funcall_nargs, funcall_args);

  SAFE_FREE ();
  return retval;
}
```

**Example**:
```lisp
(apply #'+ 1 2 '(3 4))
;; Transforms to: (funcall #'+ 1 2 3 4)
;; Returns: 10
```

---

## 6. Bytecode Execution

### 6.1 Why Bytecode?

Interpreted Lisp (walking the AST) is slow. Native compilation is fast but has high latency. Bytecode offers a middle ground:

- **Faster than interpretation**: Pre-compiled to a compact instruction stream
- **Smaller than native code**: More cache-friendly
- **Quick to load**: No JIT compilation overhead

### 6.2 Bytecode Structure

A byte-compiled function is represented as a closure with:

**Location**: `/home/user/emacs/src/eval.c:3329-3343`

```c
else if (CLOSUREP (fun))
  {
    syms_left = AREF (fun, CLOSURE_ARGLIST);
    /* Bytecode objects using lexical binding have an integral
       ARGLIST slot value: pass the arguments to the byte-code
       engine directly.  */
    if (FIXNUMP (syms_left))
      return exec_byte_code (fun, XFIXNUM (syms_left), nargs, arg_vector);
    /* Otherwise the closure either is interpreted
       or uses dynamic binding and the ARGLIST slot contains a standard
       formal argument list whose variables are bound dynamically below.  */
    lexenv = CONSP (AREF (fun, CLOSURE_CODE))
             ? AREF (fun, CLOSURE_CONSTANTS)
             : Qnil;
  }
```

A compiled function closure has:
- **CLOSURE_CODE**: The bytecode string
- **CLOSURE_CONSTANTS**: Vector of constants
- **CLOSURE_ARGLIST**: Either a formal parameter list or an encoded arity

### 6.3 The Bytecode Interpreter

**Location**: `/home/user/emacs/src/bytecode.c:481-500`

```c
/* Execute the byte-code in FUN.  ARGS_TEMPLATE is the function arity
   encoded as an integer (the one in FUN is ignored), and ARGS, of
   size NARGS, should be a vector of the actual arguments.  The
   arguments in ARGS are pushed on the stack according to
   ARGS_TEMPLATE before executing FUN.  */

Lisp_Object
exec_byte_code (Lisp_Object fun, ptrdiff_t args_template,
                ptrdiff_t nargs, Lisp_Object *args)
{
  unsigned char quitcounter = 1;
  struct bc_thread_state *bc = &current_thread->bc;

  /* Values used for the first stack record when called from C.  */
  register Lisp_Object *top BC_REG_TOP = NULL;
  register unsigned char const *pc BC_REG_PC = NULL;

  Lisp_Object bytestr = AREF (fun, CLOSURE_CODE);

  /* ... setup bytecode stack frame ... */
```

### 6.4 Bytecode Stack Architecture

The bytecode interpreter uses a separate stack for performance:

**Location**: `/home/user/emacs/src/bytecode.c:339-377`

```c
/* Bytecode interpreter stack:

           |--------------|         --
           |fun           |           |                   ^ stack growth
           |saved_pc      |           |                   | direction
           |saved_top    -------      |
     fp--->|saved_fp     ----   |     | current frame
           |--------------|  |  |     | (called from bytecode in this example)
           |   (free)     |  |  |     |
     top-->| ...stack...  |  |  |     |
           : ...          :  |  |     |
           |incoming args |  |  |     |
           |--------------|  |  |   --
           |fun           |  |  |     |
           |saved_pc      |  |  |     |
           |saved_top     |  |  |     |
           |saved_fp      |<-   |     | previous frame
           |--------------|     |     |
           |   (free)     |     |     |
           | ...stack...  |<----      |
           : ...          :           |
           |incoming args |           |
           |--------------|         --
           :              :
*/

struct bc_frame {
  struct bc_frame *saved_fp;        /* previous frame pointer */
  Lisp_Object *saved_top;           /* previous stack pointer */
  const unsigned char *saved_pc;    /* previous program counter */
  Lisp_Object fun;                  /* current function object */
  Lisp_Object next_stack[];         /* data stack of next frame */
};
```

**Design Choice**: A separate stack for bytecode (instead of using the C stack) allows:
- **Faster function calls**: No C calling convention overhead
- **Tail call optimization**: Can reuse stack frames
- **Better GC integration**: Precise scanning of Lisp objects

### 6.5 Sample Bytecode Operations

**Location**: `/home/user/emacs/src/bytecode.c:73-200`

```c
#define BYTE_CODES                                                      \
DEFINE (Bstack_ref, 0)   /* reference stack[n] */                      \
DEFINE (Bvarref, 010)    /* varref symbol in constants[n] */           \
DEFINE (Bvarset, 020)    /* varset symbol in constants[n] */           \
DEFINE (Bvarbind, 030)   /* bind symbol in constants[n] */             \
DEFINE (Bcall, 040)      /* call function with n args from stack */    \
DEFINE (Bunbind, 050)    /* unbind n local variables */                \
                                                                        \
DEFINE (Bpushconditioncase, 061)  /* push condition handler */         \
DEFINE (Bpushcatch, 062)          /* push catch tag */                 \
                                                                        \
DEFINE (Bcar, 0100)      /* (car top) */                               \
DEFINE (Bcdr, 0101)      /* (cdr top) */                               \
DEFINE (Bcons, 0102)     /* (cons top top-1) */                        \
DEFINE (Blist1, 0103)    /* (list top) */                              \
/* ... many more opcodes ... */
```

**Example: Bytecode for `(+ x 1)`**

Assuming `x` is lexically bound:
```
Bstack-ref 0     ; Push x onto stack
Bconstant 1      ; Push constant 1
Bplus            ; Call + with 2 args from stack
Breturn          ; Return top of stack
```

---

## 7. Native Compilation

### 7.1 Overview

Emacs 28+ supports native compilation via libgccjit, compiling Lisp to native machine code.

**Location**: `/home/user/emacs/src/comp.c:1-200`

```c
/* Compile Emacs Lisp into native code.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.

Author: Andrea Corallo <acorallo@gnu.org>

This file is part of GNU Emacs.
*/

#include <config.h>
#include "lisp.h"

#ifdef HAVE_NATIVE_COMP

#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <libgccjit.h>
```

### 7.2 Native Compiled Functions

Native functions use the same `Lisp_Subr` structure but with additional metadata:

**Location**: `/home/user/emacs/src/lisp.h:2213-2218`

```c
#ifdef HAVE_NATIVE_COMP
    Lisp_Object native_comp_u;    /* Compilation unit */
    char *native_c_name;           /* Name in native code */
    Lisp_Object lambda_list;       /* Original parameter list */
    Lisp_Object type;              /* Type information */
#endif
```

Native functions are called through the same `funcall_lambda` path, but execution jumps directly to machine code:

**Location**: `/home/user/emacs/src/eval.c:3348-3354`

```c
#ifdef HAVE_NATIVE_COMP
  else if (NATIVE_COMP_FUNCTION_DYNP (fun))
    {
      syms_left = XSUBR (fun)->lambda_list;
      lexenv = Qnil;
    }
#endif
```

### 7.3 Advantages and Tradeoffs

**Advantages**:
- **10x+ speedup**: Native code is much faster than bytecode
- **Type specialization**: Can optimize for specific types
- **Inlining**: Cross-function optimization

**Tradeoffs**:
- **Compilation latency**: Initial compile can take seconds
- **Disk space**: Native code is larger than bytecode
- **Complexity**: Debugging is harder

---

## 8. Scoping and Closures

### 8.1 Lexical vs. Dynamic Scoping

Emacs Lisp supports both:

- **Lexical scoping** (modern, recommended): Variables capture their definition-time environment
- **Dynamic scoping** (legacy): Variables look up the latest binding at runtime

**Location**: `/home/user/emacs/src/eval.c:2514-2528`

```c
DEFUN ("eval", Feval, Seval, 1, 2, 0,
       doc: /* Evaluate FORM and return its value.
If LEXICAL is `t', evaluate using lexical binding by default.
This is the recommended value.

If absent or `nil', use dynamic scoping only.

LEXICAL can also represent an actual lexical environment; see the Info
node `(elisp)Eval' for details.  */)
  (Lisp_Object form, Lisp_Object lexical)
{
  specpdl_ref count = SPECPDL_INDEX ();
  specbind (Qinternal_interpreter_environment,
            CONSP (lexical) || NILP (lexical) ? lexical : list_of_t);
  return unbind_to (count, eval_sub (form));
}
```

The lexical environment is stored in `Vinternal_interpreter_environment`, which is an association list of `(symbol . value)` pairs.

### 8.2 Variable Lookup

**Location**: `/home/user/emacs/src/eval.c:2553-2560`

```c
if (SYMBOLP (form))
  {
    /* Look up its binding in the lexical environment.
       We do not pay attention to the declared_special flag here, since we
       already did that when let-binding the variable.  */
    Lisp_Object lex_binding
      = Fassq (form, Vinternal_interpreter_environment);
    return !NILP (lex_binding) ? XCDR (lex_binding) : Fsymbol_value (form);
  }
```

**Lookup Order**:
1. Check lexical environment (alist lookup)
2. If not found, check symbol's value cell (dynamic binding)

### 8.3 Creating Closures: funcall_lambda

**Location**: `/home/user/emacs/src/eval.c:3316-3421`

```c
static Lisp_Object
funcall_lambda (Lisp_Object fun, ptrdiff_t nargs, Lisp_Object *arg_vector)
{
  Lisp_Object syms_left, lexenv;

  if (CONSP (fun))
    {
      /* Interpreted lambda */
      lexenv = Qnil;
      syms_left = XCDR (fun);
      if (CONSP (syms_left))
        syms_left = XCAR (syms_left);  // Parameter list
      else
        xsignal1 (Qinvalid_function, fun);
    }
  else if (CLOSUREP (fun))
    {
      syms_left = AREF (fun, CLOSURE_ARGLIST);
      /* Bytecode objects using lexical binding have an integral
         ARGLIST slot value */
      if (FIXNUMP (syms_left))
        return exec_byte_code (fun, XFIXNUM (syms_left), nargs, arg_vector);
      /* Otherwise the closure either is interpreted
         or uses dynamic binding */
      lexenv = CONSP (AREF (fun, CLOSURE_CODE))
               ? AREF (fun, CLOSURE_CONSTANTS)
               : Qnil;
    }

  /* Bind parameters to arguments */
  specpdl_ref count = SPECPDL_INDEX ();
  ptrdiff_t i = 0;
  bool optional = false;
  bool rest = false;
  bool previous_rest = false;

  for (; CONSP (syms_left); syms_left = XCDR (syms_left))
    {
      Lisp_Object next = XCAR (syms_left);

      if (BASE_EQ (next, Qand_rest))
        {
          rest = 1;
          previous_rest = true;
        }
      else if (BASE_EQ (next, Qand_optional))
        optional = 1;
      else
        {
          Lisp_Object arg;
          if (rest)
            {
              arg = Flist (nargs - i, &arg_vector[i]);
              i = nargs;
            }
          else if (i < nargs)
            arg = arg_vector[i++];
          else if (!optional)
            xsignal2 (Qwrong_number_of_arguments, fun, make_fixnum (nargs));
          else
            arg = Qnil;

          /* Bind the argument.  */
          if (!NILP (lexenv))
            /* Lexically bind NEXT by adding it to the lexenv alist.  */
            lexenv = Fcons (Fcons (next, arg), lexenv);
          else
            /* Dynamically bind NEXT.  */
            specbind (next, arg);
          previous_rest = false;
        }
    }

  if (!BASE_EQ (lexenv, Vinternal_interpreter_environment))
    /* Instantiate a new lexical environment.  */
    specbind (Qinternal_interpreter_environment, lexenv);

  Lisp_Object val;
  if (CONSP (fun))
    val = Fprogn (XCDR (XCDR (fun)));  // Evaluate body
  /* ... bytecode and native paths ... */

  return unbind_to (count, val);
}
```

**Key Points**:

1. **Parameter binding**: Match formal parameters to actual arguments
2. **Lexical binding**: Extends the environment alist
3. **Dynamic binding**: Uses the `specpdl` (special variable bindings stack)
4. **&optional and &rest**: Special parameter list markers
5. **Cleanup**: `unbind_to` restores previous bindings

### 8.4 The Specpdl: Dynamic Binding Stack

The `specpdl` (special bindings stack) tracks dynamic variable bindings for cleanup.

**Location**: `/home/user/emacs/src/eval.c:3617-3676`

```c
void
specbind (Lisp_Object symbol, Lisp_Object value)
{
  struct Lisp_Symbol *sym = XBARE_SYMBOL (symbol);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS:
      sym = SYMBOL_ALIAS (sym);
      XSETSYMBOL (symbol, sym);
      goto start;

    case SYMBOL_PLAINVAL:
      /* The most common case is that of a non-constant symbol with a
         trivial value.  Make that as fast as we can.  */
      specpdl_ptr->let.kind = SPECPDL_LET;
      specpdl_ptr->let.symbol = symbol;
      specpdl_ptr->let.old_value = SYMBOL_VAL (sym);
      specpdl_ptr->let.where.kbd = NULL;
      break;

    case SYMBOL_LOCALIZED:
    case SYMBOL_FORWARDED:
      {
        Lisp_Object ovalue = find_symbol_value (symbol);
        specpdl_ptr->let.kind = SPECPDL_LET_LOCAL;
        specpdl_ptr->let.symbol = symbol;
        specpdl_ptr->let.old_value = ovalue;
        specpdl_ptr->let.where.buf = Fcurrent_buffer ();

        /* Handle buffer-local variables */
        if (sym->u.s.redirect == SYMBOL_LOCALIZED)
          {
            if (!blv_found (SYMBOL_BLV (sym)))
              specpdl_ptr->let.kind = SPECPDL_LET_DEFAULT;
          }
        break;
      }
    default: emacs_abort ();
    }
  grow_specpdl ();
  do_specbind (sym, specpdl_ptr - 1, value, SET_INTERNAL_BIND);
}
```

**The specpdl is a stack of**:
- **Variable bindings** (`SPECPDL_LET`)
- **Unwind-protect handlers** (`SPECPDL_UNWIND`)
- **Catch tags** (`SPECPDL_CATCH`)
- **Condition handlers** (`SPECPDL_HANDLER`)

When a function returns or signals an error, `unbind_to` walks back the stack, restoring old values and calling cleanup functions.

---

## 9. Special Forms and Macros

### 9.1 Special Forms

Special forms are built-in functions that receive their arguments UNEVALUATED, allowing them to control evaluation.

**Location**: `/home/user/emacs/src/eval.c:387-402`

```c
DEFUN ("if", Fif, Sif, 2, UNEVALLED, 0,
       doc: /* If COND yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE's.
THEN must be one expression, but ELSE... can be zero or more expressions.
If COND yields nil, and there are no ELSE's, the value is nil.
usage: (if COND THEN ELSE...)  */)
  (Lisp_Object args)
{
  Lisp_Object cond;

  cond = eval_sub (XCAR (args));

  if (!NILP (cond))
    return eval_sub (Fcar (XCDR (args)));
  return Fprogn (Fcdr (XCDR (args)));
}
```

**Key**: `maxargs = UNEVALLED` means arguments arrive as a list, not evaluated. The special form controls when/if to call `eval_sub` on them.

**Other special forms**:
- **quote**: Returns argument without evaluating
- **function**: Returns function object
- **let**: Binds variables in a new scope
- **cond**: Multi-way conditional
- **while**: Looping
- **catch/throw**: Non-local exits

**Location**: `/home/user/emacs/src/eval.c:508-519`

```c
DEFUN ("quote", Fquote, Squote, 1, UNEVALLED, 0,
       doc: /* Return the argument, without evaluating it.  `(quote x)' yields `x'.
Warning: `quote' does not construct its return value, but just returns
the value that was pre-constructed by the Lisp reader...
usage: (quote ARG)  */)
  (Lisp_Object args)
{
  if (!NILP (XCDR (args)))
    xsignal2 (Qwrong_number_of_arguments, Qquote, Flist_length (args));
  return XCAR (args);
}
```

### 9.2 Macros

Macros are functions that return code to be evaluated.

**Location**: `/home/user/emacs/src/eval.c:2729-2754`

```c
if (EQ (funcar, Qmacro))
  {
    specpdl_ref count1 = SPECPDL_INDEX ();
    Lisp_Object exp;
    /* Bind lexical-binding during expansion of the macro, so the
       macro can know reliably if the code it outputs will be
       interpreted using lexical-binding or not.  */
    specbind (Qlexical_binding,
              NILP (Vinternal_interpreter_environment) ? Qnil : Qt);

    /* Make the macro aware of any defvar declarations in scope. */
    Lisp_Object dynvars = Vmacroexp__dynvars;
    for (Lisp_Object p = Vinternal_interpreter_environment;
         !NILP (p); p = XCDR(p))
      {
        Lisp_Object e = XCAR (p);
        if (SYMBOLP (e))
          dynvars = Fcons(e, dynvars);
      }
    if (!EQ (dynvars, Vmacroexp__dynvars))
      specbind (Qmacroexp__dynvars, dynvars);

    exp = apply1 (Fcdr (fun), original_args);
    exp = unbind_to (count1, exp);
    val = eval_sub (exp);  // Evaluate the macro expansion
  }
```

**Macro Expansion Process**:

1. **Detect macro**: Check if function is `(macro . function)`
2. **Call macro function**: Apply to UNEVALUATED arguments
3. **Evaluate expansion**: Call `eval_sub` on the result
4. **Recursive**: The expansion might contain more macro calls

**Example**:
```lisp
;; Macro definition
(defmacro when (cond &rest body)
  `(if ,cond (progn ,@body)))

;; Usage
(when (> x 0)
  (print "positive")
  (print x))

;; Expands to:
(if (> x 0)
    (progn
      (print "positive")
      (print x)))
```

At runtime, the evaluator:
1. Sees `(when ...)` is a macro
2. Calls the macro function with `((> x 0) (print "positive") (print x))`
3. Gets back `(if (> x 0) (progn ...))`
4. Evaluates that expansion

---

## 10. Design Tradeoffs

### 10.1 Three Execution Models

| Model | Speed | Startup | Memory | Use Case |
|-------|-------|---------|--------|----------|
| **Interpreted** | 1x | Instant | Low | Development, rarely-used code |
| **Bytecode** | 3-5x | Fast | Medium | Most Emacs code |
| **Native** | 10-20x | Slow (compile) | High | Hot paths, compute-heavy |

**Key Insight**: The interpreter can seamlessly call between all three:
- Bytecode can call interpreted functions
- Native code can call bytecode
- All share the same `funcall` interface

### 10.2 Tagged Pointers vs. Boxed Values

**Tagged pointers** (Emacs approach):
- **Pros**: Fast type checking, immediate integers, no allocation for fixnums
- **Cons**: Reduced integer range, complex pointer arithmetic

**Boxed values** (Python, Ruby):
- **Pros**: Simpler implementation, uniform representation
- **Cons**: Every integer is heap-allocated, more GC pressure

### 10.3 Separate Function Namespace (Lisp-2)

Emacs Lisp has separate namespaces for variables and functions (Lisp-2), unlike Scheme (Lisp-1).

**Pros**:
- Can have variable `list` and function `list` separately
- Closer to Common Lisp
- No accidental shadowing

**Cons**:
- Need `funcall` to call function-valued variables
- More complex scoping rules
- Harder to pass functions as arguments

**Example**:
```lisp
;; Lisp-2 (Emacs Lisp)
(let ((list '(1 2 3)))
  (list 4 5 6))  ; OK - function `list` != variable `list`
=> (4 5 6)

;; Lisp-1 (Scheme)
(let ((list '(1 2 3)))
  (list 4 5 6))  ; ERROR - `list` is shadowed
```

### 10.4 Dynamic vs. Lexical Scoping

**Lexical scoping** (modern default):
- **Pros**: Faster (compile-time resolution), safer, enables optimization
- **Cons**: Can't dynamically rebind context variables

**Dynamic scoping** (legacy):
- **Pros**: Convenient for configuration (like `case-fold-search`)
- **Cons**: Slow (runtime lookup), hard to reason about, breaks modularity

Emacs supports both, with special variables marked by `defvar`.

### 10.5 Specpdl vs. C Stack

Emacs uses a separate `specpdl` stack for dynamic bindings instead of the C stack.

**Pros**:
- **Precise unwinding**: Can restore exactly the right bindings
- **GC integration**: Knows what's a Lisp object
- **Introspection**: Backtrace, profiling

**Cons**:
- **Extra indirection**: Slower than native C calls
- **Memory overhead**: Two stacks instead of one

### 10.6 UNEVALLED vs. Macros

For implementing conditionals like `if`, two choices:

**Special form** (`UNEVALLED`):
```c
DEFUN ("if", Fif, Sif, 2, UNEVALLED, 0, ...)
  (Lisp_Object args)
{
  Lisp_Object cond = eval_sub (XCAR (args));
  if (!NILP (cond))
    return eval_sub (Fcar (XCDR (args)));
  return Fprogn (Fcdr (XCDR (args)));
}
```

**Macro**:
```lisp
(defmacro if (cond then &rest else)
  (list 'cond (list cond then) (cons t else)))
```

Special forms are:
- **Faster**: No macro expansion step
- **More flexible**: Can inspect arguments at runtime
- **Built-in only**: Can't be defined in Lisp

Macros are:
- **Extensible**: Users can define their own
- **Composable**: Macros can call other macros
- **Debuggable**: Expansion is visible

Emacs uses special forms for core control flow (`if`, `while`, `catch`) and macros for everything else.

---

## Summary

The Emacs Lisp interpreter is a sophisticated piece of software that balances:

1. **Flexibility**: Three execution models, two scoping modes, extensible via macros
2. **Performance**: Tagged pointers, bytecode compilation, native compilation
3. **Compatibility**: Supports 40+ years of Emacs Lisp code
4. **Debuggability**: Rich introspection, backtraces, profiling

**Core Flow**:
```
Text → Reader → Lisp_Object → eval_sub → {
    Symbol lookup (lexical/dynamic)
    Function call (SUBR/lambda/bytecode/native)
    Special form (UNEVALLED)
    Macro expansion
} → Result
```

**Key Files**:
- `/home/user/emacs/src/lisp.h`: Core data structures
- `/home/user/emacs/src/eval.c`: Evaluation engine (eval_sub, funcall, apply)
- `/home/user/emacs/src/data.c`: Type predicates, primitive operations
- `/home/user/emacs/src/lread.c`: Reader, obarray, symbol interning
- `/home/user/emacs/src/print.c`: Printer
- `/home/user/emacs/src/bytecode.c`: Bytecode interpreter
- `/home/user/emacs/src/comp.c`: Native compiler (libgccjit)

The beauty of this design is that all the complexity is hidden behind simple interfaces:
- Every value is a `Lisp_Object`
- Every function call goes through `funcall`
- Every evaluation goes through `eval`

This uniformity makes the system both powerful and understandable.
