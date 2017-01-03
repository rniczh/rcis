Rcis
====
rcis is  the scheme to x64 compiler, it based on Indiana university's compiler courses, using nanopass compiler framework and chezscheme

Syntax
=====

```scheme
;;  Program → Exp
;;
;; Exp     → var
;;         | constant
;;         | 'Datum
;;         | (quote Datum)
;;         | (define var Exp)
;;         | (define (var var*) Exp)
;;         | (if Exp Exp)
;;         | (if Exp Exp Exp)
;;         | (begin Exp* Exp)
;;         | (let ([uvar Exp]*) Exp)
;;         | (letrec ([uvar Exp]*) Exp)
;;         | (letrec* ([uvar Exp]*) Exp)
;;         | (lambda (uvar*) Exp)
;;         | (ccall string Exp*)
;;         | (prim Exp*)
;;         | (Exp Exp* )
;;
;; Datum   → immediate
;;         | (Datum . Datum)
;;         | #(Datum*)
;;         | #&Datum
;;
;; prim    → + - * / mod div logand logor sra ash
;;         | <= < = > >=
;;         | boolean? eq? fixnum? procedure? box? pair? null? vector? string?
;;         | cons car cdr set-car! set-cdr!
;;         | box unbox set-box!
;;         | make-vector vector-length vector-ref vector-set!
;;         | void display newline read

```

Install
========

Dependencies:

`Linux`, `GNU Make`, `GCC(version>=5)`, `ChezScheme`

```shell
$ make
```
if failed on using the protable ChezScheme,
you need to install the [ChezScheme](https://github.com/cisco/ChezScheme) and type
```shell
$ make install
```
If success it will display:
```shell
* configure options.ss...ok
* booting file...ok
* produce executable...ok
Executable at boot/rcis
```
and rcis will be compiled into boot directory.

Usage
========

alias the rcis in your .bashrc or .zshrc ... etc

- Compile with C: (in example directory)
rule: `-o <output> <scheme-file> <c-files>*`
```shell
$ rcis -o prog change.ss change.c
$ ./prog
```

- Get more information
```shell
$ rcis --help
```

Test
========

- Check file can be compiled
```
$ make test-all
```

- Compare the rcis result with chezscheme result
```
$ make test-driver
```

Acknowledgements
========

The main reference comes from [yscheme](https://github.com/yinwang0/yscheme)
