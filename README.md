# Spartns

Spartns is a SPARse TeNSor representation library (if you don't know what a tensor is, think of it as a matrix with any number of dimensions, not just two). Spartns is distributed under the LLGPL license.

[See more information on Spartns here](http://aleph0.info/spartns/)

Features:

*    No external dependencies (no BLAS or any other C/Fortran library needed). Just plain Common Lisp;
*    Represents mappings from one dimension onto another using any scheme you want (there are three built-in schemes: array, hash and compressed-vector, but you can roll your own and plug it);
*    Flexible: works with any data type;
*    Heavily optimized: traversing the tensor can be extremely fast (in one specific situation -- traversing the tensor -- it was 10 times faster than a naive implementation in C++);
*    Fairly portable: works with SBCL, ABCL, Clisp, ECL, GCL, and XCL.
*    Spartns is never released without going through regression tests (if a platform breaks and can't be supported, it will be clear in the release announcement);
*    ASDF installable (thanks Slobodan Blazeski!);
*    Easy to use, with introductory documentation (not only on-line);
*    Comes with description of the internals of the library.

One simple example: two dimensional matrix multiplication. First, define a type for 2-dimensional matrices:

```
(defspartn 2dmatrix
   :representation (spartns:hash spartns:cvector)
   :non-zero       (3 4)
   :element-type   long-float
   :sparse-element 0L0)
```

The Spartn type "2dmatrix" is then defined as the type for sparse tensors that map indices onto long-floats using a hashtable of compressed vectors. When they are created, the hashtables start with :size 3, and the compressed vectors with :size 4. Now, create three matrices, X Y and Z, and multiply them:

```
(let ((X (make-2dmatrix))
      (Y (make-2dmatrix))
      (Z (make-2dmatrix)))
  (set-2dmatrix X 0 0 5L0)
  (set-2dmatrix Y 0 1 6L4)
  ;; set non-zeros in the rest of the matrices X and Y

  ;; and now multiply them:
  (w/fast-traversals ((2dmatrix X i j val-x)
                      (2dmatrix Y j k val-y))
    (set-2dmatrix Z i k
      (+ (get-2dmatrix Z i k) (* val-x val-y)))))
```



