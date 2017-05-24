The script test-across-lisps.pl runs tests across implementations.
You can add more implementations there if you want.

Currently recommended:
----------------------

The fastest platform for Spartns seems to be SBCL, although
all others seem to run fine (but do run the "do-benchmarks.lisp"
file on your implementation before deciding to use it -- some
implementations are very slow for some representation schemes).


Current state:
--------------

```
ABCL               works
CLISP              works
Clozure            works
CMUCL              works (tested the 32bit binary on a 64bit platform; passes all tests)
ECL                works
GCL                works
LispWorks          ?
Mankai             fails 18 Spartns tests (but no spartns-utils tests)
Poplog             ?
SBCL               works
XCL                probably works (but I can't currently compile XCL)
```

==> Scieneer passes tests, but something is broken in benchmarks


Workarounds:
------------

* For some reason doing `(aref M 2)` (or any other number) in
  Clozure CL doesn't seem to work, so we have to do `(coerce 2 'fixnum)`.
  That probably has an impact on performance.
  This only afects the `ARRAY` scheme on Cozure, and only the `GET`
  function (`SET` and traversals are not affected)
* Poplog does not accept `(make-hash-table :size 0)`, so Spartns
  uses `#+poplog (max 1 size) #-poplog size`
* In Allegro Common Lisp, if A is a `SIMPLE-ARRAY`, the result of
  `(adjust-array A new-size)` is not.
  Spartns now uses a function called `ROBUST-ADJUST-ARRAY` for this.
  ACL is treated as a special case.
* As a consequence of adjusting non-adjustable arrays (as mentioned
  in the previous points), growing a `CVECTOR` beyond its capacity
  can be fast or expensive, depending on the Common Lisp implementation
  (if your Lisp makes a new array and copies everything over, it will
  be O(n); if it is smart enough to optimize this then it can be done in
  O(1))
* GCL doesn't seem to accept a `DECLARE` statement inside a `SYMBOL-MACROLET`
  which, in turn, is isnide a `LOOP` inside a `WHILE`. Type declarations inside `do-traverse-cvector` for GCL 
  is disabled for now.
