;; This software is Copyright (c) Jeronimo Pellegrini, 2008.
;; You have the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :spartns-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defscheme :name spartns-test::silly
             :make     (make-code)
             :get      (get-code)
             :set      (set-code data index value)
             :delete   (delete-code)
             :traverse (traverse-code)
             :capacity (capacity-code)
             :count    (count-code)))

;; Set to T when debugging:
;(setf lisp-unit::*use-debugger* T)

(defun coerce-list (type numbers)
  (mapcar #'(lambda (x) (coerce x type)) numbers))

(defun first-value (values)
  values)

(defmacro check (test v1 v2)
  `(,(intern (format nil "ASSERT-~A" test)) (first-value ,v1) (first-value ,v2)))

;; new lisp-unit doesn't seem to have this...
;(remove-all-tests)

(define-test defscheme
  (declare (optimize (debug 3) (speed 1)))
  (assert-true (boundp 'spartns::*representation-schemes*))
  (let ((retrieved-scheme (spartns::get-scheme 'spartns-test::silly)))
    (assert-false (null retrieved-scheme))
    (assert-eql   (type-of retrieved-scheme) 'hash-table)
    (multiple-value-bind (cap-code found) (gethash 'spartns::capacity retrieved-scheme)
      (assert-true found)
      (assert-equal '(capacity-code) cap-code))))

(define-test expand-scheme
  (declare (optimize (debug 3) (speed 1)))
  (assert-true (boundp 'spartns::*representation-schemes*))
  (let ((retrieved-scheme (spartns::get-scheme 'spartns-test::silly)))
    (assert-false (null retrieved-scheme))
    (assert-eql   (type-of retrieved-scheme) 'hash-table))
  (let ((expanded (spartns::expand-scheme 'set 'spartns-test::silly '((data  expanded-data)
								      (index expanded-index)
								      (value expanded-value)))))
    (assert-equal expanded '(set-code expanded-data expanded-index expanded-value))))

(defmacro test-scheme (scheme test-capacity)
  ""
  `(progn
     (assert-false (null (spartns::get-scheme ',scheme)))
     (macrolet ((make-map (size)
		  (spartns::expand-scheme 'spartns::make ',scheme `((spartns::size ,size)
								    (spartns::element-type short-float)
								    (spartns::sparse-element 0s0))))
		(get-map (data index)
		  (jp-utils:with-gensyms (block-name)
		    `(block ,block-name
		       ,(spartns::expand-scheme 'spartns::get ',scheme `((spartns::data ,data)
									 (spartns::index ,index)
									 (spartns::test-equal eql)
									 (spartns::element-type short-float)
									 (spartns::sparse-element 0s0)
									 (spartns::block-name ,block-name))))))
		(set-map (data index value)
		  (jp-utils:with-gensyms (block-name)
		    `(block ,block-name
		       ,(spartns::expand-scheme 'spartns::set ',scheme `((spartns::data ,data)
									 (spartns::index ,index)
									 (spartns::value ,value)
									 (spartns::element-type short-float)
									 (spartns::block-name ,block-name))))))
		(delete-map (data index)
		  (jp-utils:with-gensyms (block-name)
		    `(block ,block-name
		       ,(spartns::expand-scheme 'spartns::delete ',scheme `((spartns::data ,data)
									    (spartns::index ,index)
									    (spartns::element-type short-float)
									    (spartns::sparse-element 0s0)
									    (spartns::block-name ,block-name))))))
		(capacity-map (data)
		  (spartns::expand-scheme 'spartns::capacity ',scheme `((spartns::data ,data))))
		(count-map (data)
		  (spartns::expand-scheme 'spartns::count ',scheme `((spartns::data ,data)))))
       (let ((m (make-map 10)))
	 (assert-eql (capacity-map m) ,(if test-capacity 10 most-positive-fixnum))
	 (let ((indices (coerce-list 'fixnum '(5   4    6   3   7   2   8   1   9   0)))
	       (values                       '(5s0 44s0 6s0 3s0 7s0 2s0 8s0 1s0 9s0 4.5s0)))
	   (loop
	      for i in indices
	      for v in values do
		(set-map m i v))
	   (check eql (get-map m 2) 2s0)
	   (loop
	      for i in indices
	      for v in values do
		(check eql (get-map m i) v))
	   (delete-map m 7)
	   (check eql (get-map m 7) 0s0)
	   (check eql (get-map m 0) 4.5s0))))))

(define-test test-schemes
  (test-scheme array T)
  (test-scheme cvector T)
;  (test-scheme avl NIL)
  (test-scheme hash NIL))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defspartn hcd*
      :representation (spartns:hash spartns:cvector)
      :non-zero       (3 4)
      :element-type   long-float
      :sparse-element 0l0
      :def            T)

  (defspartn hhd*
      :representation (spartns:hash spartns:hash)
      :non-zero       (10 10)
      :element-type   long-float
      :sparse-element 0l0
      :def            T)
  
  (defspartn ccd*
      :representation (spartns:cvector spartns:cvector)
      :non-zero       (3 4)
      :element-type   long-float
      :sparse-element 0l0
      :declare        (optimize (debug 3))
      :def            T)

  (defspartn hacd*
      :representation (spartns:hash spartns:array spartns:cvector)
      :non-zero       (3 4 4)
      :element-type   long-float
      :sparse-element -1l0
      :def            T))

(define-test defspartn
  (assert-true (boundp 'spartns::*spartn-schemes*))
  (let ((m (make-hcd*)))
    (set-hcd* m 1 0 2l0)
    (assert-eql (get-hcd* m 1 0) 2l0))
  (let ((n (make-hacd*)))
    (set-hacd* n 1 2 1 3l0)
    (assert-eql (get-hacd* n 1 2 1)  3l0)
    (assert-eql (get-hacd* n 1 1 1) -1l0)))


(define-test spartn-convert
  (assert-true (boundp 'spartns::*spartn-schemes*))
  (let ((m (make-hcd*)))
    (set-hcd* m 1 0 2l0)
    (let ((n (spartn-convert m :from hcd* :to ccd*)))
      (assert-eql (get-ccd* n 1 0) 2l0))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defspartn hhcchcf
      :representation (spartns:hash spartns:hash spartns:cvector spartns:cvector spartns:hash spartns:cvector)
      :non-zero       (4    3    4       3       4    3      )
      :element-type   short-float
      :sparse-element 0s0)
  
  (defspartn cchhchf
      :representation (spartns:cvector spartns:cvector spartns:hash spartns:hash spartns:cvector spartns:hash)
      :non-zero       (4       3       4    3    4       3   )
      :element-type   short-float
      :sparse-element 0s0))

(define-test deep
  (let ((m (make-hhcchcf)))
    (set-hhcchcf m 1 2 1 2 3 1 2.5s0)
    (set-hhcchcf m 1 1 1 2 3 1 1.5s0)
    (set-hhcchcf m 1 2 1 2 2 1 0.5s0)
    (assert-eql (get-hhcchcf m 1 2 1 2 3 1) 2.5s0)
    (assert-eql (get-hhcchcf m 1 1 1 2 3 1) 1.5s0)
    (assert-eql (get-hhcchcf m 1 2 1 2 2 1) 0.5s0)
    (let ((n (make-cchhchf)))
      (set-cchhchf n 1 2 1 2 3 1 -2.5s0)
      (set-cchhchf n 1 1 1 2 3 1 -1.5s0)
      (set-cchhchf n 1 2 1 2 2 1 -0.5s0)
      (assert-eql (get-cchhchf n 1 2 1 2 3 1) -2.5s0)
      (assert-eql (get-cchhchf n 1 1 1 2 3 1) -1.5s0)
      (assert-eql (get-cchhchf n 1 2 1 2 2 1) -0.5s0))
    (let ((n (spartn-convert m :from hhcchcf :to cchhchf)))
      (assert-eql (get-cchhchf n 1 2 1 2 3 1) 2.5s0)
      (assert-eql (get-cchhchf n 1 1 1 2 3 1) 1.5s0)
      (assert-eql (get-cchhchf n 1 2 1 2 2 1) 0.5s0))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defspartn hchf
      :representation (spartns::hash spartns::cvector spartns::hash)
      :non-zero       (3 4 3)
      :element-type   short-float
      :sparse-element 0s0
      :def            T ;nil
      :declare        (optimize (speed 3) (safety 1)))
  
  (defspartn cad
      :representation (spartns::cvector spartns::array)
      :non-zero       (5 5)
      :element-type   double-float
      :sparse-element 0d0
      :def            T
      :declare        (optimize (speed 3) (safety 1)))

  (defspartn cvd
      :representation (spartns:cvector spartns:array) ; was AVL...
      :non-zero       (5 5)
      :element-type   double-float
      :sparse-element 0d0
      :def            T
      :declare        (optimize (speed 3) (safety 1))) )

(define-test w/spartns
  (w/spartns (hchf)
    (let ((s (make-hchf)))
      (set-hchf s 0 0 0 0.5s0)
      (set-hchf s 0 2 0 0.4s0)
      (set-hchf s 1 0 1 0.3s0)
      (assert-eql (get-hchf s 0 0 0) 0.5s0)
      (assert-eql (get-hchf s 0 2 0) 0.4s0)
      (assert-eql (get-hchf s 1 0 1) 0.3s0))))

(define-test spartn-copy-one
  (w/spartns (cad)
    (let ((thing (make-cad)))
      (set-cad thing 2 3 1.2d0)
      (set-cad thing 2 1 3.1d0)
      (set-cad thing 1 2 4.3d0)
      (let ((otherthing (copy-cad thing)))
	(check eql (get-cad otherthing 2 3) 1.2d0)
	(check eql (get-cad otherthing 2 1) 3.1d0)
	(check eql (get-cad otherthing 1 2) 4.3d0)
	(check eql (get-cad otherthing 1 1) 0d0)))))

(define-test spartn-copy
  (w/spartns (cvd)
    (let ((thing (make-cvd)))
      (set-cvd thing 2 3 1.2d0)
      (set-cvd thing 2 1 3.1d0)
      (set-cvd thing 1 2 4.3d0)
      (let ((otherthing (copy-cvd thing)))
	(check eql (get-cvd otherthing 2 3) 1.2d0)
	(check eql (get-cvd otherthing 2 1) 3.1d0)
	(check eql (get-cvd otherthing 1 2) 4.3d0)
	(check eql (get-cvd otherthing 1 1) 0d0)))))

(define-test traversals
  (w/spartns (hchf ccd* cad)
    (let ((X (make-hchf))
	  (list ()))
      (dotimes (i 2)
	(dotimes (j 3)
	  (dotimes (k 4)
	    (set-hchf X i j k (coerce k 'short-float)))))
      (traverse-hchf ((a b c) value X)
		     (push (list a b c value) list))
      (dotimes (i 2)
	(dotimes (j 3)
	  (dotimes (k 4)
	    (assert-true (member (list i j k (coerce k 'short-float)) list :test #'equal)))))
      (let ((counter 100.5s0))
	(declare (type short-float counter))
	(traverse-hchf ((a b c) value X)
		       (setf value counter)
		       (incf counter))
	(setf counter 100.5s0)
	(traverse-hchf ((a b c) value X)
		       (check eql value counter)
		       (incf counter))))
    (let ((Y (make-ccd*))
	  (counter 40L0))
      (dotimes (i 2)
	(dotimes (j 3)
	  (set-ccd* Y i j counter)
	  (incf counter)))
      (setf counter 40L0)
      (traverse-ccd* ((a b) value Y)
	(check eql value counter)
	(incf counter)))
    (let ((Z (make-cad))
	  (counter 40d0))
      (declare (type  double-float counter))
      (dotimes (i 3)
	(dotimes (j 3)
	  (set-cad Z i j counter)
	  (incf counter)))
      (setf counter 40d0)
      (traverse-cad ((a b) value Z :dont-bind ())
	(check eql value counter)
	(incf counter)))))

(define-test multiple-traversal-base
  (assert-equal
   (spartns::with-spartn-traversals-aux '(hhf ccf hcf) '(A B C) '((i j k l) (a b c) (x y z)) '(va vb vc) () () '((pprint b)))
   '(TRAVERSE-HHF ((I J K L) VA A :DONT-BIND NIL)
     (TRAVERSE-CCF ((A B C) VB B :DONT-BIND NIL)
      (TRAVERSE-HCF ((X Y Z) VC C :DONT-BIND NIL)
       (PROGN (PPRINT B))))))
  
  (assert-equal
   (spartns::split-traversal-descriptions '((A data-a i j k l val-a)
					    (B data-b x y z   val-b)
					    (C data-c a b c   val-c)))
   (values '(A B C)
	   '(DATA-A DATA-B DATA-C)
	   '(VAL-A VAL-B VAL-C)
	   '((I J K L) (X Y Z) (A B C))
	   '(NIL NIL NIL)))
  
  (check equal
	 (macroexpand-1 '(w/spartn-traversals ((A data-a i j k l val-a)
						(B data-b a b c   val-b :dont-bind (a))
						(C data-c m n     val-c))
			  (pprint val-a)))
	 '(TRAVERSE-A ((I J K L) VAL-A DATA-A :DONT-BIND NIL)
	   (TRAVERSE-B ((A B C) VAL-B DATA-B :DONT-BIND (A))
	    (TRAVERSE-C ((M N) VAL-C DATA-C :DONT-BIND NIL)
	     (PROGN (PPRINT VAL-A)))))))
  
(define-test with-spartn-traversals
  (w/spartns (hchf ccd* cad)
    (let ((X (make-hchf))
	  (Y (make-ccd*))
	  (Z (make-cad)))
      (dotimes (i 2)
	(dotimes (j 3)
	  (dotimes (k 2)
	    (set-hchf X i j k (coerce (* j k) 'short-float)))))
      (dotimes (i 2)
	(dotimes (j 3)
	  (set-ccd* Y i j (coerce (* i j) 'long-float))))
      (dotimes (i 3)
	(dotimes (j 3)
	  (set-cad Z i j (coerce i 'double-float))))
      (w/spartn-traversals ((hchf X a b c val-x)
			    (cad  Z   m a val-z)
			    (ccd* Y   b c val-y))
	(check eql val-x (coerce (* b c) 'short-float))
	(check eql val-y (coerce (* b c) 'long-float))
	(check eql val-z (coerce m 'double-float))))))


(define-test with-spartn-traversals-setfable
  (w/spartns (hchf ccd* cad)
    (let ((X (make-hchf))
	  (Y (make-cad))
	  (Z (make-ccd*)))
      (dotimes (i 2)
	(dotimes (j 3)
	  (dotimes (k 2)
	    (set-hchf X i j k (coerce (* j k) 'short-float)))))
      (dotimes (i 2)
	(dotimes (j 3)
	  (set-cad Y i j (coerce (* i j) 'double-float))))
      (dotimes (i 3)
	(dotimes (j 3)
	  (set-ccd* Z i j (coerce i 'long-float))))
      
      (w/spartn-traversals ((hchf X a b c val-x)
			    (ccd* Z   m a val-z)
			    (cad  Y   b c val-y))
	
	(when (eql 0 (mod b 2))
	  (setf val-y -20d0))

	(when (eql 0 (mod m 3))
	  (setf val-z -30l0))
	
	(when (eql 0 (mod c 5))
	  (setf val-x -50d0)))

     
      
      (w/spartn-traversals ((hchf X a b c val-x)
			    (ccd* Z   m a val-z)
			    (cad  Y   b c val-y))
	(if (eql 0 (mod c 5))
	    (check eql val-x -50d0)
	    (check eql val-x (coerce (* b c) 'short-float)))
	(if (eql 0 (mod b 2))
	    (check eql val-y -20d0)
	    (check eql val-y (coerce (* b c) 'double-float)))
	(if (eql 0 (mod m 3))
	    (check eql val-z -30l0)
	    (check eql val-z (coerce m 'long-float)))))))

(define-test with-spartn-traversals-setfable-2
  (w/spartns (hchf ccd* cad)
    (let ((X (make-hchf))
	  (Y (make-cad))
	  (Z (make-ccd*)))
      (dotimes (i 2)
	(dotimes (j 3)
	  (dotimes (k 2)
	    (set-hchf X i j k (coerce (* j k) 'short-float)))))
      (dotimes (i 2)
	(dotimes (j 3)
	  (set-cad Y i j (coerce (* i j) 'double-float))))
      (dotimes (i 3)
	(dotimes (j 3)
	  (set-ccd* Z i j (coerce i 'long-float))))
      
      (traverse-hchf ((q w e) vv x)
	(check eql vv (get-hchf x q w e)))

      (w/spartn-traversals ((cad  Y   b c val-y)
			    (hchf X a b c val-x)
			    (ccd* Z   m a val-z))
	
	(when (eql 0 (mod b 2))
	  (setf val-y -20d0))
	
	(when (eql 0 (mod m 3))
	  (setf val-z -30l0))

	(when (eql 0 (mod c 5))
	  (setf val-x -50.0)))
     
      (w/spartn-traversals ((cad  Y   b c val-y)
			    (hchf X a b c val-x)
			    (ccd* Z   m a val-z))
	(if (eql 0 (mod c 5))
	    (check eql val-x  -50.0)
	    (check eql val-x (coerce (* b c) 'short-float)))
	
	(if (eql 0 (mod b 2))
	    (check eql val-y -20d0)
	    (check eql val-y (coerce (* b c) 'double-float)))
	(if (eql 0 (mod m 3))
	    (check eql val-z -30l0)
	    (check eql val-z (coerce m 'long-float)))))))
 
(define-test matrix-mult
  ;; ( 0 4 2 )   (  7 1 0 )
  ;; ( 1 8 3 ) x ( -4 0 0 )
  ;; ( 9 0 0 )   ( -2 8 2 )
  ;;
  ;;   ( -20 16 4 )
  ;; = ( -31 25 6 )
  ;;   (  63  9 0 )
  ;;
  (let ((X (make-ccd*))
	(Y (make-ccd*))
	(Z (make-ccd*)))
    (set-ccd* X 0 1 4l0)
    (set-ccd* X 0 2 2l0)
    (set-ccd* X 1 0 1l0)
    (set-ccd* X 1 1 8l0)
    (set-ccd* X 1 2 3l0)
    (set-ccd* X 2 0 9l0)
    
    (set-ccd* Y 0 0  7l0)
    (set-ccd* Y 0 1  1l0)
    (set-ccd* Y 1 0 -4l0)
    (set-ccd* Y 2 0 -2l0)
    (set-ccd* Y 2 1  8l0)
    (set-ccd* Y 2 2  2l0)

    (w/spartn-traversals ((ccd* X i j val-x)
		          (ccd* Y j k val-y))
      
      (set-ccd* Z i k
		(+ (get-ccd* Z i k) (* val-x val-y))))

    (check eql (get-ccd* Z 0 0) -20l0)
    (check eql (get-ccd* Z 0 1)  16l0)
    (check eql (get-ccd* Z 0 2)   4l0)
    (check eql (get-ccd* Z 1 0) -31l0)
    (check eql (get-ccd* Z 1 1)  25l0)
    (check eql (get-ccd* Z 1 2)   6l0)
    (check eql (get-ccd* Z 2 0)  63l0)
    (check eql (get-ccd* Z 2 1)   9l0)
    (check eql (get-ccd* Z 2 2)   0l0)))

(define-test flatten
    (check equal (spartns::flatten '(a b (c (d e e) f) (((g) h) i (i) k)))
		  '(a b c d e e f g h i i k)))

(define-test make-types-list
    (check equal (spartns::make-types-list '(ccd* hchf) '(i j k))
	   '(long-float short-float fixnum fixnum fixnum)))

(define-test do-fast-traversal
    ;; Just check for execution errors:
    (spartns::do-fast-traversal '(i j k) '(v1 v2 v3) '(fixnum double-float double-float)
		       '(format t "i=~a  j=~a  k=~a~%" i j k)))

(define-test w/fast-spart-traversals
  (w/spartns (hchf ccd* cad)
    (let ((X (make-hchf))
	  (Y (make-ccd*))
	  (Z (make-cad)))
      (dotimes (i 2)
	(dotimes (j 3)
	  (dotimes (k 2)
	    (set-hchf X i j k (coerce (* j k) 'short-float)))))
      (dotimes (i 2)
	(dotimes (j 3)
	  (set-ccd* Y i j (coerce (* i j) 'long-float))))
      (dotimes (i 3)
	(dotimes (j 3)
	  (set-cad Z i j (coerce i 'double-float))))
      
      (spartns::w/fast-traversals ((hchf X a b c val-x)
				   (cad  Z   m a val-z)
				   (ccd* Y   b c val-y))
	(check eql val-x (coerce (* b c) 'short-float))
	(check eql val-y (coerce (* b c) 'long-float))
	(check eql val-z (coerce m       'double-float))))))

(define-test fast-matrix-mult
  ;; ( 0 4 2 )   (  7 1 0 )
  ;; ( 1 8 3 ) x ( -4 0 0 )
  ;; ( 9 0 0 )   ( -2 8 2 )
  ;;
  ;;   ( -20 16 4 )
  ;; = ( -31 25 6 )
  ;;   (  63  9 0 )
  ;;
  (let ((X (make-ccd*))
	(Y (make-ccd*))
	(Z (make-ccd*)))
    (set-ccd* X 0 1 4l0)
    (set-ccd* X 0 2 2l0)
    (set-ccd* X 1 0 1l0)
    (set-ccd* X 1 1 8l0)
    (set-ccd* X 1 2 3l0)
    (set-ccd* X 2 0 9l0)
    
    (set-ccd* Y 0 0  7l0)
    (set-ccd* Y 0 1  1l0)
    (set-ccd* Y 1 0 -4l0)
    (set-ccd* Y 2 0 -2l0)
    (set-ccd* Y 2 1  8l0)
    (set-ccd* Y 2 2  2l0)

    (w/fast-traversals ((ccd* X i j val-x)
			(ccd* Y j k val-y))
      (set-ccd* Z i k
		(+ (get-ccd* Z i k) (* val-x val-y))))
    
    (check eql (get-ccd* Z 0 0) -20l0)
    (check eql (get-ccd* Z 0 1)  16l0)
    (check eql (get-ccd* Z 0 2)   4l0)
    (check eql (get-ccd* Z 1 0) -31l0)
    (check eql (get-ccd* Z 1 1)  25l0)
    (check eql (get-ccd* Z 1 2)   6l0)
    (check eql (get-ccd* Z 2 0)  63l0)
    (check eql (get-ccd* Z 2 1)   9l0)
    (check eql (get-ccd* Z 2 2)   0l0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defspartn ci
      :representation (spartns::cvector)
      :non-zero       (10)
      :element-type   fixnum
      :sparse-element 0
      :resize-amount  50
      :def            NIL))

(define-test resize-amount-spartn
  (w/spartns (ci)
    (let ((V (make-ci)))
      (assert-eql (spartns::cvector-count V) 0)
      (assert-eql (spartns::cvector-max-capacity V) 10)
      (dotimes (i 10)
	(set-ci V i (the fixnum (* 2 i))))
      (assert-eql (spartns::cvector-count V) 10)
      (assert-eql (spartns::cvector-max-capacity V) 10)
      (dotimes (i 10)
	(set-ci V (+ i 10) (the fixnum (* 2 (+ i 10)))))
      (assert-eql (spartns::cvector-count V) 20)
      (assert-eql (spartns::cvector-max-capacity V) 60)
      (traverse-ci ((index) val V)
	(check eql val (* 2 index)))
      (pack-ci V)
      (assert-eql (spartns::cvector-max-capacity V) 20)
      (assert-eql (spartns::cvector-count V) 20))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defspartn ci2
      :representation (spartns::cvector)
      :non-zero       (10)
      :element-type   fixnum
      :sparse-element 0
      :resize-function (lambda (x) (* 2 x)) ; double size
      :def            NIL))

(define-test resize-spartn
  (w/spartns (ci2)
    (let ((V (make-ci2)))
      (assert-eql (spartns::cvector-count V) 0)
      (assert-eql (spartns::cvector-max-capacity V) 10)
      (dotimes (i 10)
	(set-ci2 V i (the fixnum (* 2 i))))
      (assert-eql (spartns::cvector-count V) 10)
      (assert-eql (spartns::cvector-max-capacity V) 10)
      (dotimes (i 5)
	(set-ci2 V (+ i 10) (the fixnum (* 2 (+ i 10)))))
      (assert-eql (spartns::cvector-count V) 15)
      (assert-eql (spartns::cvector-max-capacity V) 20) ; (* 2 10)
      (traverse-ci2 ((index) val V)
	(check eql val (* 2 index)))
      (pack-ci2 V)
      (assert-eql (spartns::cvector-max-capacity V) 15)
      (assert-eql (spartns::cvector-count V) 15))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defspartn ccd-tiny
      :representation (spartns:cvector spartns:cvector)
      :non-zero       (1 1)
      :element-type   long-float
      :sparse-element 0l0
      :resize-amount  10
      :declare        (optimize (debug 3))
      :def            T))


(define-test fast-matrix-mult-with-packing
  ;; ( 0 4 2 )   (  7 1 0 )
  ;; ( 1 8 3 ) x ( -4 0 0 )
  ;; ( 9 0 0 )   ( -2 8 2 )
  ;;
  ;;   ( -20 16 4 )
  ;; = ( -31 25 6 )
  ;;   (  63  9 0 )
  ;;
  (let ((X (make-ccd-tiny))
	(Y (make-ccd-tiny))
	(Z (make-ccd-tiny)))
    (set-ccd-tiny X 0 1 4l0)
    (set-ccd-tiny X 0 2 2l0)
    (set-ccd-tiny X 1 0 1l0)
    (set-ccd-tiny X 1 1 8l0)
    (set-ccd-tiny X 1 2 3l0)
    (set-ccd-tiny X 2 0 9l0)
    
    (set-ccd-tiny Y 0 0  7l0)
    (set-ccd-tiny Y 0 1  1l0)
    (set-ccd-tiny Y 1 0 -4l0)
    (set-ccd-tiny Y 2 0 -2l0)
    (set-ccd-tiny Y 2 1  8l0)
    (set-ccd-tiny Y 2 2  2l0)

    (pack-ccd-tiny X)
    (pack-ccd-tiny Y)
    
    (w/fast-traversals ((ccd-tiny X i j val-x)
			(ccd-tiny Y j k val-y))
      (set-ccd-tiny Z i k
		    (+ (get-ccd-tiny Z i k) (* val-x val-y))))
    (pack-ccd-tiny Z)
    
    (check eql (get-ccd-tiny Z 0 0) -20l0)
    (check eql (get-ccd-tiny Z 0 1)  16l0)
    (check eql (get-ccd-tiny Z 0 2)  4l0)
    (check eql (get-ccd-tiny Z 1 0) -31l0)
    (check eql (get-ccd-tiny Z 1 1)  25l0)
    (check eql (get-ccd-tiny Z 1 2)  6l0)
    (check eql (get-ccd-tiny Z 2 0)  63l0)
    (check eql (get-ccd-tiny Z 2 1)  9l0)
    (check eql (get-ccd-tiny Z 2 2)  0l0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (spartns:defspartn 2d-matrix
     :representation (spartns::cvector spartns::cvector)
     :element-type double-float
     :sparse-element 0d0
     :test-equal eql
     :resize-amount  10
     :def t))

(define-test no-nonzero-list
    ;This will test an empty nonzero list. Reported by Yves Vandriessche
    (let ((s (make-2d-matrix :non-zeros '(5 6))))
      (set-2d-matrix s 1 1 42d0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defspartn hacha
      :representation (spartns:hash spartns:array spartns:cvector spartns:hash spartns:array)
      :non-zero       (5 5 5 5 5)
      :element-type   list  ;long-float
      :sparse-element '() ;0l0
      :resize-amount  10
      :def            T))

(define-test partial-traversals
  (let ((accumulator ())
	(data (make-hacha)))
    (dotimes (i 3)
      (dotimes (j 3)
	(dotimes (k 3)
	  (dotimes (l 3)
	    (dotimes (m 3)
	      (set-hacha data i j k l m (mapcar #'(lambda (x) (* 10 x))
						(list i j k l m))))))))
    (let ((b 2)
	  (d 1))
      (traverse-hacha ((a b c d e) val data :dont-bind (b d))
	(push val accumulator)))
    (let ((final (reverse accumulator)))
      ;; Clisp does NOT traverse hashtables in order, so we can't test
      ;; order...
      (assert-eq ()
		 (set-difference '((0 20 0 10 0)
				   (0 20 0 10 10)
				   (0 20 0 10 20)
				   (0 20 10 10 0)
				   (0 20 10 10 10)
				   (0 20 10 10 20)
				   (0 20 20 10 0)
				   (0 20 20 10 10)
				   (0 20 20 10 20)
				   (10 20 0 10 0)
				   (10 20 0 10 10)
				   (10 20 0 10 20)
				   (10 20 10 10 0)
				   (10 20 10 10 10)
				   (10 20 10 10 20)
				   (10 20 20 10 0)
				   (10 20 20 10 10)
				   (10 20 20 10 20)
				   (20 20 0 10 0)
				   (20 20 0 10 10)
				   (20 20 0 10 20)
				   (20 20 10 10 0)
				   (20 20 10 10 10)
				   (20 20 10 10 20)
				   (20 20 20 10 0)
				   (20 20 20 10 10)
				   (20 20 20 10 20)) final :test #'equal)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defspartn 2dmat
      :representation (cvector array)
      :element-type   symbol
      :sparse-element 'NOTHING
      :def            T))

(define-test symbol
  (let ((mat (make-2dmat))
	(list ()))
    (set-2dmat mat 0 3 'X)
    (set-2dmat mat 1 2 'Y)
    (traverse-2dmat ((a b) val mat)
      (push (list a b val) list))
    (assert-eq () (set-difference '((0 3 x) (1 2 y)) list :test #'equal))))

  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defspartn sym-mat
      :representation (hash cvector array)
      :non-zero       (5 5 5)
      :element-type   symbol
      :sparse-element 'sparse
      :declare        (optimize (debug 3))
      :def t))

(define-test partial-traversals-2
  (let ((X (make-sym-mat)))
    (set-sym-mat x  0 1 1 'A)
    (set-sym-mat x  0 2 3 'B)
    (set-sym-mat x  0 3 2 'C)
    
    (set-sym-mat x  1 1 3 'D)
    (set-sym-mat x  1 2 3 'E)

    (traverse-sym-mat ((i j k) v x)
      (assert-eql v (get-sym-mat x i j k)))
    
    (let ((final ())
	  (i 1)
	  (j 2)
	  (k 3))
      
      (traverse-sym-mat ((i j k) v x :dont-bind (k))
	(assert-eql k 3)
	(push v final))
      (assert-eq () (set-difference '(b d e) final :test #'equal))

      (setf final () i 1)
      (traverse-sym-mat ((i j k) val x :dont-bind (i))
	(assert-eql i 1)
	(push val final))
      (assert-eq () (set-difference '(e d) final :test #'equal))

      (setf final () i 1 k 3)
      (traverse-sym-mat ((i j k) v x :dont-bind (i k))
	(push v final))
      (assert-eq () (set-difference '(d e) final :test #'equal))


      
      (setf final () i 1 j 2)
      (traverse-sym-mat ((i j k) v x :dont-bind (i j))
	(push v final))
      (assert-eq () (set-difference '(e) final :test #'equal))

      (setf final () j 2 k 3)
      (traverse-sym-mat ((i j k) v x :dont-bind (j k))
	(push v final))
      (assert-eq () (set-difference '(b e) final :test #'equal))

      (setf final () i 1 j 2 k 3)
      (traverse-sym-mat ((i j k) v x :dont-bind (i j k))
	(push v final))
      (assert-eq () (set-difference '(e) final :test #'equal))
      
      (setf final () j 2)
      (traverse-sym-mat ((i j k) v x :dont-bind (j))
	(assert-eql j 2)
	(push v final))
      (assert-eq () (set-difference '(e b) final :test #'equal)))))

(define-test split-indices-values-dont-bind
  (multiple-value-bind (ind val dont-bind)
      (spartns::split-indices-values-dont-bind '(A B i j k l V))
    (assert-equal ind '(i j k l))
    (assert-eql   val 'v)
    (assert-eql   dont-bind nil))
  
  (multiple-value-bind (ind val dont-bind)
      (spartns::split-indices-values-dont-bind '(A B i j k V :dont-bind (i k)))
    (assert-equal ind '(i j k))
    (assert-eql   val 'v)
    (assert-equal dont-bind '(i k))))

(define-test with-spartns-traversals-aux
  ;; Silly test, should be much better
  (assert-equal (spartns::with-spartn-traversals-aux
		    '(number-2d fixnum-2d)
		  '(A B)
		  '((I J) (x y))
		  '(va vb)
		  '((j) NIL)
		  ()
		  '((pprint x)
		    (pprint y)
		    (pprint va)))
		'(TRAVERSE-NUMBER-2D ((I J) VA A :DONT-BIND (J))
		  (TRAVERSE-FIXNUM-2D ((X Y) VB B :DONT-BIND NIL)
		   (PROGN (PPRINT X) (PPRINT Y) (PPRINT VA))))))
		  
(in-package :jp-utils-test)
(jp-utils-test::run-tests)
(terpri)
(format t "--------------------------------------------------")
(in-package :spartns-test)
(run-tests)

;; new lisp-unit doesn't seem to have this...
;(remove-all-tests)
