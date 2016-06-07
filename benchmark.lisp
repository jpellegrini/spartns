;; This software is Copyright (c) Jeronimo Pellegrini, 2008.
;; You have the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;(load "spartns.lisp")
;(compile-file "spartns.lisp")

(in-package :spartns)

#+poplog (setf pop11::popminmemlim 70000000)
;	       pop11::popgctrace   1)
#+poplog (pop11::sysgarbage) 

(defun string->symbol (string &rest args)
  (intern (string-upcase (apply #'format `(nil ,string ,@args)))))

(defmacro define-tests (scheme-list)
  (let ((result (loop
		 for scheme in scheme-list collect
		 `(eval-when (:compile-toplevel :load-toplevel :execute)
		   ,(let ((rep-scheme-name (string->symbol "~a" scheme)))
			 `(defspartn ,(string->symbol "3d-~a" scheme)
			   :representation (,rep-scheme-name ,rep-scheme-name ,rep-scheme-name)
			   :non-zero       (100  100 100)
			   :element-type   single-float
			   :sparse-element 0.0
			   :def            NIL
			   :declare        (optimize (speed 3) (safety 0))))

		   (defun  ,(string->symbol "run-~a-get" scheme) ()
		     (declare (optimize (speed 3) (safety 0)))
		     (w/spartns (,(string->symbol "3d-~a" scheme))
				(flet ((xloop (the-array)
					 (let ((value 8.5))
					   (declare (dynamic-extent value))
					   (declare (type single-float value))
					   (dotimes (n 50)
					     (dotimes (i 90)
					       (dotimes (j 90)
						 (dotimes (k 90)
						   (setf value (,(string->symbol "get-3d-~a" scheme) the-array i j k)))))))))
				  
				  (let ((A ( ,(string->symbol "make-3d-~a" scheme) )))
				    (dotimes (i 90)
				      (dotimes (j 90)
					(dotimes (k 90)
					  (,(string->symbol "set-3d-~a" scheme) A i j k -2.0))))
				    (time (funcall #'xloop A))))))

		   
		   #-scl (compile ',(string->symbol "run-~a-get" scheme))

		   
		   (defun ,(string->symbol "run-~a-set" scheme) ()
		     (declare (optimize (speed 3) (safety 0)))
		     (w/spartns (,(string->symbol "3d-~a" scheme))
				(flet ((xloop (the-array)
					 (let ((value 8.5))
					   ;(declare (dynamic-extent value))
					   (declare (type single-float value))
					   (dotimes (n 50)
					     (dotimes (i 90)
					       (dotimes (j 90)
						 (dotimes (k 90)
						   (,(string->symbol "set-3d-~a" scheme) the-array i j k value))))))))
				  (let ((A ( ,(string->symbol "make-3d-~a" scheme) )))
				    (dotimes (i 90)
				      (dotimes (j 90)
					(dotimes (k 90)
					  (,(string->symbol "set-3d-~a" scheme) A i j k -2.0))))
				    (time (funcall #'xloop A))))))
		   
		   #-scl (compile ',(string->symbol "run-~a-set" scheme))
		   
		   
		   (defun  ,(string->symbol "traverse-~a-get" scheme) ()
		     (declare (optimize (speed 3) (safety 0)))
		     (w/spartns (,(string->symbol "3d-~a" scheme))
				(flet ((xloop (the-array)
					 (let ((value (the single-float 8.5)))
					   (declare (dynamic-extent value))
					   (declare (type single-float value))
					   (dotimes (n 50)
					     (,(string->symbol "traverse-3d-~a" scheme) ((i j k) val the-array)
					       (setf value val))))))
				  (let ((A ( ,(string->symbol "make-3d-~a" scheme) )))
				    (dotimes (i 90)
				      (dotimes (j 90)
					(dotimes (k 90)
					  (,(string->symbol "set-3d-~a" scheme) A i j k 2.0))))
				    (time (funcall #'xloop A))))))
		   
		   #-scl (compile ',(string->symbol "traverse-~a-get" scheme))
		   
		   (defun  ,(string->symbol "traverse-~a-set" scheme) ()
		     (declare (optimize (speed 3) (safety 0)))
		     (w/spartns (,(string->symbol "3d-~a" scheme))
				(flet ((xloop (the-array)
					 (let ((value (the single-float 8.5)))
					   (declare (dynamic-extent value))
					   (declare (type single-float value))
					   (dotimes (n 50)
					     (,(string->symbol "traverse-3d-~a" scheme) ((i j k) val the-array)
					       (setf val value))))))
				  (let ((A ( ,(string->symbol "make-3d-~a" scheme) )))
				    (dotimes (i 90)
				      (dotimes (j 90)
					(dotimes (k 90)
					  (,(string->symbol "set-3d-~a" scheme) A i j k 2.0))))
				    (time (funcall #'xloop A))))))
		   
		   #-scl (compile ',(string->symbol "traverse-~a-set" scheme))))))
    `(progn ,@result)))


(defun run-array-set-old ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((A (make-array '(100 100 100) :element-type 'single-float :adjustable nil))
	(value 8.5))
    (declare (dynamic-extent value))
    (time (dotimes (n 50)
	    (dotimes (i 90)
	      (dotimes (j 90)
		(dotimes (k 90)
		  (setf (aref A i j k) value))))))))

(defun run-array-set ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (flet ((xloop ()
	   (let ((A (make-array '(100 100 100) :element-type 'single-float :adjustable nil))
		 (value 8.5))
	     (dotimes (n 50)
	       (dotimes (i 90)
		 (dotimes (j 90)
		   (dotimes (k 90)
		     (setf (aref A i j k) value))))))))
    (time (funcall #'xloop))))

(compile 'run-array-set)

(defun run-array-get ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((A (make-array '(100 100 100) :element-type 'single-float :adjustable nil))
	(value 8.5))
    (time (dotimes (n 50)
	    (dotimes (i 90)
	      (dotimes (j 90)
		(dotimes (k 90)
		  (setf value (aref A i j k)))))))))

(defun run-array-get ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (flet ((xloop ()
	   (let ((A (make-array '(100 100 100) :element-type 'single-float :adjustable nil))
		 (value 8.5))
	     (dotimes (n 50)
	       (dotimes (i 90)
		 (dotimes (j 90)
		   (dotimes (k 90)
		     (setf value (aref A i j k)))))))))
    (time (funcall #'xloop))))


(compile 'run-array-get)

(defmacro run-tests (scheme-list)
  (let ((result 
	 (loop for scheme in scheme-list collect
	       `(progn
		  
		  (format t "~a (SET)~%" ',(string->symbol "RUN-~a-SET" scheme))
		  (,(string->symbol "RUN-~a-SET" scheme))
		  
		  (format t "~a (GET)~%" ',(string->symbol "RUN-~a-GET" scheme))
		  (,(string->symbol "RUN-~a-GET" scheme))
		  
		  (format t "~a (tr-SET)~%" ',(string->symbol "TRAVERSE-~a-SET" scheme))
		  (,(string->symbol "TRAVERSE-~a-SET" scheme))
		  
		  (format t "~a (tr-GET)~%" ',(string->symbol "TRAVERSE-~a-GET" scheme))
		  (,(string->symbol "TRAVERSE-~a-GET" scheme)))))) 
    `(progn
       ,@result
       (format t "Plain array (SET)~%") (run-array-set)
       (format t "Plain array (GET)~%") (run-array-get))))

(define-tests (hash cvector))

(compile 'run-cvector-get)
(compile 'run-cvector-set)
(compile 'traverse-cvector-get)
(compile 'traverse-cvector-set)
(compile 'run-hash-get)
(compile 'run-hash-set)
(compile 'traverse-hash-get)
(compile 'traverse-hash-set)

(run-tests (cvector hash))

