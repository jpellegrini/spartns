;; This software is Copyright (c) Jeronimo Pellegrini, 2008.
;; You have the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.


(in-package :spartns)

;;;--- SPECIAL VARIABLES ---;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *representation-schemes* (make-hash-table :size 20 :test 'eq)
    "This variable holds a hashtable that maps names (symbols)
to representation schemes for sparse vectors. These are structures
that map fixnums onto data of any type.")
  (defvar *spartn-schemes* (make-hash-table)
    "This variable holds a hashtable which maps names onto spartn schemes. "))


;;;--- DEFSCHEME ---;;;

(defun get-scheme (name)
  "Retrieves a representation scheme given its name, which should be a symbol. If the scheme
is not registered, this function returns NIL.

 (spartns::get-scheme 'spartns:cvector)
 ==>
  #<HASH-TABLE :TEST EQ :COUNT 10 {10036CF001}>
  T

The hashtable returned is the representation scheme (see the function DEFSCHEME)."
  (check-type name symbol)
  (gethash name *representation-schemes* nil))

(defun remove-scheme (name)
  "Removes a representation scheme from the database."
  (check-type name symbol)
  (remhash name *representation-schemes*))

(defun expand-scheme (function scheme subst-list)
  "Expands the code from a representation scheme, substituting parameters.
For example, if a scheme with the name 'cvector has this function registered:
'spartns::get --> '(get-cvec data index sparse-element)
Then expand-scheme will work like this:
 (expand-scheme 'spartns::get 'cvector '((data d) (index i) (sparse-element sp)))
  => '(get-cvec d i sp)"
   (let ((scheme-hash (gethash scheme *representation-schemes*)))
     (when (null scheme-hash)
       (cerror  "Ignore this error" (format nil "Can't expand scheme ~a (it's not in the schemes database)." scheme)))
     (subst-all subst-list (gethash function scheme-hash))))


(defmacro defscheme (&key
		     name
		     type
		     make
		     get
		     set
		     delete
		     traverse
		     pack
		     capacity
		     count)
  "Adds a new representation scheme.
- name is the name of the scheme (a symbol)
- type is the Common Lisp type of an instance of this scheme. For example, if your scheme
  represents the mapping as a hashtable, then type is HASH-TABLE. If it is represented as a
  Lisp list, then it is LIST

All other parameters (make, get, set, delete, traverse, capacity, count) should contain the
Lisp code necessary to perform these actions:

- make: the code necessary to create an instance of this scheme. If your code contains the
  symbols SIZE and ELEMENT-TYPE, they will be substituted for the actual values;
- get: code necessary to access one element. Symbols that are substituted: DATA, INDEX,
  SPARSE-ELEMENT. It should return TWO VALUES:
  + the retrieved element (the sparse element, if the index was not found);
  + either T or NIL, depending on wether the element was found or not.
  For example, (IF found (VALUES element T)
                         (VALUES sparse NIL));
- set: code to set one position. Substituted symbols: DATA, INDEX, VALUE, ELEMENT-TYPE
- delete: code to remove one element. Substituted symbols: DATA, INDEX, SPARSE-ELEMENT,
  ELEMENT-TYPE;
- traverse: code to traverse the structure, binding variables to the index and value while
  executing a body of code. Substituted symbols: INDEX, VALUE, DATA, ELEMENT-TYPE, BODY;
- pack: code that packs the container, releasing non-used memory;
- capacity: code that returns the maximum number of elements that the instance can handle.
  Substituted symbols: DATA;
- count: code to return the actual number of non-zero elements. Substituted symbols: DATA.

The macro will return the name of the newly created scheme.

For example:
 (defscheme :name     hash
            :type     HASH-TABLE
            :make     (let () (declare (ignore element-type)) (make-hash-table :size size))
            :get      (gethash index data sparse-element)
            :set      (setf (gethash index data) value)
            :delete   (remhash index data)
            :traverse (do-traverse-hash (index value data) body)
            :pack     (values) ; we don't write code for shrinking a hashtable!
            :capacity (let () (declare (ignore data)) most-positive-fixnum)
            :count    (hash-table-count data))

In the example, the make function ignores ELEMENT-TYPE (because hashtables ignore them).

However, you should NOT try to make the code ignore symbols by using, for example,
 (let () (declare (ignore element-type)) (make-hash-table :size size))
because the element-type inside DECLARE will be substituted.

Since the make function returns an object of type HASH-TABLE, then this is the value passed to
the type parameter.

The capacity code always returns most-positive-fixnum, since hashtables can grow."
    (check-type name symbol)
    (dolist (parameter (list spartns::make
			     spartns::get
			     spartns::set
			     spartns::delete
			     spartns::traverse
			     spartns::pack
			     spartns::capacity
			     spartns::count))
      (check-type parameter list))
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (let ((new-scheme (gensym)))
	`(progn
	   (let ((,new-scheme (make-hash-table :size 7 :test 'eq)))
	     (setf (gethash 'spartns::name     ,new-scheme)      ',name
		   (gethash 'spartns::type     ,new-scheme)      ',type
		   (gethash 'spartns::make     ,new-scheme)      ',make
		   (gethash 'spartns::get      ,new-scheme)       ',get
		   (gethash 'spartns::set      ,new-scheme)       ',set
		   (gethash 'spartns::delete   ,new-scheme)    ',delete
		   (gethash 'spartns::traverse ,new-scheme)  ',traverse
		   (gethash 'spartns::pack     ,new-scheme)      ',pack
		   (gethash 'spartns::capacity ,new-scheme)  ',capacity
		   (gethash 'spartns::count    ,new-scheme)     ',count)
	     (setf (gethash ',name *representation-schemes*) ,new-scheme))))))


;;;--- SCHEME: PATRICIA ---;;;

;; Currently being developed

#+ nil (defscheme :name patricia
           :type ???
	   :make     ()
	   :get      (patricia::find data index)
	   :set      ()
	   :delete   (patricia::delete data index)
	   :traverse (patricia::traverse (index value data) body)
	   :pack     (values)
           :capacity (values most-positive-fixnum) ; no limit
	   :count    (patricia::patricia-tree-count data))

;;;--- SCHEME: AVL ---;;;

;; Currently being developed

#+ nil (defscheme :name     avl
           :type     avl::avl-tree
	   :make     (avl::make-avl-tree)
	   :get      (multiple-value-bind (v f) (avl::avl-find data index sparse-element)
		       (values (the element-type v)
			       (the boolean f)))
	   :set      (avl::avl-insert data index value)
	   :delete   (avl::avl-insert data index sparse-element) ; cheating...
	   :traverse (avl::avl-in-order (index value data) body)
	   :pack     (values) ; can't pack an AVL tree
	   :capacity (values most-positive-fixnum) ; no limit
	   :count    (avl::avl-size data))

;;;--- SCHEME: HASH ---;;;

;; We will add a representation scheme based on hashtables.
;; Poplog does not allow zero as :size to MAKE-HASH-TABLE,
;; so we use (max 1 size) for it. However, I'd like it to be
;; zero for all others (it saves memory -- for example, Clisp
;; seems to allocate a completely empty hashtable is this
;; case). So that's why we have implementation specific code
;; there.
(defscheme  :name     hash
  :type     hash-table
  :make     (make-hash-table :size #+poplog (max 1 size)
				   #-poplog
				   #+gcl (max 1 size)
				   #-gcl
				   size
			     :test #'eq)
  :get      #+xcl (gethash index data sparse-element)
  	    #-xcl (multiple-value-bind (v f) (gethash index data)
	      (values (the element-type (if f v sparse-element))
		      (the boolean f)))
  :set      (setf (gethash (the fixnum index) data) (the element-type value))
  :delete   (remhash index data)
  :traverse #|loop for index fixnum being the hash-keys of data do
		  (when (and (>= index start)
			     (or (< end 0) 
				 (<= index end)))
		    (symbol-macrolet ((value (gethash index data)))
				     body)))|#
  (maphash #'(lambda (index value) ; value will be overriden
	       (declare (type fixnum index))
	       (when (and (>= index start)
			  (or (< end 0)
			      (<= index end)))
		 (symbol-macrolet ((value (gethash index data)))
		   body)))
	   data) 
  :pack     (values) ; can't pack a hashtable -- yet!
  :capacity (values most-positive-fixnum)
  :count    (hash-table-count data))

;;;--- SCHEME: ARRAY ---;;;

;; An array-structure has an array of ELEMENT-TYPE for the data and a bit vector
;; for representing sparsity: when the bit is 0, the element is not present; when it
;; is one, it is present
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (array-structure (:type vector))
    (values (make-array 0)                         :type simple-array)
    (active (make-array 0  :element-type '(mod 2)) :type (simple-array (mod 2) (*)))
    (max-capacity   0      :type fixnum)
    (count          0      :type fixnum)))



;; We will add a representation scheme based on arrays:
(defscheme      :name     array
  :type     (simple-vector 4)
  :make     (make-array-structure
	     :values (make-array size
				 :element-type (quote element-type)
				 :initial-element (the element-type sparse-element))
	     :active (make-array size
				 :element-type (quote (mod 2))
				 :initial-element 0)
	     :max-capacity size)
  :get      (values
	     (aref (the (simple-array element-type (*)) (array-structure-values data))
		   #-openmcl index
		   #+openmcl (coerce index 'fixnum))
	     (test-equal (aref (the (simple-array (mod 2) (*)) (array-structure-active data)) index)
			 1))
  :set      (progn (setf (aref (the (simple-array element-type (*)) (array-structure-values data)) index)
			 (the element-type value)
			 (aref (the (simple-array (mod 2)      (*)) (array-structure-active data)) index)
			 1)
		   (incf (array-structure-count data))
		   (values))
  :delete   (progn
	      (setf (aref
		     (the (simple-array element-type (*)) (array-structure-values data)) index)
		    (the element-type sparse-element))
	      (setf (aref
		     (the (simple-array (mod 2)      (*)) (array-structure-active data)) index)
		    0)
	      (decf (array-structure-count data)))
  :traverse (when (<= start (1- (length data)))
	      (loop for index fixnum
		 from start
		 to (if (< end 0)
		      (1- (length data))
		      (min end (1- (length data)))) do
		   (when (test-equal 1 (aref (the (simple-array (mod 2) (*))
					       (array-structure-active data))
					     index))
		     (symbol-macrolet ((value (aref (the (simple-array element-type (*))
						      (array-structure-values data))
						    index)))
		       body))))
  :pack     (values) ; can't pack an ordinary array
  :capacity (array-structure-max-capacity data)
  :count    (array-structure-count        data))

;;;--- SCHEME: CVECTOR ---;;;

;;; * values is an array holding the nonzero elements of the sparse-array;
;;; * index is an array of fixnums where the indices are stored;
;;; * max-capacity is the maximum number of elements that the cvector can handle
;;;   (it is the size of the internal arrays values and index);
;;; * count is the number of elements actually stored, which may be less than
;;;   max-capacity.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (cvector (:type vector))
    (values (make-array 0) :type simple-array)
    (index  (make-array 0  :element-type 'fixnum) :type (simple-array fixnum (*)))
    (max-capacity   0      :type fixnum)
    (count          0      :type fixnum)))

;;; The operations on cvectors are defined as macros because we need to generate
;;; code for different data types. If we switch to dynamic typing, array traversals
;;; can be more than 5 times slower.

;; Indices are set to -1 when the cvector is created.
(defmacro make-new-cvector (size &key (element-type t) (sparse-element 0))
  "Makes a new (empty) cvector with maximum capacity equal to size.
The element-type parameter can be used to make the internal representation more efficient.
The default element-type is T."
  `(make-cvector
    :values       (make-array ,size :element-type ',element-type :initial-element ,sparse-element)
    :index        (make-array ,size :element-type 'fixnum :initial-element -1)
    :max-capacity ,size))

(defmacro get-cvec (cv index sparse-element element-type test-equal)
  "Retrieves an element from a cvector, given its index.
If the element is found, returns (values element T).
If the element is not in the vector, returns (values sparse-element NIL) instead."
  (jp-utils:with-gensyms (pos found)
    `(multiple-value-bind (,pos ,found)
       (binary-search ,index ; fast
			     (cvector-index ,cv)
;			     :element-type fixnum
			     :end (the fixnum (1- (cvector-count ,cv)))
			     :not-found t)
       (declare (type fixnum ,pos)
		(type boolean ,found))
       (if (not ,found)
         (values (the ,element-type ,sparse-element) nil)
         (let ((result (aref (the (simple-array ,element-type (*))(cvector-values ,cv)) ,pos)))
           (values (the ,element-type result)
                   (the boolean (not (the boolean (,test-equal result ,sparse-element))))))))))

(defmacro resize-cvec (cv element-type new-size)
  "Resizes a cvector, adjusting the arrays for values and indices"
  `(setf
    (cvector-values ,cv) (robust-adjust-array (cvector-values ,cv) ,new-size :element-type ',element-type)
    (cvector-index ,cv)  (robust-adjust-array (cvector-index  ,cv) ,new-size :element-type 'fixnum)
    (cvector-max-capacity ,cv) ,new-size))

(defmacro pack-cvec (cv element-type)
  "Packs a cvector so cvector-count will be the same as cvector-max-capacity."
  `(resize-cvec ,cv ,element-type (cvector-count ,cv)))

(declaim (inline bsearch-fixnum/index))
;(pprint (macroexpand '
 (jp-utils:def-nonrec-binary-search bsearch-fixnum/index :element-type fixnum :not-found t)
; ))
(compile 'bsearch-fixnum/index)

(defmacro set-cvec (cv index sparse-element element-type value resize-function)
  "Sets an element in a cvector, given its index.
If the index is not in the vector, it is added, unless there is no more space left,
in which case an error is signaled."
  (declare (ignore sparse-element))
  (jp-utils:with-gensyms (pos found idx)
    (let ((data-vector-type `(simple-array ,element-type (*))))
      `(multiple-value-bind (,pos ,found)
#|	   (nonrec-binary-search
	    ,index
	    (cvector-index ,cv)
	    :not-found t
	    :start 0
	    :end (the fixnum (1- (cvector-count ,cv))))|#

	   (bsearch-fixnum/index
	    ,index
	    (cvector-index ,cv)
	    0
	    (the fixnum (1- (cvector-count ,cv))))
	 (declare (type fixnum ,pos)
		  (type boolean ,found))
	 (unless ,found
	   (when (>= (cvector-count ,cv) (cvector-max-capacity ,cv))
	     (resize-cvec ,cv ,element-type
			  (the fixnum ;(+ (cvector-max-capacity ,cv)
			    (funcall #',resize-function (cvector-max-capacity ,cv)))))
	   
	   (loop for ,idx fixnum from (cvector-count ,cv) downto (the fixnum (1+ ,pos)) do
	     (setf (aref (the ,data-vector-type (cvector-values ,cv)) ,idx)
		   (aref (the ,data-vector-type (cvector-values ,cv)) (1- ,idx)))
	     (setf (aref (the (simple-array fixnum (*)) (cvector-index  ,cv)) ,idx)
		   (aref (the (simple-array fixnum (*)) (cvector-index  ,cv)) (1- ,idx))))
	   (incf (cvector-count ,cv)))
	 (setf (aref (the ,data-vector-type (cvector-values ,cv)) ,pos)
	       (the ,element-type ,value))
	 (setf (aref (the (simple-array fixnum (*)) (cvector-index ,cv)) ,pos)
	       (the fixnum ,index))
	 (values)))))

(defmacro do-traverse-cvector ((index value cvec &key (element-type t) (start 0) (end -1) (setf t)) &body body)
  "Traverses a cvector, binding variables to the index and value.

element-type is used in type declarations inside the code; start and end are used
for indices; setf is a flag that makes the method use SYMBOL-MACROLET instead of
LET for the value binding (it is on by default).

An example follows (although the macro expansion is a bit convoluted):
 (macroexpand-1 '(spartns::do-traverse-cvector (i v d) b))

 ==>

(WHEN (<= 0 (AREF (CVECTOR-INDEX D) (1- (CVECTOR-COUNT D))))
  (LET ((#:FIXED-START-3597
         (MULTIPLE-VALUE-BIND (#:POS-3599 #:FOUND-3600)
             (BINARY-SEARCH 0 (CVECTOR-INDEX D) :END
                            (THE FIXNUM (1- (CVECTOR-COUNT D))) :NOT-FOUND T)
           #:POS-3599))
        (#:FIXED-END-3598
         (IF (< -1 0)
             (1- (CVECTOR-COUNT D))
             (MULTIPLE-VALUE-BIND (#:POS-3599 #:FOUND-3600)
                 (BINARY-SEARCH -1 (CVECTOR-INDEX D) :END
                                (THE FIXNUM (1- (CVECTOR-COUNT D))) :NOT-FOUND
                                T)
               (IF #:FOUND-3600
                   #:POS-3599
                   (- #:POS-3599 1))))))
    (LOOP FOR #:I-3596 FIXNUM FROM #:FIXED-START-3597 TO #:FIXED-END-3598
          DO (LET ((I (AREF (CVECTOR-INDEX D) #:I-3596)))
               (SYMBOL-MACROLET ((V
                                  (AREF
                                   (THE (SIMPLE-ARRAY T (*))
                                        (CVECTOR-VALUES D))
                                   #:I-3596)))
                 (DECLARE (TYPE T V)
                          (TYPE FIXNUM I))
                 B)))))
T"
  ;; FIXME: start and end should be indices, and we should search those indices
  ;; and then traverse only that part of the cvector.
  (jp-utils:with-gensyms (i fixed-start fixed-end pos found)
    (let ((the-let (if setf
		       'symbol-macrolet
		       'let)))
      ;; Don't traverse if start > largest stored index:
      `(when (<= ,start (aref (cvector-index ,cvec) (1- (cvector-count ,cvec))))
	 ;; Now find start and end positions of given indices:
	 (let ((,fixed-start
		(multiple-value-bind (,pos ,found)
		    (binary-search ,start
				   (cvector-index ,cvec)
					;		:element-type fixnum
				   :end (the fixnum (1- (cvector-count ,cvec)))
				   :not-found t)
	       ,pos))
	       (,fixed-end (if (< ,end 0)
			       (1- (cvector-count ,cvec))
			       (multiple-value-bind (,pos ,found)
				   (binary-search ,end ; fast
						  (cvector-index ,cvec)
					;			       :element-type fixnum
						  :end (the fixnum (1- (cvector-count ,cvec)))
						  :not-found t)
				 (if ,found ,pos  (- ,pos 1))))))
	   ;; And traverse!
	   (loop for ,i fixnum from ,fixed-start to ,fixed-end do
	     (let ((,index  (aref (cvector-index ,cvec) ,i)))
	       (,the-let ((,value  (aref (the (simple-array ,element-type (*))
					   (cvector-values ,cvec)) ,i)))
			 (declare (type ,element-type ,value)
				  (type fixnum ,index))
			 ,@body))))))))

;; We will add a representation scheme that uses cvectors:
(defscheme :name     cvector
  :type       (simple-vector 4)
  :make       (make-new-cvector (the fixnum size)
				:element-type element-type
				:sparse-element sparse-element)
  :get        (get-cvec data index sparse-element element-type test-equal)
  :set        (set-cvec data index sparse-element element-type value resize-function)
  :delete     (set-cvec data index sparse-element element-type sparse-element
			(lambda (x)
			  (declare (ignore x)) 0))
  :traverse   (do-traverse-cvector (index value data :start start
					             :end   end
						     :element-type element-type)
		body)
  :pack       (pack-cvec data element-type)
  :capacity   (cvector-max-capacity data)
  :count      (cvector-count data))

;;;--- AUXILIARY SPARTN FUNCTIONS AND MACROS ---;;;

;;; These functions and macros are used by the macros that define the functions
;;; which will operate on sparse tensors.

(defun sparse-element-for-next-dim (rep-list empty-dim-map sparse-element)
  "Returns the sparse element to be used in the next dimension: either the
sparse-element for the tensor (if this is the last dimension) or an empty
map using next dimension's representation."
  (if (null (cdr rep-list))
      sparse-element
      (gethash (second rep-list) empty-dim-map)))

(defun chained-make (&key rep-list non-zero-list element-type sparse-element empty-dim-map)
  "This function will return the code necessary to create the representation of one
of the dimensions of the sparse tensor."
  (let ((type-next-dim (if (null (cdr rep-list))
			    element-type
			    (gethash 'type (get-scheme (second rep-list))))))
    (expand-scheme 'spartns::make (car rep-list) `((size (car ,non-zero-list))
						   (element-type ,type-next-dim)
						   (sparse-element ,(sparse-element-for-next-dim
								     rep-list
								     empty-dim-map
								     sparse-element))))))

(defun chained-pack (&key data rep-list element-type)
  (if (null (cdr rep-list))
      `(progn ,(expand-scheme 'spartns::pack (car rep-list) `((data ,data)
							      (element-type ,element-type))))
      
      (let ((type (gethash 'type (get-scheme (second rep-list)))))
	(jp-utils:with-gensyms (trav-index trav-value)
	  `(progn
	    ,(expand-scheme 'spartns::pack (car rep-list) `((data ,data)
							    (element-type ,type)))
	    ,(expand-scheme 'spartns::traverse (car rep-list) `((data ,data)
								(index ,trav-index)
								(value ,trav-value)
								(element-type ,type)
								(start 0)
								(end -1)
								(body ,(chained-pack :data trav-value
										     :rep-list (cdr rep-list)
										     :element-type element-type)))))))))


(defun chained-get (&key data rep-list index-list element-type sparse-element block-name test-equal empty-dim-map)
  "This function will return the code necessary to access one element in
a sparse tensor, given the chained representation scheme.
- data will be inserted as is in the code. It is the container where the tensor is stored.
- rep-list MUST be a list of the representation schemes, and it CANNOT be a variable,
           because it will be used in macroexpansion time.
- index-list is inserted as is, and will be later replaced by the list of indices.
- element-type is inserted as is. It is the type of the stored elements.
- sparse-element is inserted as is. It is the value of the sparse element.
- block-name is just a name for the block of code. A wrapper function will use gensym
  for this.
This function should be used by a macro that creates the real function for accessing the
tensor."
  (jp-utils:with-gensyms (value found)
    (let* ((test (if (null (second rep-list))
		     test-equal
		     'eql))
	   (sparse (sparse-element-for-next-dim rep-list empty-dim-map sparse-element))
	   (type (if (null (second rep-list))
		     element-type
		     (gethash 'type (get-scheme (second rep-list)))))
	   (get-body (expand-scheme 'spartns::get (car rep-list) `((data ,data)
								   (test-equal ,test)
								   (index (the fixnum ,(car index-list)))
								   (sparse-element ,sparse)
								   (element-type ,type)))))
      `(progn (multiple-value-bind (,value ,found) ,get-body
		(declare (type boolean ,found))
		(if ,found
		    ,(if (null (second rep-list)) ; last dimension, return:
			 `(return-from ,block-name (the ,element-type ,value))
			 (chained-get :data           value  ; not last, recurse!
				      :rep-list       (cdr rep-list) 
				      :index-list     (cdr index-list)
				      :element-type   element-type
				      :sparse-element sparse-element
				      :block-name     block-name
				      :test-equal     test-equal
				      :empty-dim-map  empty-dim-map))
		    (return-from ,block-name (the ,element-type ,sparse-element))))))))

(defun create-next-dim (value data rep-list index-list non-zero-list element-type sparse-element resize-function)
  "This function is used by chained-set to create a new structure when necessary."
  (jp-utils:with-gensyms (new-structure)
    (let* ((next-rep  (second rep-list))
	   (third-rep (third rep-list))
	   (second-type (if (null next-rep)
			    element-type
			    (gethash 'type (get-scheme next-rep))))
	   (third-type (if (null third-rep) ; either elements (final dim) or indices
			   element-type
			   (gethash 'type (get-scheme third-rep))))
	   (third-sparse (if (null third-rep)
			     sparse-element
			     (expand-scheme 'spartns::make third-rep `((size 0)
								       (element-type fixnum)
								       (sparse-element 0)))))
	   (make-body (expand-scheme 'spartns::make next-rep `((size ,(second non-zero-list))
							       (element-type ,third-type)
							       (sparse-element ,third-sparse)))))
      `(let ((,new-structure ,make-body))
	(setf ,value ,new-structure)
	,(expand-scheme 'spartns::set (car rep-list) `((data ,data)
						       (index (the fixnum ,(car index-list)))
						       (value ,value)
						       (element-type ,second-type)
						       (resize-function ,resize-function)))))))


(defun chained-set (&key
		    data
		    rep-list
		    index-list
		    new-value
		    non-zero-list
		    element-type
		    sparse-element
		    block-name
		    test-equal
		    empty-dim-map
		    resize-function)
  "This function will return the code necessary to store one element in
a sparse tensor, given the chained representation scheme.
- data will be inserted as is in the code. It is the container where the tensor is stored.
- rep-list MUST be a list of the representation schemes, and it CANNOT be a variable,
           because it will be used in macroexpansion time.
- index-list is inserted as is, and will be later replaced by the list of indices.
- new-value will be inserted as is. It is the value to be stored in the tensor.
- element-type is inserted as is. It is the type of the stored elements.
- sparse-element is inserted as is. It is the value of the sparse element.
- block-name is just a name for the block of code. A wrapper function will use gensym
  for this."
  (jp-utils:with-gensyms (value found)
    (let* ((test (if (null (second rep-list))
		     test-equal
		     'eql))
	   (next-rep (second rep-list))
	   (sparse (sparse-element-for-next-dim rep-list empty-dim-map sparse-element))
	   (type (if (null next-rep)
		     element-type
		     (gethash 'type (get-scheme next-rep))))
	   (get-body (expand-scheme 'spartns::get (car rep-list) `((data ,data)
								   (test-equal ,test)
								   (index (the fixnum ,(car index-list)))
								   (sparse-element ,sparse)
								   (element-type ,type))))
	   (set-body (expand-scheme 'spartns::set (car rep-list) `((data ,data)
								   (index (the fixnum ,(car index-list)))
								   (value ,new-value)
								   (element-type ,type)
								   (resize-function ,resize-function)))))
      (if (null next-rep) ; last dimension, set it:
	  set-body
	  `(multiple-value-bind (,value ,found) ,get-body
	     (declare (type boolean ,found))  ; not last... create next if necessary, then recurse:
	     (unless ,found
	       ,(create-next-dim value data rep-list index-list non-zero-list element-type sparse-element resize-function))
	     ,(chained-set :data          value          ; not last, recurse!
			   :rep-list      (cdr rep-list)
			   :index-list    (cdr index-list)
			   :new-value      new-value
			   :non-zero-list  (cdr non-zero-list)
			   :element-type   element-type
			   :sparse-element sparse-element
			   :block-name     block-name
			   :test-equal     test-equal
			   :empty-dim-map  empty-dim-map
			   :resize-function  resize-function))))))

	     
(defun chained-delete (&key data rep-list index-list element-type sparse-element block-name test-equal empty-dim-map)
  "This function will return the code necessary to delete one element from
a sparse tensor, given the chained representation scheme.
- data will be inserted as is in the code. It is the container where the tensor is stored.
- rep-list MUST be a list of the representation schemes, and it CANNOT be a variable,
           because it will be used in macroexpansion time.
- index-list is inserted as is, and will be later replaced by the list of indices.
- element-type is inserted as is. It is the type of the stored elements.
- sparse-element is inserted as is. It is the value of the sparse element.
- block-name is just a name for the block of code. A wrapper function will use gensym
  for this."
  (jp-utils:with-gensyms (value found)
    (let* ((test (if (null (second rep-list))
		     test-equal
		     'eql))
	   (sparse (sparse-element-for-next-dim rep-list empty-dim-map sparse-element))
	   (type (if (null (second rep-list))
		     element-type
		     (gethash 'type (get-scheme (second rep-list)))))
	   (get-body    (expand-scheme 'spartns::get (car rep-list) `((data ,data)
								      (test-equal ,test)
								      (index (the fixnum ,(car index-list)))
								      (sparse-element ,sparse)
								      (element-type ,type))))
	   (delete-body (expand-scheme 'spartns::delete (car rep-list) `((data ,data)
									 (index (the fixnum ,(car index-list)))
									 (sparse-element ,sparse)
									 (element-type ,element-type)))))
      `(multiple-value-bind (,value ,found) ,get-body
	 (declare (type boolean ,found)
		  (ignorable ,value))
	 (if ,found
	     ,(if (null (second rep-list)) ; last dimension, return:
		  `(progn
		     ,delete-body
		     (return-from ,block-name (values)))
		  (chained-delete :data           value    ; not last, recurse!
				  :rep-list       (cdr rep-list)
				  :index-list     (cdr index-list)
				  :element-type   element-type
				  :sparse-element sparse-element
				  :block-name     block-name
				  :test-equal     test-equal
				  :empty-dim-map  empty-dim-map))
	     (return-from ,block-name (values)))))))


(defun traverse-spartn-aux (index-list
			    value
			    data
			    element-type
			    sparse-element
			    rep-list
			    test-equal
			    dont-bind
			    empty-dim-map
			    body)
  "This is an auxiliary function that is used by the spartn-generic-traverse macro.
It will produce the code to traverse a sparse tensor, given the representation
scheme.

dont-bind is a list of indices not to bind."
  (declare (optimize (debug 3)))
  (when (null rep-list)
    (return-from traverse-spartn-aux body))
 
  (let* ((the-value (if (null (cdr rep-list)) ; either a new var, or the var given to be bound:
			value
			(gensym "NEW-VAR-")))
	 (test (if (null (second rep-list))
		   test-equal
		   'eql))
	 (type (if (null (second rep-list))
		   element-type
		   (gethash 'type (get-scheme (second rep-list))))))

    (jp-utils:with-gensyms (the-start the-end new-index)
      (let ((start 0)
	    (end -1)
	    (final-index (car index-list)))
	(when (member (car index-list) dont-bind)
	  (setf start (car index-list)
		end   (car index-list)
		final-index new-index))

	;; start and end could be set to "index", so they'll be mixed up. Here
	;; we split them:
	`(let ((,the-start ,start)
	       (,the-end ,end))
	   ,(expand-scheme 'spartns::traverse
			   (car rep-list)
			   `((data ,data)
			     (test-equal ,test)
			     (index ,final-index)
			     (value ,the-value)
			     (element-type ,type)
			     (sparse-element ,sparse-element)
			     (start ,the-start)
			     (end ,the-end)
			     (body ,(traverse-spartn-aux (cdr index-list)
							 value
							 the-value
							 element-type
							 sparse-element
							 (cdr rep-list)
							 test-equal
							 dont-bind
							 empty-dim-map
							 body)))))))))
  

(defmacro spartn-generic-traverse ((index-list value data)
				   rep-list
				   element-type
				   sparse-element
				   test-equal
				   dont-bind
				   &body body)
  "Traverses a sparse tensor, given:
- A list of symbols (that will be bound to the indices)
- A symbol to be bound to the value
- The data
- The representation scheme
- The body"
  (multiple-value-bind (make-empty-dims empty-dim-map)
      (make-empty-dimensions rep-list)
    (append make-empty-dims
	    (list (traverse-spartn-aux index-list
				       value
				       data
				       element-type
				       sparse-element
				       rep-list
				       test-equal
				       dont-bind
				       empty-dim-map
				       `(progn ,@body))))))

;;;--- SPARTN FUNCTION GENERATORS ---;;;

;;; These functions will generate functions to operate on Spartn structures.

(defun def-spartn-get (fun-name rep-list type sparse-element test-equal empty-dim-map
		       &key (declare '(optimize (speed 3) (safety 1))) (def NIL))
  "Creates a function to get elements in a tensor."
;;  (assert (eql (type-of sparse-element) type))
  (check-type rep-list list)
  (jp-utils:with-gensyms (block-name data)
    (let* ((index-list (loop repeat (length rep-list) collect (gensym "INDEX-")))
	   (definition `(,fun-name (,data ,@index-list)
			 ,(format nil "This function retrieves an element from a sparse tensor. The element type is
~a, the sparse element is ~a and the tensor is represented using the scheme ~a." type sparse-element rep-list)
			 (declare ,declare)
			 (declare (type ,(gethash 'spartns::type (get-scheme (car rep-list)))
					       ,data) 
					 (type fixnum ,@index-list))
			 (block ,block-name
			   ,(chained-get :data           data
					 :rep-list       rep-list
					 :index-list     index-list
					 :element-type   type
					 :sparse-element sparse-element
					 :block-name     block-name
					 :test-equal     test-equal
					 :empty-dim-map  empty-dim-map)))))
      (if (null def)
	  definition
	  (append (list 'defun) definition)))))

(defun def-spartn-pack (fun-name rep-list type &key (def NIL))
  "Creates a function that packs a sparse tensor."
  (check-type rep-list list)
  (jp-utils:with-gensyms (data)
    (let ((definition `(,fun-name (,data)
			,(format nil "This function packs a sparse tensor with element type ~a and representation scheme ~a."
				 type
				 rep-list)
			,(chained-pack :data data :rep-list rep-list :element-type type))))
      (if (null def)
	  definition
	  (append (list 'defun) definition)))))


(defun def-spartn-set (fun-name rep-list type sparse-element non-zero-list test-equal empty-dim-map resize-function
		       &key (declare '(optimize (speed 3) (safety 1))) (def NIL))
  "Creates a function to set elements in a tensor."
;;  (assert (eql (type-of sparse-element) type))
  (check-type rep-list list)
  (jp-utils:with-gensyms (block-name data value)
    (let* ((index-list (loop repeat (length rep-list) collect (gensym "INDEX-")))
	   (definition `(,fun-name (,data ,@index-list ,value)
				   ,(format nil "This function sets an element on a sparse tensor. The element type is
~a, the sparse element is ~a and the tensor is represented using the scheme ~a." type sparse-element rep-list)
				   (declare ,declare)
				   (block ,block-name
				     ,(chained-set :data           data
						   :rep-list       rep-list
						   :index-list     index-list
						   :new-value      value
						   :non-zero-list  non-zero-list
						   :element-type   type
						   :sparse-element sparse-element
						   :block-name     block-name
						   :test-equal     test-equal
						   :resize-function  resize-function
						   :empty-dim-map  empty-dim-map)))))
      ;; If we don't return (values), CMUCL complains about not knowing the return type:
      (if (null def)
	  (append definition (list '(values)))
	  (append (list 'defun) definition (list '(values)))))))


(defun def-spartn-delete (fun-name rep-list type sparse-element test-equal empty-dim-map
			  &key (declare '(optimize (speed 3) (safety 1))) (def NIL))
  "Creates a function to delete elements from a tensor."
  ;;  (assert (eql (type-of sparse-element) type))
  (check-type rep-list list)
  (jp-utils:with-gensyms (block-name data)
    (let* ((index-list (loop repeat (length rep-list) collect (gensym "INDEX-")))
	   (definition `(,fun-name (,data ,@index-list)
				   ,(format nil "This function deletes an element from a sparse tensor. The element type is
~a, the sparse element is ~a and the tensor is represented using the scheme ~a." type sparse-element rep-list)
				   (declare ,declare)
				   (block ,block-name
				     ,(chained-delete :data           data
						      :rep-list       rep-list
						      :index-list     index-list
						      :element-type   type
						      :sparse-element sparse-element
						      :block-name     block-name
						      :test-equal     test-equal
						      :empty-dim-map  empty-dim-map)))))
      ;; If we don't return (values), CMUCL complains about not knowing the return type:
      (if (null def)
	  (append definition (list '(values)))
	  (append (list 'defun) definition (list '(values)))))))

(defun def-spartn-make (fun-name rep-list type sparse non-zero-list empty-dim-map
			&key (declare '(optimize (speed 3) (safety 1))) (def NIL))
  "Creates a function to initialize a tensor."
  (check-type rep-list list)
  (check-type non-zero-list list)
  (let ((definition `(,fun-name (&key (non-zeros ',non-zero-list))
		      ,(format nil "This function creates a sparse tensor with element type ~a and representation scheme ~a."
			       type
			       rep-list)
		      (declare ,declare)
		      ,(chained-make :rep-list rep-list
				     :non-zero-list 'non-zeros
				     :element-type type
				     :sparse-element sparse
				     :empty-dim-map empty-dim-map))))
    (if (null def)
	definition
	(append (list 'defun) definition))))

;; I tried not using LIST and using only quasiquotation, and I went nuts.
;; But it works now:
(defun def-spartn-traverse (mac-name rep-list type sparse test-equal &key (def NIL))
  "Creates a macro for traversing one type of spartn, given the representation scheme (as a list), the element
type, the sparse element and the desired name for the new macro.
For example:
 (def-spartn-traverse do-hhd (hash hash) double-float 0d0 #'=)
This will create a macro DO-HHD which can be used like this:
 (do-hhd ((i j) val data)
   (format t \"The value at (~a, ~a) is ~a\" i j val))"
  (jp-utils:with-gensyms (value data)
    (let* ((index-list (loop repeat (length rep-list) collect (gensym)))
	   (definition `(,mac-name ((,index-list ,value ,data &key (dont-bind NIL)) &body body)
				   (list 'spartn-generic-traverse
					 (list (list ,@index-list)
					       ,value
					       ,data)
					 ',rep-list
					 ',type
					 ',sparse
				         ',test-equal
					 dont-bind
					 (push 'progn body)))))
      (if (null def)
	  definition
	  (append (list 'defmacro) definition)))))


(defun def-spartn-copy (fun-name scheme-name &key  (declare '(optimize (speed 3) (safety 1))) (def NIL))
  "Creates a fuction that copies a spartn.

Examples:

 (def-spartn-copy 'my-copy-function 'hash)
 ==>
 (MY-COPY-FUNCTION (#:DATA-817) \"Copies a sparse tensor of type HASH\"
   (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 1)))
   (SPARTN-CONVERT #:DATA-817 :FROM HASH :TO HASH :DESTRUCTIVE NIL))

 (def-spartn-copy 'my-copy-function 'hash :def t)
 ==>
  (DEFUN MY-COPY-FUNCTION (#:DATA-820)
     \"Copies a sparse tensor of type HASH\"
     (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 1)))
     (SPARTN-CONVERT #:DATA-820 :FROM HASH :TO HASH :DESTRUCTIVE NIL))"
  (declare (optimize (debug 3)))
  (check-type scheme-name symbol)
  (jp-utils:with-gensyms (data)
    (let ((definition `(,fun-name (,data)
				  ,(format nil "Copies a sparse tensor of type ~a" scheme-name)
				  (declare ,declare) ;; XCL gets confused here
				  (spartns:spartn-convert ,data
							  :from ,scheme-name
							  :to ,scheme-name
							  :destructive nil))))
      (if (null def)
	  definition
	  (append (list 'defun) definition)))))


;;;--- SPARTNS ---;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct spartn-scheme
    representation
    non-zero
    element-type
    sparse-element
    declare
    test-equal
    resize-function))

(defun define-spartn-functions (name spartn-scheme empty-dim-map empty-dim-defs &key (def nil) (declare nil))
  "Returns the code for the functions used with a spartn scheme. This is used by defspartn and w/spartns.
If DEF is T, then this function will generate top-level code to define the functions:

 (DEFUN get-X (tensor index) ...)

If DEF is NIL, then only the names, lambda-lists and bodies are generated:

 (get-X (tensor index) ...)

The first form is used by defspartn (which is supposed to be used as a top-leval form), and the second
is used by w/spartns (which will use LABELS to create local functions)."
  (let ((make-fun-name     (format-symbol "MAKE-~a"     name))
	(get-fun-name      (format-symbol "GET-~a"      name))
	(set-fun-name      (format-symbol "SET-~a"      name))
	(delete-fun-name   (format-symbol "DELETE-~a"   name))
	(pack-fun-name     (format-symbol "PACK-~a"     name))
	(copy-fun-name     (format-symbol "COPY-~a"     name))
	(representation    (spartn-scheme-representation spartn-scheme))
	(element-type      (spartn-scheme-element-type   spartn-scheme))
	(sparse-element    (spartn-scheme-sparse-element spartn-scheme))
	(non-zero          (spartn-scheme-non-zero       spartn-scheme))
	(test-equal        (spartn-scheme-test-equal     spartn-scheme))
	(resize-function   (spartn-scheme-resize-function spartn-scheme))
	(dec               (cond ((not (null declare))
				  declare)
				 ((not (null (spartn-scheme-declare spartn-scheme)))
				  (spartn-scheme-declare spartn-scheme))
				 (t
				  '(optimize (speed 3) (safety 1))))))
    (let ((defs
	      `(,(def-spartn-make     make-fun-name     representation element-type sparse-element  non-zero            empty-dim-map :declare dec :def def)
		,(def-spartn-get      get-fun-name      representation element-type sparse-element           test-equal empty-dim-map :declare dec :def def)
		,(def-spartn-set      set-fun-name      representation element-type sparse-element  non-zero test-equal empty-dim-map resize-function :declare dec :def def)
		,(def-spartn-delete   delete-fun-name   representation element-type sparse-element           test-equal empty-dim-map :declare dec :def def)
		
		,(def-spartn-pack     pack-fun-name     representation element-type                                                                :def def)
		,(def-spartn-copy     copy-fun-name     name                                                                          :declare dec :def def))))
      (if def
	  defs
	  (append empty-dim-defs defs)))))

(defun define-spartn-macros (name spartn-scheme empty-dim-map &key (def nil))
  "Returns the code for the macros used with a spartn scheme. This is used by defspartn and w/spartns.
If DEF is T, then this function will generate top-level code to define the functions and macros:

 (DEFMACRO traverse-X (index-list value tensor &body body) ...)

If DEF is NIL, then only the names, lambda-lists and bodies are generated:

 (traverse-X (index-list value tensor &body body) ...)

The first form is used by defspartn (which is supposed to be used as a top-leval form), and the second
is used by w/spartns (which will use MACROLET to create local macros)."
  (declare (ignorable empty-dim-map))
  (let ((traverse-fun-name (format-symbol "TRAVERSE-~a" name))
	(representation    (spartn-scheme-representation spartn-scheme))
	(element-type      (spartn-scheme-element-type   spartn-scheme))
	(sparse-element    (spartn-scheme-sparse-element spartn-scheme))
	(test-equal        (spartn-scheme-test-equal     spartn-scheme)))
    `(,(def-spartn-traverse
	traverse-fun-name
	representation
	element-type
	sparse-element
	test-equal
	:def def))))

(defun get-create (rep)
  "Returns the Lisp code to create an empty dimension, given its representation.
Examples:

 (get-create 'hash)
 ==>
   (MAKE-HASH-TABLE :SIZE 0)

 (get-create 'cvector)
 ==>
   (MAKE-NEW-CVECTOR (THE FIXNUM 0) :ELEMENT-TYPE FIXNUM :SPARSE-ELEMENT 0)"
  (expand-scheme 'spartns::make (gethash 'spartns::name (get-scheme rep))
		 '((size 0)
		   (element-type fixnum)
		   (sparse-element 0))))


(defun make-empty-dimensions (rep-list)
  "Given arepresentation list, this function returns two values:
the Lisp code for creating empty dimensions for those representations,
and a hashtable that maps symbols like EMPTY-REP- to those empty
structures. For example,

 (make-empty-dimensions '(cvector hash array)
    ==>
   (LET ((#:EMPTY-ARRAY-847
       (MAKE-ARRAY-STRUCTURE :VALUES
                             (MAKE-ARRAY 0 :ELEMENT-TYPE 'FIXNUM
                                         :INITIAL-ELEMENT (THE FIXNUM 0))
                             :ACTIVE
                             (MAKE-ARRAY 0 :ELEMENT-TYPE '(MOD 2)
                                         :INITIAL-ELEMENT 0)
                             :MAX-CAPACITY 0))
      (#:EMPTY-HASH-846 (MAKE-HASH-TABLE :SIZE 0)))),
      #<HASHTABLE :TEST EQL :COUNT 2 {129910020})

The second value returned (the hashtable) maps:
hash    --> #:EMPTY-HASH-846
array   --> #:EMPTY-ARRAY-847

The cvector scheme was not included because it is the first in the
representation list."
  (let ((defs nil)
	(empty-dimensions (make-hash-table)))
    (loop for rep in (cdr rep-list) do
	  (when (null (gethash rep empty-dimensions))
	    (let ((symbol (gensym (string-upcase (format nil "empty-~a-" rep)))))
	      (push `(,symbol ,(get-create rep)) defs)
	      (setf (gethash rep empty-dimensions) symbol))))
    (values `(let ,defs)
	    empty-dimensions)))

(defun check-defspartn-parameters (name
				   representation
				   non-zero
				   element-type
				   sparse-element
				   test-equal
				   resize-function
				   resize-amount
				   def
				   declare)
  "This function checks the types of some parameters to defspartn."
  (declare (ignorable element-type
		      sparse-element
		      test-equal
		      def
		      declare))
  ;; FIXME: there should be an easier way...
  (assert (eql (type-of name) 'symbol))
  (assert (eql (type-of representation) 'cons))
  (assert (eql (type-of non-zero) 'cons))
  (assert (eql (type-of resize-function) 'cons))
  (assert (subtypep (type-of resize-amount) 'integer))
  
  ;; GCL doesn't seem to like the assertions below:
  ;;  (assert (eql (type-of def) 'boolean))
  ;;  (assert (eql (type-of declare) 'list))
  ;;(format t "+++++++++++++++++++ + + + + + def ~a  declare ~a ~%" (type-of def) (type-of declare))
  
  ;; MUST specify non-zero for ALL dimensions:
  (assert (eql (length non-zero)
	       (length representation))))


(defmacro defspartn (name &key
		     representation
		     non-zero
		     element-type
		     sparse-element
		     (test-equal 'eql)
		     (resize-function '(lambda (x) (* x 2)))
		     (resize-amount   -1)
		     (def t)
		     (declare nil))
  "Defines a new spartn scheme.
If the DEF parameter is not NIL, then functions and macros will be created to access the spartn
scheme.

If the name of the new scheme is X, then the following functions will be defined:
- (MAKE-X)
- (GET-X tensor index)
- (SET-X tensor index value)
- (DELETE-X tensor index)
- (PACK-X tensor)
- (COPY-X tensor)
The macro (TRAVERSE-X index-list value tensor body) will also be defined.

NON-ZERO is just a hint so that containers in each dimension will be created with approximately
this size. It is not a hard limit, and there is no guarantee that this will actually be the size
of the containers.

RESIZE-FUNCTION is a function that tells the amount by which container sizes is increased.
The default value is (lambda (x) 200). This may or may not be used, depending on the representation scheme.

The default value of DEF is T. You can set it to NIL if you want to use w/spartns (which will create
the same macros and functions locally anyway).

The parameter DECLARE is used to include declarations inside the defined functions. By default it is
 (optimize (speed 3) (safety 1))."
  ;; NON-ZERO is all 50 by default:
  (when (> resize-amount -1)
    (setq resize-function `(lambda (x) (+ x ,resize-amount))))
  
  (when (null non-zero)
    (setf non-zero (loop repeat (length representation) collect 50)))
  
  (check-defspartn-parameters name representation non-zero element-type sparse-element test-equal resize-function resize-amount def declare)
  (let ((new-spartn (make-spartn-scheme :representation representation
					:non-zero       non-zero
					:element-type   element-type
					:sparse-element sparse-element
					:test-equal     test-equal
					:resize-function  resize-function
					:declare        declare)))
    (setf (gethash name *spartn-schemes*) new-spartn)
    (multiple-value-bind (empty-dim-defs empty-dim-map)
	(make-empty-dimensions representation)
      (if def
	  (let ((macros (define-spartn-macros       name new-spartn empty-dim-map :def t))
		(functions (define-spartn-functions name new-spartn empty-dim-map empty-dim-defs :declare declare :def t)))
	    (append (push 'progn macros) `(,(append empty-dim-defs `(,@functions)))))
	  (values)))))

(defmacro w/spartns (spartn-list &body body)
  "Creates local definitions for functions and macros to access a spartn scheme.
These are the same functions and macros that DEFSPARTN creates if you pass it DEF=T.

This is useful if you don't want the overhead of function calls (including boxing and unboxing
of numbers). This macro will use LABELS and MACROLET to create the functions and macros
that access the spartns in spartn-list.

Example:
  (defspartn xyz
      ...
      :def          nil)

  (w/spartns (xyz)
    (let ((s (make-xyz)))
      (set-xyz s '(0 0 0) 0.5)
      ...))

The w/spartns S-expression will be expanded into:
  (labels ((make-xyz (...))
           (get-xyz  (...))
           (set-xyz  (...))
           ...)
    (macrolet ((traverse-xyz ...))
       (let ((s (make-xyz)))
         (set-xyz s '(0 0 0) 0.5)
         ...)))"
  
  (let* ((empty-dim-defs (list))
	 (empty-dim-map  (make-hash-table))
	 (macros
	  (loop for spartn-scheme-name in spartn-list
		append (define-spartn-macros
			   spartn-scheme-name
			   (gethash spartn-scheme-name *spartn-schemes*)
			 (make-hash-table)
			 :def nil)))
	 (functions
	  (let ((empty-dim-defs-tmp) (empty-dim-map-tmp))
	    (loop for spartn-scheme-name in spartn-list
		  do (multiple-value-setq (empty-dim-defs-tmp empty-dim-map-tmp)
		       (make-empty-dimensions (spartn-scheme-representation
					       (gethash spartn-scheme-name *spartn-schemes*))))
		  (setf empty-dim-defs (nconc empty-dim-defs (cadr empty-dim-defs-tmp))) ; remove LET and ((
		  (maphash #'(lambda (k v)
			       (setf (gethash k empty-dim-map) v))
			   empty-dim-map-tmp)
		  append (define-spartn-functions
			     spartn-scheme-name
			     (gethash spartn-scheme-name *spartn-schemes*)
			   empty-dim-map
			   NIL
			   :def nil)))))
    `(let ,empty-dim-defs
	  (macrolet ,macros
	    (labels ,functions
	      ,@body)))))


;;;--- CONVERTS BETWEEN SPARTN REPRESENTATIONS ---;;;

(defmacro spartn-convert (old-data &key from to (destructive nil))
  "Returns a new spartn using a different representation but with the same contents as the one
given. from and to must be the names of the representation schemes (symbols, not strings).

Example:

 (defspartn hashhash
    :representation '(hash hash)
    ...)

 (defspartn
    :representation '(hash cvector)
    ...)

 (let ((mat (make-hashash)))
  (let ((mat2 (spartn-convert mat :from hashash :to hashcvec)))
    (get-hashvec mat2 '(2 3))))

Here MAT uses spartn scheme HASHHASH, which represents the tensor as a hashtable of hashtables.
MAT2 is obtained by converting MAT to scheme HASHCVEC, which represents the tensor as a hashtable
of compressed vectors."
  (jp-utils:with-gensyms (new-data value)
    (let* ((make-to       (format-symbol "MAKE-~A" to))
	   (set-to        (format-symbol "SET-~A" to))
	   (traverse-from (format-symbol "TRAVERSE-~A" from))
	   (delete-from   (format-symbol "DELETE-~A" from))
	   (from-repr     (spartn-scheme-representation (gethash from *spartn-schemes*)))
	   (index-list (loop repeat (length from-repr) collect (gensym "INDEX-"))))
      `(let ((,new-data (,make-to)))
	 (,traverse-from (,index-list ,value ,old-data)
			 (,set-to ,new-data ,@index-list ,value)
			 (when ,destructive
			   (,delete-from ,old-data ,@index-list)))
	 ,new-data))))


;;;--- MULTIPLE TRAVERSALS ---;;;

(defun split-indices-values-dont-bind (trav)
  "Returns index-list values dont-bind"

  ;; There should br at least (TYPE DATA INDEX VALUE) in the list; there may be more
  ;; index variables and the dont'bind parameter, but theminimum is four arguments.
  (assert (> (length trav) 3))
  
  (let ((the-list (reverse (cddr trav))))
    (if (eql (cadr the-list) :dont-bind)
	(values (reverse (cdddr the-list)) (caddr the-list) (car the-list))
	(values (reverse (cdr the-list))   (car the-list) NIL))))

 
(defun split-traversal-descriptions (traversal-list)
  "Splits the descriptions of several traversals into several lists: one for
spartn type, one for the data containers, one for the variable to bind for value and
one for the lists of variables to bind for indices.

For example,
 (split-traversal-descriptions '((A data-a i j k l val-a)
                                 (B data-b x y z   val-b :dont-bind (x))
                                 (C data-c a b c   val-c)))
 ==>

 (values (A B C)
	 (DATA-A DATA-B DATA-C)
	 (VAL-A VAL-B VAL-C)
	 ((I J K L) (X Y Z) (A B C))
         (NIL (x) NIL))

This is used by the macro w/spartns."
  (declare (optimize (debug 3)))
  (let ((types NIL)
	(datas NIL)
	(values NIL)
	(indices-lists NIL)
	(dont-bind-lists NIL))
    (loop for trav in traversal-list do
       (push (first trav)   types)
       (push (second trav)  datas)
       (multiple-value-bind (index-list value dont-bind)
	   (split-indices-values-dont-bind trav)
	 (push index-list  indices-lists)
	 (push value       values)
	 (push dont-bind   dont-bind-lists)))
       
    (values (reverse types)
	    (reverse datas)
	    (reverse values)
	    (reverse indices-lists)
	    (reverse dont-bind-lists))))


(defun with-spartn-traversals-aux (spartn-type-list data-list index-list-list value-list dont-binds used-index-list body)
  "Generates the code to traverse several spartns, possibly with shared indices.
This function does the heavy work, while w/spartn-traversals is the macro that exposes the
functionality.

 (with-spartn-traversals-aux '(number2d fixnum2d) '(A B) '((I J) (x y)) '(va vb) '(pprint))

==>

 (TRAVERSE-NUMBER2D ((I J) VA A)
   (TRAVERSE-FIXNUM2D ((X Y) VB B)
     (PROGN PPRINT))

Now an example with an excluded index:

 (with-spartn-traversals-aux '(number-2d fixnum-2d)
                             '(A B)
                             '((I J) (x y))
                             '(va vb)
                             '(x)  ;; <======= HERE!
                             '(progn
                                (pprint x)
                                (pprint y)
                                (pprint va)))

==>

 (TRAVERSE-NUMBER-2D ((I J) VA A)
  (TRAVERSE-FIXNUM-2D ((#:B-X-803 Y) VB B)
   (WHEN (AND (EQL X #:B-X-803))
     (PROGN PROGN (PPRINT X) (PPRINT Y) (PPRINT VA)))))

Note that the X index was excluded from the variables over which
we loop. The code will loop over #:B-X-803 (locally created), but
the body will only be evaluated if it is equal to X, which should
be set outside the loop. This is equivalent to using a fixed value
for one index."
  (declare (optimize (debug 3)))
  (if (null spartn-type-list)
      `(progn ,@body)
      (let* ((traversal-name  (format-symbol "TRAVERSE-~A" (car spartn-type-list)))

	     ;; If index I is used in an outer traversal, it is declared
	     ;; "dont-bind" in inner traversals.
	     (this-index-list (car index-list-list))
	     (extra-dont-bind (intersection this-index-list
					    used-index-list))
	     (final-dont-binds (union (car dont-binds)
				      extra-dont-bind))

	     (traversal-body (with-spartn-traversals-aux (cdr spartn-type-list)
				(cdr data-list)
				(cdr index-list-list)
				(cdr value-list)
				(cdr dont-binds)
				(union used-index-list this-index-list)
				body)))
	`(,traversal-name (,this-index-list
			   ,(car value-list)
			   ,(car data-list)
			   :dont-bind ,final-dont-binds)
			  ,traversal-body))))
	
(defmacro w/spartn-traversals (traversal-list &body body)
  "Traverses several tensors, possibly synchronizing indices. For example:

 (w/spartn-traversals ((A data-a i j k l val-a)
                       (B data-b i j k   val-b)
   ...)

Will traverse data-a and data-b (of type A and B respectively), looping over l.

 (w/spartn-traversals ((A data-a i j k l val-a)
                       (B data-b a b i j val-b)
                       (C data-c i a m n val-c :dont-bind (b m)))
   ...)

Will traverse data-a, data-b and data-c, but the only entries will be those
such that the first index on data-a and data-c are equal to that of the second
index of data-b, and so on."
  (declare (optimize (debug 3)))
  (multiple-value-bind (type-list data-list value-list index-list dont-binds)
      (split-traversal-descriptions traversal-list)
    (with-spartn-traversals-aux type-list data-list index-list value-list dont-binds () body)))

		      
;;;--- FAST TRAVERSALS ---;;;


(defun collect-non-zeros-into-fast-traversal (symbol-list
					      array-list
					      types-list
					      position)
  "Produces the code to SETF values in a fast traversal:

 (collect-non-zeros-into-fast-traversal '(i j k v w)
                                        '(II JJ KK VV WW)
                                        '(fixnum fixnum fixnum double-float double-float)
                                        10)
  ==>
 ((SETF (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) II) 10) I)
  (SETF (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) JJ) 10) J)
  (SETF (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) KK) 10) K)
  (SETF (AREF (THE (SIMPLE-ARRAY DOUBLE-FLOAT (*)) VV) 10) V)
  (SETF (AREF (THE (SIMPLE-ARRAY DOUBLE-FLOAT (*)) WW) 10) W))"
  (loop
     for symbol      in symbol-list
     for value-array in array-list
     for type        in types-list
     collect `(setf (aref (the (simple-array ,type (*)) ,value-array)
			  ,position) ,symbol)))


(defun flatten (list)
  "Flattens a list."
  (cond ((null list)
	 nil)
	((atom list)
	 (list list))
	(t (append (flatten (car list))
		   (flatten (cdr list))))))


(defun make-types-list (type-list index-symbol-list)
  "Make s alist of types:

 (make-types-list '(hash-float cvector-double) '(i j k l))

==>
 '(single-float double-float fixnum fixnum fixnum fixnum)

type-list is a list of spartn schemes."
  (labels ((spartn-data-type (spartn)
		    (spartn-scheme-element-type (gethash spartn *spartn-schemes*))))
  (append (mapcar #'spartn-data-type type-list)
	  (loop repeat (length index-symbol-list) collect 'fixnum))))

(defun create-and-bind-arrays (non-zeros symbol-list type-list)
  "Generates the code to create and bind arrays, and also the
type declarations. For example:

 (multiple-value-bind (n b d)
     (create-and-bind-arrays 4 '(x y z w) '(fixnum character double-float fixnum))
   (format t \"=======n:~%~a~%=======b:~%~a~%=======d:~%~a~%\" n b d))

The output is:

 (ARRAY-X-2960 ARRAY-Y-2961 ARRAY-Z-2962 ARRAY-W-2963)
 =======b:
 ((ARRAY-X-2960 (MAKE-ARRAY 4 ELEMENT-TYPE FIXNUM))
  (ARRAY-Y-2961 (MAKE-ARRAY 4 ELEMENT-TYPE CHARACTER))
  (ARRAY-Z-2962 (MAKE-ARRAY 4 ELEMENT-TYPE DOUBLE-FLOAT))
  (ARRAY-W-2963 (MAKE-ARRAY 4 ELEMENT-TYPE FIXNUM)))
 =======d:
 ((TYPE (SIMPLE-ARRAY FIXNUM (*)) ARRAY-X-2960)
  (TYPE (SIMPLE-ARRAY CHARACTER (*)) ARRAY-Y-2961)
  (TYPE (SIMPLE-ARRAY DOUBLE-FLOAT (*)) ARRAY-Z-2962)
  (TYPE (SIMPLE-ARRAY FIXNUM (*)) ARRAY-W-2963))"
  (let ((name-list (loop
		      for sym in symbol-list
		      collect (gensym (format nil "ARRAY-~a-" sym)))))
    (multiple-value-bind (bindings declaration)
	(loop
	   for type in type-list
	   for name in name-list
	   collect `(,name (make-array ,non-zeros :element-type ',type)) into bind
	   collect `(type (simple-array ,type (*)) ,name) into declare
	   finally (return (values bind declare)))
      (values name-list
	      bindings
	      declaration))))


(defun do-fast-traversal (symbol-list array-list types-list body)
  "Traverses a structure of the type spartn-fast-traversal,
binding the symbols for indices and values.

Example:
   (do-fast-traversal '(i j k)     ; indices
                      '(v1 v2 v3)  ; arrays
                      '(fixnum double-float double-float) ; types
     '((format t \"i=~a  j=~a  k=~a~%\" i j k))))           ; body

 ==>

 (SYMBOL-MACROLET ((I (AREF (THE (SIMPLE-ARRAY FIXNUM       (*)) V1) #:INDEX-2309))
                   (J (AREF (THE (SIMPLE-ARRAY DOUBLE-FLOAT (*)) V2) #:INDEX-2309))
                   (K (AREF (THE (SIMPLE-ARRAY DOUBLE-FLOAT (*)) V3) #:INDEX-2309)))
   (LOOP FOR #:INDEX-2309 BELOW 4 DO
     (FORMAT T \"i=~a  j=~a  k=~a~%\" I J K)))

The symbol used for the index variable (#:INDEX-2309 in this example) is created
by gensym.

*NOTE* that we used '(( body )) when calling do-fast-traversal, and ,@body inside it."
  (let* ((index (gensym "INDEX-"))
	 (bindings (loop
		    for sym  in symbol-list
		    for dat  in array-list
		    for type in types-list
		    collect `(,sym (aref (the (simple-array ,type (*)) ,dat) ,index)))))
    `(symbol-macrolet ,bindings
      (loop for ,index below (length ,(car array-list)) do
       ,@body))))


(defmacro w/fast-traversals (traversal-list &body body)
  "Builds a structure of the type SPARTN-FAST-TRAVERSAL and calls do-fast-traversal
to actually run through it.

 (spartns:w/fast-traversals ((2d-matrix m1 i j val-1)
                             (2d-matrix m2 x y val-2))
   (set-2d-matrix prod
                  (+ (* i 5) x)
                  (+ (* j 5) y)
                  (* val-1 val-2)))
==>

 (LET ((#:POSITION-830 0)
      (#:ARRAY-VAL-1-831 (MAKE-ARRAY 1000 :ELEMENT-TYPE 'NUMBER))
      (#:ARRAY-VAL-2-832 (MAKE-ARRAY 1000 :ELEMENT-TYPE 'NUMBER))
      (#:ARRAY-I-833 (MAKE-ARRAY 1000 :ELEMENT-TYPE 'FIXNUM))
      (#:ARRAY-J-834 (MAKE-ARRAY 1000 :ELEMENT-TYPE 'FIXNUM))
      (#:ARRAY-X-835 (MAKE-ARRAY 1000 :ELEMENT-TYPE 'FIXNUM))
      (#:ARRAY-Y-836 (MAKE-ARRAY 1000 :ELEMENT-TYPE 'FIXNUM)))
  (DECLARE (TYPE FIXNUM #:POSITION-830)
	   (TYPE (SIMPLE-ARRAY NUMBER (*)) #:ARRAY-VAL-1-831)
	   (TYPE (SIMPLE-ARRAY NUMBER (*)) #:ARRAY-VAL-2-832)
	   (TYPE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-I-833)
	   (TYPE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-J-834)
	   (TYPE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-X-835)
	   (TYPE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-Y-836))
  ;; w/spartn-traversals will go make the traversal we'd like to do
  ;; repeatedly; we'll store the indices AND values:
  (SPARTNS:W/SPARTN-TRAVERSALS
      ((2D-MATRIX M1 I J VAL-1) (2D-MATRIX M2 X Y VAL-2))
    (SETF (AREF (THE (SIMPLE-ARRAY NUMBER (*)) #:ARRAY-VAL-1-831) #:POSITION-830) VAL-1)
    (SETF (AREF (THE (SIMPLE-ARRAY NUMBER (*)) #:ARRAY-VAL-2-832) #:POSITION-830) VAL-2)
    (SETF (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-I-833) #:POSITION-830) I)
    (SETF (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-J-834) #:POSITION-830) J)
    (SETF (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-X-835) #:POSITION-830) X)
    (SETF (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-Y-836) #:POSITION-830) Y)
    (INCF #:POSITION-830)
    ;; (car array-names): all ararys have the same size. If the first one is
    ;; full, increase the size of all of them:
    (WHEN (>= #:POSITION-830 (THE FIXNUM (LENGTH #:ARRAY-VAL-1-831)))
      (SETF #:ARRAY-VAL-1-831
	    (JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-VAL-1-831
					  (THE FIXNUM (+ 1000 #:POSITION-830))
					  :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-VAL-1-831)))
      (SETF #:ARRAY-VAL-2-832
	    (JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-VAL-2-832
					  (THE FIXNUM (+ 1000 #:POSITION-830))
					  :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-VAL-2-832)))
      (SETF #:ARRAY-I-833
	    (JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-I-833
					  (THE FIXNUM (+ 1000 #:POSITION-830))
					  :ELEMENT-TYPE
					  (ARRAY-ELEMENT-TYPE #:ARRAY-I-833)))
      (SETF #:ARRAY-J-834
	    (JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-J-834
					  (THE FIXNUM (+ 1000 #:POSITION-830))
					  :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-J-834)))
      (SETF #:ARRAY-X-835
	    (JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-X-835
					  (THE FIXNUM (+ 1000 #:POSITION-830))
					  :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-X-835)))
      (SETF #:ARRAY-Y-836
	    (JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-Y-836
					  (THE FIXNUM (+ 1000 #:POSITION-830))
					  :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-Y-836)))))
  ;; position now holds the size of the final traversal, so we adjust all arrays to that
  ;; size:
  (SETQ #:ARRAY-VAL-1-831
	(JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-VAL-1-831 #:POSITION-830
				      :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-VAL-1-831)))
  (SETQ #:ARRAY-VAL-2-832
	(JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-VAL-2-832 #:POSITION-830
				      :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-VAL-2-832)))
  (SETQ #:ARRAY-I-833
	(JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-I-833 #:POSITION-830
				      :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-I-833)))
  (SETQ #:ARRAY-J-834
	(JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-J-834 #:POSITION-830
				      :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-J-834)))
  (SETQ #:ARRAY-X-835
	(JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-X-835 #:POSITION-830
				      :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-X-835)))
  (SETQ #:ARRAY-Y-836
	(JP-UTILS:ROBUST-ADJUST-ARRAY #:ARRAY-Y-836 #:POSITION-830
				      :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE #:ARRAY-Y-836)))
  (SYMBOL-MACROLET ((VAL-1 (AREF (THE (SIMPLE-ARRAY NUMBER (*)) #:ARRAY-VAL-1-831) #:INDEX-837))
                    (VAL-2 (AREF (THE (SIMPLE-ARRAY NUMBER (*)) #:ARRAY-VAL-2-832) #:INDEX-837))
                    (I (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-I-833)     #:INDEX-837))
                    (J (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-J-834)     #:INDEX-837))
                    (X (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-X-835)     #:INDEX-837))
                    (Y (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) #:ARRAY-Y-836)     #:INDEX-837)))
    (LOOP FOR #:INDEX-837 BELOW (LENGTH #:ARRAY-VAL-1-831) DO
	 (SET-2D-MATRIX PROD (+ (* I 5) X) (+ (* J 5) Y) (* VAL-1 VAL-2)))))

*NOTE*: When you do a fast-traversal, you are NOT operating on the original structures
anymore, but on a copy (see the code above).
"
  ;; make list of relevant indices
  (multiple-value-bind (type-list data-list value-symbol-list indices-list dont-binds)
      (split-traversal-descriptions traversal-list)
    (declare (ignore data-list dont-binds))
    
    (let* ((index-symbol-list (remove-duplicates (flatten indices-list) :from-end t))
	   (symbol-list       (append value-symbol-list index-symbol-list))
	   (types-list        (make-types-list type-list index-symbol-list)))
		   
      ;; traverse, collecting non-zeros:
      (jp-utils:with-gensyms (position); one-array)
	(multiple-value-bind (array-names bindings declarations)
	    (create-and-bind-arrays 1000 symbol-list types-list)
	  (progn ;(format t "array-names: ~a~%symb: ~a~%" array-names symbol-list)
		 `(let ((,position 0)
			,@bindings) ; (ARRAY-var-NNN (make-array 1000 :ELEMENT_TYPE type))
		    (declare (type fixnum ,position)
			     ,@declarations) ; (TYPE SIMPLE_ARRAY (*) type ARRAY-var-NNN)
		    
		    ;; w/spartn-traversals will go make the traversal we'd like to do
		    ;; repeatedly; we'll store the indices AND values:
		    (w/spartn-traversals ,traversal-list
		      ; (SETF (AREF (THE (SIMPLE-ARRAY FIXNUM (*)) ARRAY-NNN) position) i) :
		      ,@(collect-non-zeros-into-fast-traversal symbol-list
							       array-names
							       types-list
							       position)
		      (incf ,position)
		      ;; (car array-names): all ararys have the same size. If the first one is
		      ;; full, increase the size of all of them:
		      (when (>= ,position (the fixnum (length ,(car array-names))))
			,@(loop for one-array in array-names collect
			       `(setf ,one-array
				      (robust-adjust-array ,one-array (the fixnum (+ 1000 ,position))
							   :element-type (array-element-type ,one-array)))))) ;; "STEP" instead of hardcoded 1000?
		    ;; position now holds the size of the final traversal, so we adjust all arrays to that
		    ;; size:
		    ,@(loop for one-array in array-names
			 ;; See CLtL about adjust-array:
			 collect `(setq ,one-array 
					(robust-adjust-array ,one-array ,position
							     :element-type (array-element-type ,one-array))))
		    ;; Actually do the traversal:
		    ,(do-fast-traversal symbol-list array-names types-list body))))))))



