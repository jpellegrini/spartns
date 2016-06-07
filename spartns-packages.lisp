;; This software is Copyright (c) Jeronimo Pellegrini, 2008.
;; You have the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-user)

(defpackage #:jp-utils
  (:use #:common-lisp)
  (:export #:binary-search
	   #:nonrec-binary-search
	   #:get-binary-search
	   #:def-nonrec-binary-search
	   #:get-map
	   #:set-map
	   #:hash-table-keys
	   #:do-sequence
	   #:generate-random-array
	   #:sortedp
	   #:counting-sort
	   #:counting-sort-g
	   #:subst-all
	   #:with-gensyms
	   #:format-symbol
	   #:format-uninterned-symbol
	   #:robust-adjust-array))

(defpackage #:spartns
  (:use #:common-lisp
	#:jp-utils)
  (:export #:defscheme
	   #:defspartn
	   #:spartn-convert
	   #:hash
	   #:cvector
	   #:array
;	   #:avl ; not ready yet
	   #:w/spartns
	   #:w/spartn-traversals
	   #:w/fast-traversals))

