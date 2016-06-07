;; This software is Copyright (c) Jeronimo Pellegrini, 2008.
;; You have the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-user)

(defpackage #:jp-utils-test
  (:use #:common-lisp
	#:jp-utils
	#:lisp-unit))

(defpackage #:spartns-test
  (:use #:common-lisp
	#:lisp-unit
	#:jp-utils
	#:jp-utils-test
	#:spartns))

