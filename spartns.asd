(asdf:defsystem spartns
  :name "spartns"
  :version "1.4.4"
  :maintainer "Jeronimo C. Pellegrini"
  :author "Jeronimo C. Pellegrini"
  :licence "LLGPL"
  :description "SPARse TeNSor representation library"
  :serial t
  :components ((:file "spartns-packages")
               (:file "utils")
               (:file "spartns"))
  :in-order-to ((test-op (load-op spartns-test)))
  :properties ((#:author-email . "j_p@aleph0.info")
               ((#:albert #:output-dir) . "doc/docbook")
	       ((#:albert #:formats) . ("docbook"))
	       ((#:albert #:docbook #:dtd) . "/usr/share/sgml/docbook/dtd/4.5/docbookx.dtd")))

(asdf:defsystem spartns-test
  :depends-on ("spartns")        
  :serial t  
  :components ((:file "lisp-unit")
               (:file "spartns-test-packages")
  	       (:file "utils-test")
               (:file "tests")))

