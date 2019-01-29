;;;; package.lisp

(defpackage #:package-local-nicknames-tests
  (:use #:cl)
  (:import-from
   #+sbcl #:sb-ext
   #+ccl  #:ccl
   #+ecl ext
   #:package-local-nicknames
   #:package-locally-nicknamed-by-list
   #:add-package-local-nickname
   #:remove-package-local-nickname))
