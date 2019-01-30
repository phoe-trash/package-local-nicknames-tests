;;;; package.lisp

(defpackage #:package-local-nicknames-tests
  (:use #:cl)
  (:export #:run)
  (:import-from
   #+sbcl #:sb-ext
   #+ccl  #:ccl
   #+ecl  #:ext
   #+abcl #:ext
   #:package-local-nicknames
   #:package-locally-nicknamed-by-list
   #:add-package-local-nickname
   #:remove-package-local-nickname))

(in-package #:package-local-nicknames-tests)

(progn
  (defparameter +test-data+
    #+sbcl '(:sb-ext :sb :exit)
    #+ccl  '(:ccl    :cc :quit)
    #+ecl  '(:ext    :ex :exit)
    #+abcl '(:ext    :ex :quit))

  (defparameter +pkg-name+ (first +test-data+))
  (defparameter +nn-name+ (second +test-data+))
  (defparameter +sym-name+ (third +test-data+))

  (defparameter +pkg-sname+ (string +pkg-name+))
  (defparameter +nn-sname+ (string +nn-name+))
  (defparameter +sym-sname+ (string +sym-name+))
  (defparameter +sym-fullname+ (concatenate 'string +pkg-sname+ ":" +sym-sname+))
  (defparameter +sym-fullnickname+ (concatenate 'string +nn-sname+ ":" +sym-sname+))
  (defparameter +sym+ (or (find-symbol +sym-sname+ +pkg-name+)
                          (error "Symbol not found while loading tests: check +SYM+ binding."))))
