;;;; package-local-nicknames-tests.lisp

(in-package #:package-local-nicknames-tests)

;;; Test runner

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *tests* '()))

(defmacro with-test ((&key name) &body body)
  (let ((name (intern (concatenate 'string "TEST-" (symbol-name name)))))
    `(progn
       (defun ,name () ,@body)
       (pushnew ',name *tests*)
       ',name)))

(defun run ()
  (let ((errors '()))
    (dolist (test *tests*)
      (format t "~S~%" test)
      (handler-case (funcall test)
        (error (e)
          (format t "Test failure in ~S: ~A~%" test e)
          (push e errors))))
    (nreverse errors)))

;;; Test code

(with-test (:name :package-local-nicknames)
  ;; Clear slate
  (#+sbcl sb-ext:without-package-locks
   #-sbcl progn
   (when (find-package :package-local-nicknames-test-1)
     (delete-package :package-local-nicknames-test-1))
   (when (find-package :package-local-nicknames-test-2)
     (delete-package :package-local-nicknames-test-2)))
  (eval `(defpackage :package-local-nicknames-test-1
           (:local-nicknames (:l :cl) (:sb :sb-ext))))
  (eval `(defpackage :package-local-nicknames-test-2
           (:export "CONS")))
  ;; Introspection
  (let ((alist (package-local-nicknames :package-local-nicknames-test-1)))
    (assert (equal (cons "L" (find-package "CL")) (assoc "L" alist :test 'string=)))
    (assert (equal (cons "SB" (find-package "SB-EXT")) (assoc "SB" alist :test 'string=)))
    (assert (eql 2 (length alist))))
  ;; Usage
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (let ((cons0 (read-from-string "L:CONS"))
          (exit0 (read-from-string "SB:EXIT"))
          (cons1 (find-symbol "CONS" :l))
          (exit1 (find-symbol "EXIT" :sb))
          (cl (find-package :l))
          (sb (find-package :sb)))
      (assert (eq 'cons cons0))
      (assert (eq 'cons cons1))
      (assert (equal "L:CONS" (prin1-to-string cons0)))
      (assert (eq 'sb-ext:exit exit0))
      (assert (eq 'sb-ext:exit exit1))
      (assert (equal "SB:EXIT" (prin1-to-string exit0)))
      (assert (eq cl (find-package :common-lisp)))
      (assert (eq sb (find-package :sb-ext)))))
  ;; Can't add same name twice for different global names.
  (assert (eq :oopsie
              (handler-case
                  (add-package-local-nickname :l :package-local-nicknames-test-2
                                              :package-local-nicknames-test-1)
                (error ()
                  :oopsie))))
  ;; But same name twice is OK.
  (add-package-local-nickname :l :cl :package-local-nicknames-test-1)
  ;; Removal.
  (assert (remove-package-local-nickname :l :package-local-nicknames-test-1))
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (let ((exit0 (read-from-string "SB:EXIT"))
          (exit1 (find-symbol "EXIT" :sb))
          (sb (find-package :sb)))
      (assert (eq 'sb-ext:exit exit0))
      (assert (eq 'sb-ext:exit exit1))
      (assert (equal "SB:EXIT" (prin1-to-string exit0)))
      (assert (eq sb (find-package :sb-ext)))
      (assert (not (find-package :l)))))
  ;; Adding back as another package.
  (assert (eq (find-package :package-local-nicknames-test-1)
              (add-package-local-nickname :l :package-local-nicknames-test-2
                                          :package-local-nicknames-test-1)))
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (let ((cons0 (read-from-string "L:CONS"))
          (exit0 (read-from-string "SB:EXIT"))
          (cons1 (find-symbol "CONS" :l))
          (exit1 (find-symbol "EXIT" :sb))
          (cl (find-package :l))
          (sb (find-package :sb)))
      (assert (eq cons0 cons1))
      (assert (not (eq 'cons cons0)))
      (assert (eq (find-symbol "CONS" :package-local-nicknames-test-2)
                  cons0))
      (assert (equal "L:CONS" (prin1-to-string cons0)))
      (assert (eq 'sb-ext:exit exit0))
      (assert (eq 'sb-ext:exit exit1))
      (assert (equal "SB:EXIT" (prin1-to-string exit0)))
      (assert (eq cl (find-package :package-local-nicknames-test-2)))
      (assert (eq sb (find-package :sb-ext)))))
  ;; Interaction with package locks.
  #+sbcl
  (progn
    (sb-ext:lock-package :package-local-nicknames-test-1)
    (assert (eq :package-oopsie
                (handler-case
                    (add-package-local-nickname :c :sb-c :package-local-nicknames-test-1)
                  (sb-ext:package-lock-violation ()
                    :package-oopsie))))
    (assert (eq :package-oopsie
                (handler-case
                    (remove-package-local-nickname :l :package-local-nicknames-test-1)
                  (sb-ext:package-lock-violation ()
                    :package-oopsie))))
    (sb-ext:unlock-package :package-local-nicknames-test-1)
    (add-package-local-nickname :c :sb-c :package-local-nicknames-test-1)
    (remove-package-local-nickname :l :package-local-nicknames-test-1)))

(defmacro with-tmp-packages (bindings &body body)
  `(let ,(mapcar #'car bindings)
     (unwind-protect
          (progn
            (setf ,@(apply #'append bindings))
            ,@body)
       ,@(mapcar (lambda (p)
                   `(when ,p (delete-package ,p)))
                 (mapcar #'car bindings)))))

(with-test (:name :delete-package-locally-nicknames-others)
  (with-tmp-packages ((p1 (make-package "LOCALLY-NICKNAMES-OTHERS"))
                      (p2 (make-package "LOCALLY-NICKNAMED-BY-OTHERS")))
    (add-package-local-nickname :foo p2 p1)
    (assert (equal (list p1) (package-locally-nicknamed-by-list p2)))
    (delete-package p1)
    (assert (not (package-locally-nicknamed-by-list p2)))))

(with-test (:name :delete-package-locally-nicknamed-by-others)
  (with-tmp-packages ((p1 (make-package "LOCALLY-NICKNAMES-OTHERS"))
                      (p2 (make-package "LOCALLY-NICKNAMED-BY-OTHERS")))
    (add-package-local-nickname :foo p2 p1)
    (assert (package-local-nicknames p1))
    (delete-package p2)
    (assert (not (package-local-nicknames p1)))))

(with-test (:name :own-name-as-local-nickname)
  (with-tmp-packages ((p1 (make-package "OWN-NAME-AS-NICKNAME1"))
                      (p2 (make-package "OWN-NAME-AS-NICKNAME2")))
    (assert (eq :oops
                (handler-case
                    (add-package-local-nickname :own-name-as-nickname1 p2 p1)
                  (error ()
                    :oops))))
    (handler-bind ((error #'continue))
      (add-package-local-nickname :own-name-as-nickname1 p2 p1))
    (assert (eq (intern "FOO" p2)
                (let ((*package* p1))
                  (intern "FOO" :own-name-as-nickname1))))))

(with-test (:name :own-nickname-as-local-nickname)
  (with-tmp-packages ((p1 (make-package "OWN-NICKNAME-AS-NICKNAME1"
                                        :nicknames '("OWN-NICKNAME")))
                      (p2 (make-package "OWN-NICKNAME-AS-NICKNAME2")))
    (assert (eq :oops
                (handler-case
                    (add-package-local-nickname :own-nickname p2 p1)
                  (error ()
                    :oops))))
    (handler-bind ((error #'continue))
      (add-package-local-nickname :own-nickname p2 p1))
    (assert (eq (intern "FOO" p2)
                (let ((*package* p1))
                  (intern "FOO" :own-nickname))))))
