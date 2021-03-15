(defpackage cl-hue2/tests/main
  (:use :cl
        :cl-hue2
        :rove))
(in-package :cl-hue2/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-hue2)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
