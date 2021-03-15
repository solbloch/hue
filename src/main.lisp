(defpackage hue
  (:import-from :jonathan :parse)
  (:use :cl))
(in-package :hue)

(defclass bridge ()
  ((address
    :accessor address
    :initarg :address)
   (username
    :accessor username
    :initarg :username)
   (token-path
    :accessor token-path
    :initform (merge-pathnames ".cache/bridge-auth" (user-homedir-pathname))
    :initarg :token-path)))

(defun bridge-list ()
  (parse (dex:get "https://discovery.meethue.com") :as :alist))


(defun get-key-bridge (ip)
  "Press bridge button, then this connects to bridge and return bridge object."
  (let ((response
          (car (parse (dex:post (concatenate 'string "http://" ip "/api") :content
                                (jonathan:to-json '(:devicetype  "hue-lisp")))
                      :as :hash-table))))
    (if (gethash "error" response)
        "BUTTON NOT CLICKED."
        (make-instance 'bridge
                       :address ip
                       :username (gethash "username" (gethash "success" response))))))


(defun slurp (path)
  (with-open-file (st path :direction :input
                           :if-does-not-exist nil)
    (when (streamp st)
      (loop for line = (read-line st nil)
            while line collect line))))
