(defpackage hue
  (:import-from :jonathan :parse :to-json)
  (:use :cl))
(in-package :hue)

(defclass bridge ()
  ((ip
    :accessor ip
    :initarg :ip)
   (username
    :accessor username
    :initarg :username)
   (token-path
    :accessor token-path
    :initform (merge-pathnames ".cache/hue-auth" (user-homedir-pathname))
    :initarg :token-path)
   (device-type
    :accessor device-type
    :initarg :device-type)))

(defun bridge-list ()
  (parse (dex:get "https://discovery.meethue.com") :as :alist))


(defmethod write-token ((bridge bridge))
  (let ((json (to-json `(:ip ,(ip bridge) :username ,(username bridge) :device-type ,(device-type bridge)))))
    (with-open-file (stream (token-path bridge)
                            :direction :output
                            :if-exists :supersede)
      (write-line json stream))))

(defun initialize-bridge (ip &key (device-type "hue"))
  "Press bridge button, then this connects to bridge, prints hue-auth file, and returns bridge."
  (let ((response
          (car (parse (dex:post (concatenate 'string "http://" ip "/api")
                                :content (format nil "{\"devicetype\":\"~a\"}" device-type))
                      :as :hash-table))))
    (if (gethash "error" response)
        "BUTTON NOT CLICKED."
        (let (((make-instance 'bridge
                              :ip ip
                              :username (gethash "username" (gethash "success" response))
                              :device-type device-type)))))))

(defun load-bridge (&key (token-path (merge-pathnames ".cache/hue-auth" (user-homedir-pathname))))
  (let ((token-hash (parse (uiop:read-file-string token-path)
                           :as :hash-table)))
    (make-instance 'bridge
                   :ip (gethash "IP" token-hash)
                   :username (gethash "USERNAME" token-hash)
                   :device-type (gethash "DEVICE-TYPE" token-hash))))

(defmethod api-request ((bridge bridge) api &key (method 'get) content)
  (parse (dex:request (format nil "http://~a/api/~a/~a" (ip bridge) (username bridge) api)
                      :method method :content content) :as :alist))
