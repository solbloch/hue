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
    :initarg :device-type)
   (groups
    :accessor groups
    :initarg :groups)
   (lights
    :accessor lights
    :initarg :lights)))

(defclass group ()
  ((number
    :accessor number
    :initarg :number)
   (name
    :accessor name
    :initarg :name)
   (action
    :accessor action
    :initarg :action)
   (class
    :accessor class
    :initarg :class)
   (state
    :accessor state
    :initarg :state)
   (type
    :accessor type
    :initarg :type)
   (lights
    :accessor lights
    :initarg :lights)))

(defclass light ()
  ((number
    :accessor number
    :initarg :number)
   (state
    :accessor state
    :initarg :state)
   (name
    :accessor name
    :initarg :name)))


(defun bridge-list ()
  (parse (dex:get "https://discovery.meethue.com") :as :alist))

(defun flatten (lst)
  (let (flat)
    (labels ((helper (l)
             (cond ((null l) nil)
                   ((consp l) (progn
                                (helper (car l))
                                (helper (cdr l))))
                   ((atom l) (push l flat)))))
      (helper lst)
      (nreverse flat))))

(defmethod jonathan:%to-json ((_ (eql nil)))
  (jonathan:%write-string "false"))

(defmethod write-token ((bridge bridge))
  (let ((json (to-json `(:ip ,(ip bridge)
                         :username ,(username bridge)
                         :device-type ,(device-type bridge)))))
    (with-open-file (stream (token-path bridge)
                            :direction :output
                            :if-exists :supersede)
      (write-line json stream))))

(defmethod test-connection ((bridge bridge))
  (unless (string= "error" (car (flatten (api-request bridge ""))))
    t))

(defun initialize-bridge (ip &key (device-type "hue"))
  "Press bridge button, then this connects to bridge, prints hue-auth file, and returns bridge."
  (let ((response
          (car (parse (dex:post (concatenate 'string "http://" ip "/api")
                                :content (alist-to-json `(("devicetype". device-type))))
                      :as :hash-table))))
    (if (gethash "error" response)
        "BUTTON NOT CLICKED."
        (let ((bridge (make-instance 'bridge
                                     :ip ip
                                     :username (gethash "username" (gethash "success" response))
                                     :device-type device-type)))
          (write-token bridge)
          bridge))))

(defun load-bridge (&key (token-path (merge-pathnames ".cache/hue-auth" (user-homedir-pathname))))
  (let* ((token-hash (parse (uiop:read-file-string token-path)
                            :as :hash-table))
         (bridge (make-instance 'bridge
                                :ip (gethash "IP" token-hash)
                                :username (gethash "USERNAME" token-hash)
                                :device-type (gethash "DEVICE-TYPE" token-hash))))
    (when (test-connection bridge)
      (setf (groups bridge) (mapcar #'alist-to-group (api-request bridge "groups")))
      (setf (lights bridge) (mapcar #'alist-to-light (api-request bridge "lights")))
      bridge)))

(defmethod api-request ((bridge bridge) api &key (method 'get) content)
  (parse (dex:request (format nil "http://~a/api/~a/~a" (ip bridge) (username bridge) api)
                      :method method :content content) :as :alist))

(defun alist-to-group (group-alist)
  (flet ((cdr-assoc (key)
           (cdr (assoc key (cdr group-alist) :test #'equal))))
    (make-instance 'group
                   :number (car group-alist)
                   :name (cdr-assoc "name")
                   :action (cdr-assoc "action")
                   :class (cdr-assoc "class")
                   :state (cdr-assoc "state")
                   :type (cdr-assoc "type")
                   :lights (cdr-assoc "lights"))))

(defun alist-to-light (light-alist)
  (flet ((cdr-assoc (key)
           (cdr (assoc key (cdr light-alist) :test #'equal))))
    (make-instance 'light
                   :number (car light-alist)
                   :state (cdr-assoc "state")
                   :name (cdr-assoc "name"))))

(defmethod set-state ((bridge bridge) (group group) alist)
  (api-request bridge (format nil "groups/~a/action" (number group))
               :method :put
               :content (to-json alist :from :alist)))

(defmethod set-state ((bridge bridge) (light light) alist)
  (api-request bridge (format nil "lights/~a/state" (number light))
               :method :put
               :content (to-json alist :from :alist)))
