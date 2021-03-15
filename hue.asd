(asdf:defsystem "hue"
  :version "0.1.0"
  :author "Solomon Bloch"
  :license ""
  :depends-on (:jonathan
               :dexador
               :cl-ppcre
               :uiop)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "")
