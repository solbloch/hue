(asdf:defsystem "hue"
  :version "0.1.0"
  :author "Solomon Bloch"
  :license ""
  :depends-on (:jonathan
               :dexador
               :alexandria
               :uiop)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "")
