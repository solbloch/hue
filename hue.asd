(asdf:defsystem "hue"
  :version "0.1.0"
  :author "Solomon Bloch"
  :license ""
  :depends-on (:jonathan
               :dexador
               :alexandria
               :bordeaux-threads
               :uiop)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "")
