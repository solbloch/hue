(asdf:defsystem "hue"
  :version "0.1.0"
  :author "Solomon Bloch"
  :license ""
  :depends-on (:jonathan
               :dexador
               :alexandria
<<<<<<< HEAD
               :bordeaux-threads
=======
               :clx
>>>>>>> 45eca24 (Adds three new files, colors, xlib, and screen)
               :uiop)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "colors")
                 (:file "xlib"))))
  :description "")
