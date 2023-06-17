(defsystem "oascg"
  :pathname "core"
  :version "0.1.0"
  :author ""
  :license ""
  :components ((:module "src" :components ((:file "main"))))
  :in-order-to ((test-op (test-op "oascg/tests")))
  :description ""
  :long-description "")

(defsystem "oascg/tests"
  :pathname "core"
  :depends-on ("oascg" "com.inuoe.jzon" "fiveam")
  :components ((:module "tests" :components ((:file "main"))))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run! (find-symbol* :oascg-core :oascg-core-tests))))

(defsystem "oascg/typescript"
  :pathname "typescript"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("oascg" "com.inuoe.jzon")
  :components ((:module "src" :components ((:file "main"))))
  :in-order-to ((test-op (test-op "oascg/typescript/tests")))
  :description ""
  :long-description "")

(defsystem "oascg/typescript/tests"
  :pathname "typescript"
  :version "0.1.0"
  :depends-on ("oascg" "oascg/typescript" "com.inuoe.jzon" "fiveam")
  :components ((:module "tests" :components ((:file "main"))))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run!
                                      (find-symbol* :oascg-typescript :oascg-typescript-tests))))

(defsystem "oascg/web"
  :pathname "web"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("hunchentoot" "oascg" "oascg/typescript" "com.inuoe.jzon")
  :components ((:module "src" :components ((:file "main"))))
  :description ""
  :long-description "")
