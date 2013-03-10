(in-package :lunula)

(defstruct (global-env (:type list))
  symbol
  emitted-name)

(defparameter *toplevel-interns* nil)

(defun emit-literal-symbol-expression (symbol)
  (concatenate 'string "{ type: \"symbol\", name: \""
               (symbol-name symbol)
               "\" }"))

(defun emit-interns ()
  (let ((packages nil))
    (dolist (intern *toplevel-interns*)
      (let* ((symbol (global-env-symbol intern))
             (package-name (package-name (symbol-package symbol))))
        (pushnew package-name packages)))
    (emit-indented-line "window.Lunula = window.Lunula || {};")
    (emit-indented-line "Lunula.packages = Lunula.packages || {};")
    ;; make sure packages exist
    (dolist (package-name packages)
      (emit-indented-line "Lunula.packages[\""
                          package-name
                          "\"] = Lunula.packages[\""
                          package-name
                          "\"] || {};")
      (emit-indented-line "Lunula.packages[\""
                          package-name
                          "\"].symbols = Lunula.packages[\""
                          package-name
                          "\"].symbols || {};"))
    ;; intern all of the symbols
    (dolist (intern *toplevel-interns*)
      (let* ((symbol (global-env-symbol intern))
             (symbol-name (symbol-name symbol))
             (package-name (package-name (symbol-package symbol)))
             (var-name (global-env-emitted-name intern)))
        (emit-indented-line "Lunula.packages[\""
                            package-name
                            "\"].symbols[\""
                            symbol-name
                            "\"] = Lunula.packages[\""
                            package-name
                            "\"].symbols[\""
                            symbol-name
                            "\"] || " (emit-literal-symbol-expression symbol) ";")
        (emit-indented-line "var " var-name " = Lunula.packages[\""
                            package-name
                            "\"].symbols[\""
                            symbol-name
                            "\"] = Lunula.packages[\""
                            package-name
                            "\"].symbols[\""
                            symbol-name
                            "\"];")))))

(defun global-lookup (symbol)
  (let ((var-name (find-unique-emitted-name *toplevel-interns* symbol)))
    (pushnew (make-global-env :symbol symbol :emitted-name var-name)
             *toplevel-interns*
             :test 'equal)
    var-name))

(defun global-lookup-value (symbol)
  (unless (boundp symbol)
    (cl:warn "Symbol ~A is not bound" symbol))
  (emit-concat (global-lookup symbol)
          ".value"))

(defun global-lookup-function (symbol)
  (unless (fboundp symbol)
    (cl:warn "Symbol ~A is not bound to a function" symbol))
  (emit-concat (global-lookup symbol)
          ".function"))
