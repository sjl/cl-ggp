(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "docparser"))

(declaim (optimize (debug 3))) ; dammit sbcl

(defparameter *index*
  (docparser:parse :cl-ggp))

(defparameter *document-packages*
  (list "GGP"))

(defparameter *header*
  "The following is a list of all user-facing parts of `cl-ggp`.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

")


;;;; From the CL Cookbook
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))


;;;; Documentation Utils
(defun get-doc (package-name symbol-name)
  (let ((results (docparser:query *index*
                                  :package-name package-name
                                  :symbol-name symbol-name)))
    (when (> (length results) 0)
      (elt results 0))))

(defun get-package-doc (package-name)
  ;; good god, lemon
  (docparser::find-package-index *index* package-name))


;;;; Markdown Rendering
(defun render-package-header (package-name)
  (format t "## Package ~A~%~%"
          (replace-all package-name "*" "\\*")))

(defun render-package-docstring (package-name)
  (let ((package-docstring
         (docparser::package-index-docstring (get-package-doc package-name))))
    (when package-docstring
      (format t "~A~%~%" package-docstring))))

(defun render-symbol-header (symbol-name extra)
  (format t "### ~A~A~%~%"
          (replace-all symbol-name "*" "\\*")
          extra))

(defun render-docstring (node)
  (let ((documentation (docparser:node-docstring node)))
    (when documentation
      (format t "~A~%~%" documentation))))

(defun render-lambda-list (node)
  (format t "    ~A~%~%"
          (cons (docparser:node-name node)
                (docparser:operator-lambda-list node))))

(defgeneric render-documentation (node symbol-name))

(defun render-class-slot (node)
  (let ((name (docparser:node-name node))
        (type (docparser:slot-type node))
        (readers (docparser:slot-readers node))
        (writers (docparser:slot-writers node))
        (accessors (docparser:slot-accessors node)))
    (format t "#### Slot ~A~%~%" name )
    (format t "* Allocation: ~A~%" (docparser:slot-allocation node))
    (when type (format t "* Type: `~A`~%" type))
    (when readers (format t "* Reader~p: ~{`~A`~^, ~}~%" (length readers) readers))
    (when writers (format t "* Writer~p: ~{`~A`~^, ~}~%" (length writers) writers))
    (when accessors (format t "* Accessor~p: ~{`~A`~^, ~}~%" (length accessors) accessors))
    (format t "~%")
    (render-docstring node)))


(defmethod render-documentation ((node docparser:class-node) symbol-name)
  (render-symbol-header symbol-name " (class)")
  (render-docstring node)
  (mapc #'render-class-slot (docparser:record-slots node)))

(defmethod render-documentation ((node docparser:documentation-node) symbol-name)
  (render-symbol-header symbol-name "")
  (format t "`~A`~%~%" (class-of node))
  (render-docstring node))

(defmethod render-documentation ((node docparser:variable-node) symbol-name)
  (render-symbol-header symbol-name " (variable)")
  (render-docstring node))

(defmethod render-documentation ((node docparser:function-node) symbol-name)
  (render-symbol-header symbol-name " (function)")
  (render-lambda-list node)
  (render-docstring node))

(defmethod render-documentation ((node docparser:generic-function-node) symbol-name)
  (render-symbol-header symbol-name " (generic function)")
  (render-lambda-list node)
  (render-docstring node))

(defmethod render-documentation ((node docparser:macro-node) symbol-name)
  (render-symbol-header symbol-name " (macro)")
  (render-lambda-list node)
  (render-docstring node))


;;;; Documentation Sections
(defun document-symbol (package-name symbol)
  (let* ((symbol-name (symbol-name symbol))
         (doc-node (get-doc package-name symbol-name)))
    (when doc-node (render-documentation doc-node symbol-name))))

(defun document-package (package-name)
  (render-package-header package-name)
  (render-package-docstring package-name)
  (let ((symbols (loop :for s :being :the external-symbol :of package-name
                       :collect s)))
    (mapc #'(lambda (symbol)
             (document-symbol package-name symbol))
          (sort symbols #'string-lessp :key #'symbol-name))))

(defun document-header ()
  (format t "# API Reference~%~%")
  (format t *header*)
  (format t "[TOC]~%~%"))


;;;; Main
(defun main ()
  (with-open-file (*standard-output* #p"docs/03-reference.markdown"
                                     :direction :output
                                     :if-exists :supersede)
    (document-header)
    (mapc #'document-package *document-packages*)))


(main)

