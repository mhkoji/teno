(defpackage teno.markdown
  (:use :cl)
  (:export :to-html))
(in-package :teno.markdown)

(defun to-html (string)
  (with-output-to-string (s)
    (cl-markdown:markdown string :stream s)))
