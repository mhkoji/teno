(defpackage :teno.server.page
  (:use :cl)
  (:export :page
           :note
           :not-found))
(in-package :teno.server.page)

(defun page (css-path js-path)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "teno")
     (:link :rel "stylesheet"
            :href (cl-who:str css-path))
    (:body
     (:div :id "app")
     (:div :id "app-modal")
     (:script :type "text/javascript"
              :src (cl-who:str js-path))))))

(defun note (css-path js-path note)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "teno")
     (:link :rel "stylesheet"
            :href (cl-who:str css-path))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str
        (format nil "window['$teno'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("note" note)))))))
     (:div :id "app")
     (:div :id "app-modal")
     (:script :type "text/javascript"
              :src (cl-who:str js-path))))))

(defun not-found ()
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita"))
    (:body
     "Not found")))
