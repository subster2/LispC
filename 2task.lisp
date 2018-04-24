;№1. Определите FUNCALL через функционал APPLY. 
(defun fucall (func &rest args) (apply func args))
;(fc-call #'+ 2 2 2 3 4)
;13
(fc-call #'list `(a b) `(1 2 ))
;((A B) (1 2))
