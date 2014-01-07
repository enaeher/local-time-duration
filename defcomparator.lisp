(in-package :ltd)

;;; FIXME -- this is just copied from local-time and modified to handle different types

(defmacro %defcomparator (name (type) &body body)
  (let ((pair-comparator-name (intern (concatenate 'string "%" (string name)))))
    `(progn
      (declaim (inline ,pair-comparator-name))
      (defun ,pair-comparator-name (a b)
        (assert (typep a ,type)
                nil
                'type-error
                :datum a
                :expected-type ,type)
        (assert (typep b ,type)
                nil
                'type-error
                :datum b
                :expected-type ,type)
        ,@body)
      (defun ,name (&rest items)
        (declare (dynamic-extent items))
        (loop for head on items
              while (cdr head)
              always (,pair-comparator-name (first head) (second head))))
      (define-compiler-macro ,name (&rest items)
        (let ((vars (loop
                      :for i :upfrom 0 :below (length items)
                      :collect (gensym (concatenate 'string "TIME-" (princ-to-string i) "-")))))
          `(let (,@(loop
                     :for var :in vars
                     :for item :in items
                     :collect (list var item)))
            ;; we could evaluate comparisons of timestamp literals here
            (and ,@(loop
                     :for (a b) :on vars
                     :while b
                     :collect `(,',pair-comparator-name ,a ,b)))))))))
