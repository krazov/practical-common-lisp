(defparameter *last-id* 0)
(defvar *todos* ())

(defun new-id ()
    (setq *last-id* (inc *last-id*)))

(defun new-todo (task)
    (list 
        :id (new-id) 
        :task task 
        :done nil))

(defun add-todo (todo)
    (push todo *todos*))

;;; utils
(defun inc (number) (+ number 1))