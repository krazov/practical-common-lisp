(defvar *todos* ())

(defparameter *new-id*
    (let ((count 0))
        #'(lambda () (setf count (1+ count)))))

(defun new-todo (task)
    (list 
        :id (funcall *new-id*) 
        :task task 
        :done nil))

(defun add-todo (todo)
    (push todo *todos*))