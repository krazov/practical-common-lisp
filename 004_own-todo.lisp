(defvar *todos* ())

(defparameter *new-id*
    (let ((count 0))
        #'(lambda () (setq count (1+ count)))))

(defun find-by-id (id)
    #'(lambda (todo)
        (equal (getf todo :id) id)))

(defun new-todo (task)
    (list 
        :id (funcall *new-id*) 
        :task task 
        :done nil))

(defun add-todo (todo)
    (push todo *todos*))

(defun set-done (id status)
    (setf *todos*
        (mapcar
            #'(lambda (todo)
                (when (funcall (find-by-id id) todo) (setf (getf todo :done) status))
                todo)
            *todos*)))

;;; I/O

(defun prompt-read (prompt)
    (format *query-io* "~a" prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun prompt-for-todo ()
    (new-todo (prompt-read "Name your task: ")))

;;; fire!

(defun main ()
    (loop (add-todo (prompt-for-todo))
        (if (not (y-or-n-p "Do you want to add another task? [y/n]: ")) (return))))