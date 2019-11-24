;;; todos list

(defvar *todos* ())

(defvar *new-id*
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

(defun prompt-for-command ()
    (prompt-read "/> "))

;;; utils

(defun list-of-words (str)
    (do ((index 0 (1+ index))
         (words nil)
         (previous " " (char str index)))
        ((= index (length str)) (reverse words))
        (let ((current (string (char str index))))
            (unless (string= current " ")
                (if (and (string= previous " ") (not (string= current " ")))
                    (push current words)
                    (push (concatenate 'string (pop words) current) words))))))

;;; flow

(defun exit? (command)
    (equal command "exit"))

;;; fire!

(defun main-for-todos ()
    (loop (add-todo (prompt-for-todo))
        (if (not (y-or-n-p "Do you want to add another task? [y/n]: ")) (return))))

(defun main ()
    (format t "Type a command ('exit' to leave).~%")
    (do ((command nil) (is-exit nil))
        (is-exit)
        (setq command (prompt-for-command))
        (if (setq is-exit (exit? command))
            (format t "Goodbye.~%")
            (format t "-> ~a~%" command))))
