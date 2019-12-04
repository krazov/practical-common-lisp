;;; commands

(defparameter *exit* "exit")
(defparameter *add* "add")
(defparameter *show* "show")
(defparameter *current* "current")
(defparameter *archived* "archived")

;;; todos list

(defvar *todos* ())

(defvar *new-id*
    (let ((count 0))
        #'(lambda () (setq count (1+ count)))))

(defun find-by-id (id)
    #'(lambda (todo)
        (equal (getf todo :id) id)))

(defun select-by-status (done?)
    (remove-if-not
        #'(lambda (todo)
            (equal (getf todo :done) done?))
        *todos*))

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
                (when (funcall (find-by-id id) todo)
                    (setf (getf todo :done) status))
                todo)
            *todos*)))

(defun formatted-todo (todo)
    (format t "~a. ~a [~:[ ~;x~]]"
        (getf todo :id)
        (getf todo :task)
        (getf todo :done)))

;;; input

(defun prompt-read (prompt)
    (format *query-io* "~a" prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun prompt-for-todo ()
    (new-todo (prompt-read "Name your task: ")))

(defun prompt-for-command ()
    (prompt-read "/> "))

;;; utils

(defun space? (str)
    (string= str " "))

(defun not-space? (str)
    (not (space? str)))

(defun list-of-words (str)
    "You pass a string and get a list of strings."
    (do ((words ())
         (start 0)
         (index 0 (1+ index))
         (previous " " (char str index)))
        ((= index (length str))
            (when (not-space? previous)
                (push (subseq str start) words))
            (reverse words))
        (let ((current (char str index)))
            (when (and (space? previous) (not-space? current))
                (setq start index))
            (when (and (not-space? previous) (space? current))
                (push (subseq str start index) words)))))

;;; flow

(defun add? (operation)
    (equal operation *add*))

(defun show? (operation)
    (equal operation *show*))

(defun current? (type)
    (equal type *current*))

(defun archived? (type)
    (equal type *archived*))

(defun exit? (operation)
    (equal operation *exit*))

(defun dispatch-show (arguments)
    (let ((todos (reverse (select-by-status (if arguments (archived? (first arguments)))))))
        (when todos
            (format t "Tasks:~%")
            (format t "~{~{~a ~a ~}~%~}" todos))
        (unless todos (format t "No tasks matching criteria.~%"))))

(defun dispatch (commands)
    (let ((operation (first commands))
          (arguments (cdr commands)))
        (cond
            ((add? operation)
                (add-todo (prompt-for-todo)))
            ((show? operation)
                (let ((todos (reverse (select-by-status (if arguments (archived? (first arguments)))))))
                    (when todos
                        (format t "Tasks:~%")
                        (format t "~{~{~a ~a ~}~%~}" todos))
                    (unless todos (format t "No tasks matching criteria.~%"))))
            ; -- edit
            ; -- mark as done/undone
            ; -- help
            ((exit? operation)
                (format t "Goodbye.~%"))
            (t
                (format t "Unknown command: ~{~a ~}~%" commands)))
        operation))

;;; fire!

(defun main-for-todos ()
    (loop (add-todo (prompt-for-todo))
        (if (not (y-or-n-p "Do you want to add another task? [y/n]: ")) (return))))

(defun main ()
    (format t "Type a command ('help' for the manual, 'exit' to leave).~%")
    (do () ((exit? (dispatch (list-of-words (prompt-for-command)))))))
