;;; commands

(defparameter *exit* "exit")
(defparameter *new* "new")
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

(defun new? (operation)
    (equal operation *new*))

(defun show? (operation)
    (equal operation *show*))

(defun exit? (operation)
    (equal operation *exit*))

(defun dispatch (commands)
    (let ((operation (first commands)))
        (cond
            ((new? operation)
                (add-todo (prompt-for-todo)))
            ((show? operation)
                (format t "Tasks:~%")
                (format t "~{~{~a ~a ~}~%~}" (reverse *todos*)))
            ((exit? operation)
                (format t "Goodbye.~%"))
            (t
                (format t "-> ~{~a ~}~%" commands)))
        operation))

;;; fire!

(defun main-for-todos ()
    (loop (add-todo (prompt-for-todo))
        (if (not (y-or-n-p "Do you want to add another task? [y/n]: ")) (return))))

(defun main ()
    (format t "Type a command ('exit' to leave).~%")
    (do () ((exit? (dispatch (list-of-words (prompt-for-command)))))))
