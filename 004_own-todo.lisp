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
    (push todo *todos*)
    (getf todo :task))

(defun set-done (id status)
    (setf *todos*
        (mapcar
            #'(lambda (todo)
                (when (funcall (find-by-id id) todo)
                    (setf (getf todo :done) status))
                todo)
            *todos*)))

(defun formatted-todo (todo &optional (id-length 0) (task-length 0))
    (let*
        ((first-tab (+ 3 id-length))
         (template (concatenate 'string "#~a~" (write-to-string first-tab) "t~a~" (write-to-string (+ 1 first-tab task-length)) "t[~:[ ~;x~]]~%")))
        (format t template
            (getf todo :id)
            (getf todo :task)
            (getf todo :done))))

;;; input

(defun prompt-read (prompt)
    (format *query-io* "~a" prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun prompt-for-todo ()
    (new-todo (prompt-read "Add a task: ")))

(defun prompt-for-command ()
    (format t "~%")
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

(defun longest-taskname (todos)
    (reduce
        #'(lambda (possible-answer current)
            (max possible-answer (length (getf current :task))))
        todos
        :initial-value 0))

(defun dispatch-show (arguments)
    (let* ((todos (reverse (select-by-status (if arguments (archived? (first arguments))))))
           ; TODO: move other local variables to a function
           (latest-todo (first (reverse todos)))
           (latest-id (getf latest-todo :id))
           (id-length (length (write-to-string latest-id)))
           (task-length (longest-taskname todos)))
        (if todos
            (progn
                (format t "Tasks:~%---~%")
                (dolist (todo todos)
                    (formatted-todo todo id-length task-length)))
            (format t "No tasks matching criteria.~%"))))

(defun dispatch (commands)
    (let ((operation (first commands))
          (arguments (cdr commands)))
        (format t "~%")
        (cond
            ((add? operation)
                (format t "Task added: \"~a\"~%" (add-todo (prompt-for-todo))))
            ((show? operation)
                (dispatch-show arguments))
            ; -- edit
            ; -- mark as done/undone
            ; -- help
            ((exit? operation)
                (format t "Goodbye.~%"))
            (t
                (format t "Unknown command: ~{~a ~}~%" commands)))
        operation))

;;; fire!

(defun main ()
    (format t "Type a command ('help' for the manual, 'exit' to leave).~%")
    (do () ((exit? (dispatch (list-of-words (prompt-for-command)))))))
