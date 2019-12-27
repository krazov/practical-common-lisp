;;; vocabulary

(defparameter *add* "add")
(defparameter *show* "show")
(defparameter *current* "current")
(defparameter *archived* "archived")
(defparameter *all* "all")
(defparameter *edit* "edit")
(defparameter *mark* "mark")
(defparameter *done* "done")
(defparameter *undone* "undone")
(defparameter *help* "help")
(defparameter *exit* "exit")

;;; todos list

(defvar *todos* ())

(defvar *new-id*
    (let ((count 0))
        #'(lambda () (setq count (1+ count)))))

(defun matches-id? (id todo)
    (equal (getf todo :id) id))

(defun select-by-id (id)
    (remove-if-not
        #'(lambda (todo)
            (equal (getf todo :id) id))
        *todos*))

(defun narrow-by-status (done?)
    (remove-if-not
        #'(lambda (todo)
            (equal (getf todo :done) done?))
        *todos*))

(defun select-by-status (status)
    (cond
        ((string= status *all*)
            *todos*)
        ((string= status *current*)
            (narrow-by-status nil))
        ((string= status *archived*)
            (narrow-by-status t))
        (t
            (format t "[ERROR] Unknown status command: ~a~%" status)
            nil)))

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
                (when (matches-id? id todo)
                    (setf (getf todo :done) status))
                todo)
            *todos*)))

(defun tab-of (count)
    (concatenate 'string "~" (write-to-string count) "t"))

(defun formatted-todo (todo &optional (id-length 0) (task-length 0))
    (let*
        ((first-tab (+ 3 id-length))
         (second-tab (+ 1 first-tab task-length))
         (template
            (concatenate 'string "#~a" (tab-of first-tab) "~a" (tab-of second-tab) "[~:[ ~;x~]]~%")))
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
    (new-todo (prompt-read "[PROMPT] Add a task: ")))

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
                (push (string-downcase (subseq str start)) words))
            (reverse words))
        (let ((current (char str index)))
            (cond
                ((and (space? previous) (not-space? current))
                    (setq start index))
                ((and (not-space? previous) (space? current))
                    (push (string-downcase (subseq str start index)) words))))))

;;; checking vocabulary

(defun add? (operation)
    (equal operation *add*))

(defun show? (operation)
    (equal operation *show*))

(defun current? (type)
    (equal type *current*))

(defun archived? (type)
    (equal type *archived*))

(defun edit? (operation)
    (equal operation *edit*))

(defun mark? (operation)
    (equal operation *mark*))

(defun done? (status)
    (equal status *done*))

(defun undone? (status)
    (equal status *undone*))

(defun help? (operation)
    (equal operation *help*))

(defun exit? (operation)
    (equal operation *exit*))

(defun longest-taskname (todos)
    (reduce
        #'(lambda (possible-answer current)
            (max possible-answer (length (getf current :task))))
        todos
        :initial-value 0))

(defun dispatch-show (arguments)
    (let* ((todos (reverse (select-by-status (if arguments (first arguments) *all*))))
           ; TODO: move other local variables to a function
           (latest-todo (first (reverse todos)))
           (latest-id (getf latest-todo :id))
           (id-length (length (write-to-string latest-id)))
           (task-length (longest-taskname todos)))
        ; TODO: handling `all` case and make it default
        (cond
            ((equal *todos* nil)
                (format t "[INFO] No todos. Type `add` to add some.~%"))
            (todos
                ; TODO: dynamic section title
                (format t "Tasks:~%------~%")
                (dolist (todo todos)
                    (formatted-todo todo id-length task-length)))
            (t
                (format t "[INFO] No tasks matching criteria.~%")))))

(defun dispatch-add ()
    (format t "[INFO] Task added: \"~a\"~%~%" (add-todo (prompt-for-todo)))
    (dispatch-show '(*archived*)))

(defun dispatch-edit (arguments)
    (let ((id (parse-integer (first arguments) :junk-allowed t)))
        ; TODO: implement edit function
        (format t "[UNDER CONSTRUCTION] Soon, this will allow to edit todo with id of ~a.~%" id)))

(defun dispatch-mark (arguments)
    (let ((id (parse-integer (first arguments) :junk-allowed t))
          (status (second arguments)))
        ; TODO: re-loading previous list after update
        (cond
            ((equal id nil)
                (format t "[ERROR] Second argument has to be a valid number.~%"))
            ((equal (select-by-id id) nil)
                (format t "[WARNING] There is no todo with id ~a. No operation pefromed.~%" id))
            ((done? status)
                (set-done id t)
                (format t "[INFO] Todo with id ~a status changed to: done.~%" id))
            ((undone? status)
                (set-done id nil)
                (format t "[INFO] Todo with id ~a status changed to: undone.~%" id))
            (t
                (format t "[ERROR] Status has to be either \"done\", or \"undone\".~%")))))

(defun dispatch-help ()
    (dolist (instruction '("- add - prompts for a new todo.~%"
                           "- show [current|archived] - shows a list of current (default) or archived todos.~%"
                           "- edit <id> - prompts for edit of a chosen todo.~%"
                           "- mark <id> done|undone - marks todo as either done, or undone.~%"
                           "- help - displays this message.~%"
                           "- exit - exits the application.~%"))
        (format t instruction)))

(defun dispatch (commands)
    (let ((operation (first commands)))
        (format t "~%")
        (cond
            ((add? operation)
                (dispatch-add))
            ((show? operation)
                (dispatch-show (cdr commands)))
            ((edit? operation)
                (dispatch-edit (cdr commands)))
            ((mark? operation)
                (dispatch-mark (cdr commands)))
            ((help? operation)
                (dispatch-help))
            ((exit? operation)
                (format t "Goodbye.~%"))
            (t
                (format t "[ERROR] Unknown command:~{ ~a~}\.~%" commands)))
        operation))

;;; fire!

(defun main ()
    (format t "Type a command ('help' for the manual, 'exit' to leave).~%")
    (do () ((exit? (dispatch (list-of-words (prompt-for-command)))))))
