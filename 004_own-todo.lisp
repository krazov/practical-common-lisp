;;; app state

(defvar *list-type* 'all)
(defvar *todos* ())

;;; vocabulary

(defun parse-operation (potential-operation)
    (find potential-operation '(add show edit mark delete help exit) :test #'string-equal))

(defun parse-status (potential-status)
    (find potential-status '(all current done) :test #'string-equal))

(defun done? (status)
    (equal status 'done))

(defun undone? (status)
    (equal status 'undone))

(defun exit? (operation)
    (equal operation 'exit))

;;; todos operations

(defun new-id ()
    (if *todos* (1+ (getf (first *todos*) :id)) 1))

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
    (case (parse-status status)
        (all *todos*)
        (current (narrow-by-status nil))
        (done (narrow-by-status t))
        (otherwise (format t "[ERROR] Unknown status: ~a. Possibilities: all (default), current, and done.~%" status))))

(defun new-todo (task)
    (list
        :id (new-id)
        :task task
        :done nil))

(defun add-todo (todo)
    (push todo *todos*)
    (getf todo :task))

(defun update-todo (id field val)
    (setf *todos*
        (mapcar
            #'(lambda (todo)
                (when (matches-id? id todo)
                    (setf (getf todo field) val))
                todo)
            *todos*)))

(defun set-done (id status)
    (update-todo id :done status))

(defun update-task (id task)
    (update-todo id :task task))

(defun delete-todo (id)
    (setf *todos*
        (remove-if
            #'(lambda (todo)
                (equal (getf todo :id) id))
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

(defun clear-screen()
    (format t "~A[H~@*~A[J" #\escape))

(defun print-multiple-lines (lines-of-text)
    (dolist (line lines-of-text)
        (format t "~a~%" line)))

(defun longest-taskname (todos)
    (reduce
        #'(lambda (possible-answer current)
            (max possible-answer (length (getf current :task))))
        todos
        :initial-value 0))

(defun title-by-status (status)
    (case (parse-status status)
        (all (format t "All tasks:~%----------~%"))
        (current (format t "Current tasks:~%--------------~%"))
        (done (format t "Finished tasks:~%---------------~%"))
        (otherwise (format t "Tasks:~%------~%"))))

;;; disk operations

(defun save-db (filename)
    (with-open-file (out filename
                     :direction :output
                     :if-exists :supersede)
        (with-standard-io-syntax (print *todos* out))))

(defun load-db (filename)
    (with-open-file (in filename
                     :if-does-not-exist nil)
        (when in
            (with-standard-io-syntax
                (setf *todos* (read in))))))

;;; dispatchers

(defun dispatch-show (&optional arguments)
    (let* ((status (if arguments (first arguments) *list-type*))
           (todos (select-by-status status)))
        (unless (string= *list-type* status)
            (setq *list-type* status))
        (cond
            ((equal *todos* nil)
                (format t "[INFO] No todos. Type 'add' to add some.~%"))
            (todos
                (title-by-status status)
                (let ((id-length (length (write-to-string (getf (first todos) :id))))
                      (task-length (longest-taskname todos)))
                    (dolist (todo (reverse todos))
                        (formatted-todo todo id-length task-length))))
            (t
                (format t "[INFO] No tasks matching criteria.~%")))))

(defun dispatch-add ()
    (format t "[INFO] Task added: \"~a\"~%~%" (add-todo (prompt-for-todo)))
    (dispatch-show))

(defun dispatch-edit (arguments)
    (let* ((id (parse-integer (first arguments) :junk-allowed t))
           (todos (select-by-id id)))
        (cond
            ((equal id nil)
                (format t "[ERROR] The argument has to be a valid number.~%"))
            ((equal todos nil)
                (format t "[WARNING] There is no todo with id ~a.~%[INFO] No operation pefromed.~%" id))
            (t
                (let ((new-description
                          (let ((todo (first todos)))
                              (format t "[INFO] Editing task #~a: ~a~%" (getf todo :id) (getf todo :task))
                              (prompt-read "[PROMPT] New description: "))))
                    (if (string= new-description "")
                        (format t "[INFO] Empty description. No operation performed.~%")
                        (progn
                            (update-task id new-description)
                            (format t "[INFO] Todo #~a changed to: ~a~%" id new-description))))
                (format t "~%")
                (dispatch-show)))))

(defun dispatch-mark (arguments)
    (let ((id (parse-integer (first arguments) :junk-allowed t))
          (status (parse-status (second arguments))))
        (cond
            ((equal id nil)
                (format t "[ERROR] The argument has to be a valid number.~%"))
            ((equal (select-by-id id) nil)
                (format t "[WARNING] There is no todo with id ~a. No operation performed.~%" id))
            ((done? status)
                (set-done id t)
                (format t "[INFO] Todo with id ~a status changed to: done.~%~%" id)
                (dispatch-show))
            ((undone? status)
                (set-done id nil)
                (format t "[INFO] Todo with id ~a status changed to: undone.~%~%" id)
                (dispatch-show))
            (t
                (format t "[ERROR] Status has to be either \"done\", or \"undone\".~%")))))

(defun dispatch-delete (arguments)
    (let ((id (parse-integer (first arguments) :junk-allowed t)))
        (cond
            ((equal id nil)
                (format t "[ERROR] The argument has to be a valid number.~%"))
            ((equal (select-by-id id) nil)
                (format t "[INFO] There is no todo with id ~a. No operation performed.~%" id))
            (t
                (delete-todo id)
                (format t "[INFO] The todo #~a has been deleted.~%" id)))))

(defun dispatch-help ()
    (print-multiple-lines '("- add - prompts for a new todo."
                           "- show [all|current|done] - shows a list of all, current, or done todos. Default value: last type of a list."
                           "- edit <id> - prompts for edit of a chosen todo."
                           "- mark <id> done|undone - marks todo as either done, or undone."
                           "- help - displays this message."
                           "- exit - exits the application.")))

(defun dispatch-welcome-message ()
    (print-multiple-lines '("Jon Krazov presents:"
                            ""
                            "T O D O   L I S T"
                            "-----------------"
                            ""
                            "Type a command ('help' for the manual, 'exit' to leave).")))

(defun dispatch (commands)
    (let ((operation (parse-operation (first commands))))
        (format t "~%")
        (case operation
            (add
                (dispatch-add)
                (save-db "todos.db"))
            (show
                (dispatch-show (cdr commands)))
            (edit
                (dispatch-edit (cdr commands))
                (save-db "todos.db"))
            (mark
                (dispatch-mark (cdr commands))
                (save-db "todos.db"))
            (delete
                (dispatch-delete (cdr commands))
                (save-db "todos.db"))
            (help
                (dispatch-help))
            (exit
                (format t "Goodbye.~%"))
            (otherwise
                (format t "[ERROR] Unknown command:~{ ~a~}\.~%" commands)))
        operation))

;;; fire!

(defun main ()
    (load-db "todos.db")
    (clear-screen)
    (dispatch-welcome-message)
    (do () ((exit? (dispatch (list-of-words (prompt-for-command)))))))
