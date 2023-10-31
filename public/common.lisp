(eval-when (:load-toplevel :compile-toplevel :execute)
  (mapcar #'require '(alexandria
		      eric
		      sqlite)))

(defpackage :hermod-common
  (:use :cl)
  (:export :*sql*
	   :*conf*
	   :select
	   :assoc-ensure
	   :check-xsrf
	   :generate-xsrf
	   :sql-bool
	   :user-name
	   :url-bool))

(in-package :hermod-common)

(defvar *db* "/var/local/eric/hermod/conf.sxp")

(defparameter *sql* (sqlite:connect "/var/local/eric/hermod/db.sqlite"))

(defun parse-select-col-select (col)
  (typecase col
    (list (first col))
    (t col)))

(defun parse-select-col-key (col)
  (typecase col
    (list (second col))
    (t col)))

(defun select (cols rest-statement &optional (conn *sql*) sql-parameters)
  (let* ((cols-statement (format nil "~{~(~a~)~^, ~}" (mapcar #'parse-select-col-select cols)))
	 (statement (format nil "SELECT ~a ~a"
			    cols-statement
			    rest-statement)))
    (mapcar (lambda (row)
	      (alexandria:alist-plist
	       (mapcar #'cons (mapcar #'parse-select-col-key cols) row)))
	    (apply #'sqlite:execute-to-list conn statement sql-parameters))))

(defmacro assoc-ensure (item alist &rest rest &key key test test-not)
  (declare (ignorable key test test-not))
  (let ((cell (gensym))
	(new-cell (gensym)))
    `(let ((,cell (assoc ,item ,alist ,@rest)))
       (if ,cell ,cell
	   (let ((,new-cell (cons ,item nil)))
	     (push ,new-cell ,alist)
	     ,new-cell)))))

(defun generate-xsrf (user)
  (let* ((key (format nil "~a" (cdr (assoc :id user))))
	 (xsrf (format nil "~x" (random (floor 1e10))))
	 (data (if (probe-file *db*)
		  (eric:read-from-file *db*)
		  `((:xsrf . ((,key . ,xsrf))))))
	 (place (assoc key (cdr (assoc :xsrf data))
		       :test #'string=)))
    (if place
	(setf (cdr place) xsrf)
	(push (cons key xsrf) data))
    (eric:overwrite-to-file *db* "~s" data)
    xsrf))

(defun check-xsrf (user xsrf)
  (declare (ignore user xsrf))
  t)
;;   (let* ((key (format nil "~a" (cdr (assoc :id user))))
;; 	 (data (if (probe-file *db*)
;; 		  (eric:read-from-file *db*)))
;; 	 (place (assoc key (cdr (assoc :xsrf data))
;; 		       :test #'string=)))
;;     (equal xsrf (cdr place))))

;; (defun user-name (user)
;;   (or
;;    (when user
;;      (getf (jonathan:parse (cdr (assoc :data user))) :|name|))
;;    "USER-NAME"))

(defun sql-bool (x)
  (if x 1 0))

(defun url-bool (x)
  (and x (not (equal x ""))))

