#! /usr/bin/env exlisp
;;;; -*- Mode: Lisp -*-

(eval-when (:load-toplevel :compile-toplevel :execute)
  (mapcar #'require '(alexandria
		      cl-fad
		      eric
		      hoba
		      iolib
		      jonathan
		      local-time
		      quri
		      rutils
		      sqlite))
  (load "common.lisp"))

(defpackage :hermod-api
  (:use :cl :hermod-common))

(in-package :hermod-api)

(defun test-action ()
  (format t "Status: 200 OK~%")
  (format t "Content-Type: application/json~%~%")
  (format t "~a~%" (jonathan:to-json '(1 2 3))))

(defun get-posts ()
  (format t "Status: 200 OK~%")
  (format t "Content-Type: application/json~%~%")
  (let* ((posts (select '(:id :deltas :secs) "FROM messages WHERE deleted = FALSE")))
    (format t "~a~%" (jonathan:to-json `(:|success| t :|posts| ,posts)))))

(defun send-client-updates (client-id)
  (dolist (filename (cl-fad:list-directory "/var/local/eric/hermod/listeners/"))
    (handler-case
	(iolib:with-open-socket (sock :address-family :local :type :stream :connect :active
				      :remote-filename (namestring filename))
	  (format sock "~s~%" `(update :client-id ,client-id)))
      (iolib/sockets:socket-connection-refused-error () nil))))

(defun create-post (delta client-id)
  (format t "Status: 200 OK~%")
  (format t "Content-Type: application/json~%~%")
  (let* ((last-post (or (getf (first (select '(:id) "FROM messages ORDER BY id DESC")) :id) 0))
	 (new-id (1+ last-post)))
    (sqlite:execute-non-query *sql* "INSERT INTO messages (id, deltas, secs, deleted) VALUES (?, ?, DATETIME('now'), FALSE)"
			      new-id
			      delta)
    (send-client-updates client-id)
    (format t "~a~%" (jonathan:to-json `(:|success| t :|new-id| ,new-id)))))

(defun update-post (id delta client-id)
  (format t "Status: 200 OK~%")
  (format t "Content-Type: application/json~%~%")
  (if id
      (progn
	(sqlite:execute-non-query *sql* "UPDATE messages SET deltas = ? WHERE id = ?"
				  delta
				  id)
	(send-client-updates client-id)
	(format t "~a~%" (jonathan:to-json `(:|success| t))))
      (format t "~a~%" (jonathan:to-json `(:|success| nil :|msg| "No ID provided.")))))

(defun unknown-action (action)
  (format t "Status: 400 Bad Request~%")
  (format t "Content-Type: text/plain~%~%")
  (format t "Unknown action ~s~%" action))

(defun error-unauthorized ()
  (format t "Status: 401 Unauthorized~%")
  (format t "Content-Type: text/plain~%~%")
  (format t "Error: Unauthorized~%"))

(defun error-xsrf ()
  (format t "Status: 401 Unauthorized~%")
  (format t "Content-Type: text/plain~%~%")
  (format t "Error: Bad XSRF~%"))

(defun main ()
  (let ((user (hoba:check-user)))
    (if user
	(let* ((get-params (quri:url-decode-params (or (uiop:getenv "QUERY_STRING") "")))
	       (post-body (if (url-bool (cdr (assoc "form" get-params :test #'string=)))
			      (alexandria:alist-hash-table
			       (mapcar (lambda (param)
					 (cons (intern (car param) :keyword)
					       (cdr param)))
				       (quri:url-decode-params (read-line))))
			      (alexandria:plist-hash-table
			       (jonathan:parse (read-line)))))
	       (action (intern (string-upcase
				(gethash :|action| post-body "")))))
	  (if (check-xsrf user (gethash :|xsrf| post-body))
	      (case action
		(get-posts (get-posts))
		(create-post (create-post (gethash :|delta| post-body (jonathan:to-json nil))
					  (gethash :|client-id| post-body)))
		(update-post (update-post (gethash :|id| post-body) (gethash :|delta| post-body (jonathan:to-json nil))
					  (gethash :|client-id| post-body)))
		(test (test-action))
		(otherwise (unknown-action action)))
	      (error-xsrf)))
	(error-unauthorized))))
  
(main)
