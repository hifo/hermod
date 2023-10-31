#! /usr/bin/env exlisp
;;;; -*- Mode: Lisp -*-

(eval-when (:load-toplevel :compile-toplevel :execute)
  (mapcar #'require '(alexandria
		      eric
		      hoba
		      iolib
		      jonathan
		      local-time
		      quri
		      rutils
		      sqlite))
  (load "/home/eric/projects/hermod/ws/common.lisp"))

(defpackage :hermod-ws
  (:use :cl :hermod-common))

(in-package :hermod-ws)

(defparameter *root* "/var/local/eric/hermod")

(defun update (client client-id)
  (close client)
  (format t "UPDATE ~a~%" client-id))

(defun unknown (client command)
  (format client "Unknown command ~a~%" command)
  (close client))

(defun safe-rm (filename)
  (handler-case (sb-posix:unlink filename)
    (sb-posix:syscall-error (c)
      (unless (= 2 (sb-posix:syscall-errno c))
	(error c)))))

(defun main-loop ()
  (let ((filename (format nil "~a/listeners/~a.uds" *root* (sb-unix:unix-getpid))))
    (unwind-protect
	 (iolib:with-open-socket (socket :address-family :local :type :stream :connect :passive
					 :local-filename filename)
	   (uiop:run-program (format nil "chmod g+w ~a" filename) :output :string) ;; Should really use SB-POSIX:CHMOD but that only takes octal modes
	   (loop
	      (iolib:with-accept-connection (client socket :wait t)
		(let* ((msg (read client))
		       (command (first msg)))
		  (case command
		    (update (update client (getf (rest msg) :client-id)))
		    (quit (close client) (return))
		    (otherwise (unknown client command)))))))
      (safe-rm filename))))

(defun main ()
  (format t "READY~%")
  (main-loop))

;; Don't start main loop in REPL
(unless (member (find-package :swank-repl) (list-all-packages))
  (main))
