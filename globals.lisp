(in-package :cl-steganography)

(defparameter *rgb-black* #0xffffff)

(defvar *convert* "/usr/bin/convert")
(defvar *display* "/usr/bin/display")

(defun dbg (fmt &rest args)
  (apply #'format t fmt args)
  (terpri t))
