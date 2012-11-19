;; ASDF package description for steganography              -*- Lisp -*-

(defpackage :steganography-system (:use :cl :asdf))
(in-package :steganography-system)

(defsystem steganography
  :name "Image Steganography Library"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Image Steganography Library"
  :depends-on (:imago
               :trivial-shell
               :cl-ppcre
               :ironclad
               :cl-utilities)
  :components ((:file "steganography-package")
	       (:file "globals" :depends-on ("steganography-package"))
               (:file "encryption" :depends-on ("globals"))
               (:file "bit-plane" :depends-on ("encryption"))
               (:file "bpcs" :depends-on ("bit-plane"))
               (:file "text" :depends-on ("bpcs"))
               (:file "interface" :depends-on ("text"))))
