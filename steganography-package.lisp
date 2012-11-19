(in-package #:cl-user)

(defpackage #:cl-steganography
  (:use #:cl #:cl-ppcre #:imago)
  (:export #:hide-image #:decode-image))
