(in-package :steganography)

(defvar *dir* "presentation")

(defun make-bit-plane-images (file)
  (let* ((image (read-image file))
         (bit-planes (separate-bit-planes image)))
    (dotimes (i (array-dimension bit-planes 0))
      (let ((bimage (make-image-from-bit-plane bit-planes i)))
        (write-png bimage (format nil "~A/~A-~A" *dir* i file))))))
