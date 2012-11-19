(in-package :cl-steganography)

(defun convert-image-to-png (image-file)
  (if (scan "\.png$" image-file)
      image-file
      (let ((png-name (regex-replace "\\.[a-z0-9A-Z]+$" image-file ".png")))
        (multiple-value-bind (output error-output exit-status)
            (trivial-shell:shell-command
             (format nil "~A ~A ~A" *convert* image-file png-name))
          (declare (ignore output))
          (if (= 0 exit-status)
              png-name
              (error "Could not convert ~A to PNG: ~A"
                     image-file error-output))))))

(defmethod display ((file string))
  (trivial-shell:shell-command
   (format nil "~A ~A" *display* file)))

(defmethod display ((image image))
  (let ((file (format nil "/var/tmp/stegtmp-~A.png" (get-universal-time))))
    (unwind-protect
         (progn
           (write-png image file)
           (display file))
      (delete-file file))))

(defun hide-image (cover secret &key encryption-key (steg-file "steg.png")
                   (method :bit-plane) (region-size 16))
  (let* ((cover-png (convert-image-to-png cover))
         (secret-png (convert-image-to-png secret))
         (cover-image (read-image cover-png))
         (secret-image (read-image secret-png)))
    (let ((steg-image
           (cond ((eq method :bit-plane)
                  (combine-images cover-image
                                  secret-image
                                  :encryption-key encryption-key))
                 ((eq method :bpcs)
                  (bpcs-combine cover-image
                                secret-image
                                :use-cgc? nil
                                :region-size region-size
                                :encryption-key encryption-key)))))
      (write-png steg-image steg-file)
      (trivial-shell:shell-command
       (format nil "~A ~A" *display* steg-file))
      steg-file)))

(defun decode-image (steg-image &key original-image encryption-key
                     (secret-file "secret.png") (method :bit-plane)
                     (region-size 16))
  (let ((secret-image
         (cond ((eq method :bit-plane)
                (extract-image (read-image steg-image)
                               :encryption-key encryption-key))
               ((eq method :bpcs)
                (extract-bpcs (read-image steg-image)
                              (read-image original-image)
                              :use-cgc? nil
                              :region-size region-size
                              :encryption-key encryption-key)))))
    (write-png secret-image secret-file)
    (trivial-shell:shell-command
     (format nil "~A ~A" *display* secret-file))
    secret-file))
