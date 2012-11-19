(in-package :steganography)

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmethod pbc-to-cgc ((bit-array array))
  (let ((new-array (cl-utilities:copy-array bit-array)))
    (dotimes (i (array-dimension bit-array 0))
      (loop for x from 0 below (array-dimension bit-array 1) do
           (loop for y from 1 below (array-dimension bit-array 2) do
                (setf (aref new-array i x y)
                      (boole boole-xor
                             (aref bit-array i x (1- y))
                             (aref bit-array i x y))))))
    new-array))

(defmethod pbc-to-cgc ((image grayscale-image))
  (let* ((cgc-image (make-instance 'grayscale-image
                                   :width (image-width image)
                                   :height (image-height image))))
    (do-image-pixels (cgc-image intensity x y)
      (if (= y 0)
          (setf intensity (image-pixel image x y))
          (setf intensity
                (make-gray (boole boole-xor
                                  (gray-intensity (image-pixel image x (1- y)))
                                  (gray-intensity (image-pixel image x y)))))))
    cgc-image))

(defmethod cgc-to-pbc ((bit-array array))
  (let ((new-array (make-array (array-dimensions bit-array)
                               :element-type 'bit :initial-element 0)))
    (dotimes (i (array-dimension bit-array 0))
      (loop for x from 0 below (array-dimension bit-array 1) do
           (loop for y from 0 below (array-dimension bit-array 2) do
                (if (= y 0)
                    (setf (aref new-array i x y) (aref bit-array i x y))
                    (setf (aref new-array i x y)
                          (boole boole-xor
                                 (aref bit-array i x y)
                                 (aref new-array i x (1- y))))))))
    new-array))

(defmethod cgc-to-pbc ((image grayscale-image))
  (let* ((pbc-image (make-instance 'grayscale-image
                                   :width (image-width image)
                                   :height (image-height image))))
    (do-image-pixels (pbc-image intensity x y)
      (setf intensity (make-gray 0)))
    (do-image-pixels (pbc-image intensity x y)
      (if (= y 0)
          (setf intensity (image-pixel image x y))
          (setf intensity
                (make-gray (boole boole-xor
                                  (gray-intensity (image-pixel image x y))
                                  (gray-intensity (image-pixel pbc-image
                                                               x (1- y))))))))
    pbc-image))

(defun make-bpcs-header (region-size)
  (let ((header (make-array (list region-size region-size)
                            :element-type 'bit
                            :initial-element 0)))
    (dotimes (x region-size)
      (dotimes (y region-size)
        (when (or (and (evenp x) (oddp y))
                  (and (oddp x) (evenp y)))
          (setf (aref header x y) 1))))
    header))

(defun bpcs-header-p (array region-size i start-x start-y)
  (loop for x from start-x below (+ start-x region-size) do
       (loop for y from start-y below (+ start-y region-size) do
            (cond ((and (or (and (evenp x) (oddp y))
                            (and (oddp x) (evenp y)))
                        (= 1 (aref array i x y)))
                   t)
                  ((and (or (and (oddp x) (oddp y))
                            (and (evenp x) (evenp y)))
                        (= 0 (aref array i x y)))
                   t)
                  (t
                   (return-from bpcs-header-p nil)))))
  t)

(defun conjugate-array (array)
  (let ((a (make-array '(8 8) :element-type 'bit)))
    (dotimes (x 8)
      (dotimes (y 8)
        (if (or (and (evenp x) (evenp y))
                (and (oddp x) (oddp y)))
            (setf (aref a x y) 0)
            (setf (aref a x y) 1))))
    (dbg "Conjugating using checker pattern~%~A" a))
  (let ((checker (make-array (array-dimensions array)
                             :element-type 'bit :initial-element 0))
        (count 0))
    (dotimes (x (1- (array-dimension checker 0)))
      (dotimes (y (1- (array-dimension checker 1)))
        (setf (aref checker x y)
              (boole boole-xor (aref array x y)
                     (if (oddp count) 1 0)))
        (incf count)))
    checker))

(defun conjugate-block (bit-plane plane start-x start-y block-size)
  (let ((count 0))
    (loop for x from start-x to (1- (+ start-x block-size)) do
         (loop for y from start-y to (1- (+ start-y block-size)) do
              (setf (aref bit-plane plane x y)
                    (boole boole-xor
                           (aref bit-plane plane x y)
                           (if (oddp count) 1 0)))
              (incf count)))
    bit-plane))

(defun array-slice-3d (array size &rest indices)
  (let ((a (make-array size)))
    (dotimes (x (first size))
      (dotimes (y (second size))
        (let ((other-x (+ x (second indices)))
              (other-y  (+ y (third indices))))
          (setf (aref a x y)
                (aref array (first indices) other-x other-y)))))
    a))

(defun separate-bit-planes (image)
  (let* ((gimage (if (typep image 'grayscale-image)
                     image
                     (convert-to-grayscale image)))
         (bit-planes (make-array
                      (list 8 (image-width gimage) (image-height gimage))
                      :element-type 'bit
                      :initial-element 0)))
    (dotimes (i 8)
      (do-image-pixels (gimage intensity x y)
        (setf (aref bit-planes i x y)
              (ldb (byte 1 i) (gray-intensity intensity)))))
    bit-planes))

(defun make-image-from-bit-plane (bit-array plane)
  (assert (eq 'bit (array-element-type bit-array)))
  (let ((image (make-instance 'grayscale-image
                              :width (second (array-dimensions bit-array))
                              :height (third (array-dimensions bit-array)))))
    (dotimes (x (image-width image))
      (dotimes (y (image-height image))
        (setf (image-pixel image x y)
              (make-gray (if (= 0 (aref bit-array plane x y)) 0 255)))))
    image))

(defun dump-bit-plane (bit-plane file)
  (assert (eq 'bit (array-element-type bit-plane)))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (dotimes (x (array-dimension bit-plane 1))
      (dotimes (y (array-dimension bit-plane 2))
        (let ((intensity 0))
          (dotimes (i 8)
            (setf intensity (dpb (aref bit-plane i x y) (byte 1 i) intensity)))
          (format out "~3D " intensity)))
      (terpri out))))

(defun merge-bit-planes (bit-array)
  (assert (and (= (first (array-dimensions bit-array)) 8)
               (eq 'bit (array-element-type bit-array))))
  (let ((image (make-instance 'grayscale-image
                              :width (array-dimension bit-array 1)
                              :height (array-dimension bit-array 2))))
    (dotimes (x (image-width image))
      (dotimes (y (image-height image))
        (let ((intensity 0))
          (dotimes (i 8)
            (setf intensity (dpb (aref bit-array i x y) (byte 1 i) intensity)))
          (setf (image-pixel image x y)
                (make-gray intensity)))))
    image))

(defun calculate-border (planes i start-x start-y width height)
  (let ((border 0) (last-bit nil))
    (loop for x from start-x to (1- (+ start-x width)) do
         (loop for y from start-y to (1- (+ start-y height)) do
              (when (and last-bit
                         (/= last-bit (aref planes i x y)))
                (incf border))
              (setf last-bit (aref planes i x y)))
         (setf last-bit nil))
    (loop for y from start-y to (1- (+ start-y height)) do
         (loop for x from start-x to (1- (+ start-x width)) do
              (when (and last-bit
                         (/= last-bit (aref planes i x y)))
                (incf border))
              (setf last-bit (aref planes i x y)))
         (setf last-bit nil))
    border))

(defun calculate-region-alphas (bit-planes &key (region-width 4) (region-height 4))
  (let* ((x-count (floor (array-dimension bit-planes 1) region-width))
         (y-count (floor (array-dimension bit-planes 2) region-height))
         (alphas (make-array (list (array-dimension bit-planes 0)
                                   x-count y-count))))
    (dotimes (i (array-dimension bit-planes 0))
      (dotimes (x-region x-count)
        (dotimes (y-region y-count)
          (let ((start-x (* x-region region-width))
                (start-y (* y-region region-height)))
            (let ((k (calculate-border
                      bit-planes i start-x start-y region-width region-height))
                  (m (log region-width 2)))
              (setf (aref alphas i x-region y-region)
                    (/ k (* 2 (expt 2 m) (1- (expt 2 m))))))))))
    alphas))

(defun sort-alphas (alphas)
  (let ((sorted-alphas nil))
    (dotimes (i (array-dimension alphas 0))
      (dotimes (j (array-dimension alphas 1))
        (dotimes (k (array-dimension alphas 2))
          (push (list i j k (aref alphas i j k)) sorted-alphas))))
    (sort sorted-alphas #'> :key 'fourth)))

(defun embed-map (map bit-plane alphas region-size)
#|
  (let ((blk nil))
    (dolist (int (flatten map))
      (when (> int 255)
        (error "Map element too large: ~D" int))
      (push int blk)
      (when (= region-size
|#
  )

(defun calculate-average-alpha (alphas)
  (let ((sorted-alphas (sort-alphas alphas)))
    (/ (reduce #'+ sorted-alphas :key 'fourth) (length sorted-alphas))))

(defun bpcs-embed (cover-bit-plane secret-bit-plane cover-alphas secret-alphas
                   region-size &key (threshold 0.3))
  (let ((map nil) (sorted-alphas (sort-alphas cover-alphas))
        (combined-bit-plane (cl-utilities:copy-array cover-bit-plane))
        (test-bit-plane (make-array (array-dimensions secret-bit-plane)
                                    :element-type 'bit
                                    :initial-element 0)))
    (dbg "Average secret alpha: ~A" (calculate-average-alpha secret-alphas))
    (let ((x-count (floor (array-dimension secret-bit-plane 1) region-size))
          (y-count (floor (array-dimension secret-bit-plane 2) region-size)))
      (dotimes (secret-i (array-dimension secret-bit-plane 0))
        (dotimes (secret-x-region x-count)
          (dotimes (secret-y-region y-count)
            (let* ((cover-region (pop sorted-alphas))
                   (secret-start-x (* secret-x-region region-size))
                   (secret-start-y (* secret-y-region region-size))
                   (secret-end-x (1- (+ region-size secret-start-x)))
                   (secret-end-y (1- (+ region-size secret-start-y))))
              (when
                  (< (aref secret-alphas secret-i secret-x-region secret-y-region)
                     threshold)
                #|
                (dbg "WARNING: (~A,~A,~A) has alpha of ~F"
                     secret-i secret-x-region secret-y-region
                     (aref secret-alphas secret-i secret-x-region secret-y-region))
                |#
                (push (list secret-i secret-x-region secret-y-region) map)
                (conjugate-block secret-bit-plane secret-i secret-start-x
                                 secret-start-y region-size))
              (destructuring-bind (cover-i cover-x-region cover-y-region a)
                  cover-region
                (let ((cover-start-x (* cover-x-region region-size))
                      (cover-start-y (* cover-y-region region-size)))
                  (loop
                     for s-x from secret-start-x to secret-end-x
                     for c-x from cover-start-x below
                       (+ cover-start-x region-size) do
                       (loop
                          for s-y from secret-start-y to secret-end-y
                          for c-y from cover-start-y below
                            (+ cover-start-y region-size) do
                            (setf (aref combined-bit-plane cover-i c-x c-y)
                                  (aref secret-bit-plane secret-i s-x s-y))
                            (setf (aref test-bit-plane secret-i s-x s-y)
                                  (aref secret-bit-plane secret-i s-x s-y)))))))))))
    ;;(embed-map map combined-bit-plane sorted-alphas region-size)
    combined-bit-plane))

(defun bpcs-descramble (bpcs-bit-plane original-bit-plane alphas region-size
                        &key (threshold 0.3))
  (let* ((sorted-alphas (sort-alphas alphas))
         (secret-bit-plane (make-array
                            (list (array-dimension bpcs-bit-plane 0)
                                  (floor (array-dimension bpcs-bit-plane 1) 2)
                                  (floor (array-dimension bpcs-bit-plane 2) 2))
                            :element-type 'bit :initial-element 0))
         (x-count (floor (array-dimension secret-bit-plane 1) region-size))
         (y-count (floor (array-dimension secret-bit-plane 2) region-size)))
    (dotimes (secret-i (array-dimension secret-bit-plane 0))
      (dotimes (secret-x-region x-count)
        (dotimes (secret-y-region y-count)
          (let ((region (pop sorted-alphas)))
            (destructuring-bind (i x-region y-region a) region
              (let* ((start-x (* x-region region-size))
                     (start-y (* y-region region-size))
                     (end-x (1- (+ start-x region-size)))
                     (end-y (1- (+ start-y region-size)))
                     (secret-start-x (* secret-x-region region-size))
                     (secret-start-y (* secret-y-region region-size))
                     (secret-end-x (1- (+ region-size secret-start-x)))
                     (secret-end-y (1- (+ region-size secret-start-y))))
                (loop
                   for s-x from secret-start-x to secret-end-x
                   for c-x from start-x to end-x
                   do
                   (loop
                      for s-y from secret-start-y to secret-end-y
                      for c-y from start-y to end-y
                      do
                      (setf (aref secret-bit-plane secret-i s-x s-y)
                            (aref bpcs-bit-plane i c-x c-y))))))))))
    secret-bit-plane))

(defun compare-alphas (image1 image2 &key (region-size 16))
  (let* ((i1-bp (separate-bit-planes image1))
         (i1-alphas (sort-alphas
                     (calculate-region-alphas i1-bp
                                              :region-width region-size
                                              :region-height region-size)))
         (i2-bp (separate-bit-planes image2))
         (i2-alphas (sort-alphas
                     (calculate-region-alphas i2-bp
                                              :region-width region-size
                                              :region-height region-size))))
    ;; (i x-region y-region a)
    (let ((i1-avg-alpha (/ (reduce #'+ i1-alphas :key 'fourth)
                           (length i1-alphas)))
          (i2-avg-alpha (/ (reduce #'+ i2-alphas :key 'fourth)
                           (length i2-alphas))))
      (values i1-avg-alpha i2-avg-alpha))))

(defun bpcs-combine (cover-image secret-image &key encryption-key (region-size 4)
                     use-cgc?)
  (unless (and (= (* 2 (image-width secret-image)) (image-width cover-image))
               (= (* 2 (image-height secret-image)) (image-height cover-image)))
    (error
     "BPCS requires that the secret image be 1/2 the resolution of the cover"))
  (unless (typep cover-image 'grayscale-image)
    (setq cover-image (convert-to-grayscale cover-image)))
  (unless (typep secret-image 'grayscale-image)
    (setq secret-image (convert-to-grayscale secret-image)))
  (when encryption-key
    (setq secret-image (encrypt-image secret-image encryption-key)))
  (let* ((cover-bit-plane (if use-cgc?
                              (separate-bit-planes (pbc-to-cgc cover-image))
                              (separate-bit-planes cover-image)))
         (secret-bit-plane (if use-cgc?
                               (separate-bit-planes (pbc-to-cgc secret-image))
                               (separate-bit-planes secret-image)))
         (cover-alphas (calculate-region-alphas cover-bit-plane
                                                :region-width region-size
                                                :region-height region-size))
         (secret-alphas (calculate-region-alphas secret-bit-plane
                                                 :region-width region-size
                                                 :region-height region-size)))
     (let ((i (merge-bit-planes
               (bpcs-embed
                cover-bit-plane secret-bit-plane
                cover-alphas secret-alphas region-size))))
       (if use-cgc?
           (cgc-to-pbc i)
           i))))

(defun extract-bpcs (image original-image &key encryption-key (region-size 4)
                     use-cgc?)
  (unless (typep image 'grayscale-image)
    (setq image (convert-to-grayscale image)))
  (unless (typep original-image 'grayscale-image)
    (setq original-image (convert-to-grayscale original-image)))
  (let* ((original-bit-plane (if use-cgc?
                                 (separate-bit-planes (pbc-to-cgc original-image))
                                 (separate-bit-planes original-image)))
         (bit-plane (if use-cgc?
                        (separate-bit-planes (pbc-to-cgc image))
                        (separate-bit-planes image)))
         (alphas (calculate-region-alphas original-bit-plane
                                          :region-width region-size
                                          :region-height region-size)))
    (let ((secret-image
           (merge-bit-planes
            (bpcs-descramble
             bit-plane original-bit-plane alphas region-size))))
      (when use-cgc?
        (setq secret-image (cgc-to-pbc secret-image)))
      (if encryption-key
          (decrypt-image secret-image encryption-key)
          secret-image))))

