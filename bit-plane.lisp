(in-package :steganography)

(defun shift-color (color &optional (bits -4))
  (multiple-value-bind (r g b) (imago:color-rgb color)
    (let ((n-r (ash r bits))
          (n-g (ash g bits))
          (n-b (ash b bits)))
      (imago:make-color n-r n-g n-b))))

(defun zero-bits (color bits)
  (multiple-value-bind (r g b) (imago:color-rgb color)
    (imago:make-color (dpb 0 (byte bits 0) r)
                      (dpb 0 (byte bits 0) g)
                      (dpb 0 (byte bits 0) b))))

(defun extract-color-bits (color bits)
  (multiple-value-bind (r g b) (imago:color-rgb color)
    (imago:make-color (ldb (byte bits 0) r)
                      (ldb (byte bits 0) g)
                      (ldb (byte bits 0) b))))

(defun add-colors (color1 color2)
  (multiple-value-bind (r1 g1 b1) (imago:color-rgb color1)
    (multiple-value-bind (r2 g2 b2) (imago:color-rgb color2)
      (imago:make-color (+ r1 r2) (+ g1 g2) (+ b1 b2)))))

(defmethod shift-image ((image imago:image) bits)
  (let ((shifted-image (make-instance (class-of image)
                                      :width (imago:image-width image)
                                      :height (imago:image-height image))))
    (imago:copy shifted-image image)
    (imago:do-image-pixels (shifted-image color x y)
      (setf color (shift-color color bits)))
    shifted-image))

(defmethod shift-image ((image string) bits)
  (let ((im (imago:read-image image)))
    (shift-image im bits)
    im))

(defun combine-images (cover-image secret-image &key (embed-bits 4) encryption-key)
  (setq cover-image
        (imago:resize cover-image
                      (image-width secret-image)
                      (image-height secret-image)))
  (when (typep cover-image 'grayscale-image)
    (setq cover-image (convert-to-rgb cover-image)))
  (when (typep secret-image 'grayscale-image)
    (setq secret-image (convert-to-rgb secret-image)))
  (let ((shifted-image (shift-image secret-image (- embed-bits))))
    (when encryption-key
      (setq shifted-image
            (encrypt-shifted-image shifted-image encryption-key)))
    (imago:do-image-pixels (cover-image cover-color x y)
      (let ((new-color (zero-bits cover-color embed-bits)))
        (setf cover-color
              (add-colors new-color (imago:image-pixel shifted-image x y)))))
    cover-image))

(defun extract-image (stego-image &key (embed-bits 4) encryption-key)
  (let* ((secret-image (make-instance (class-of stego-image)
                                      :width (imago:image-width stego-image)
                                      :height (imago:image-height stego-image))))
    (imago:do-image-pixels (secret-image secret-color x y)
      (let ((stego-color (imago:image-pixel stego-image x y)))
        (let ((new-color (extract-color-bits stego-color embed-bits)))
          (setf secret-color new-color))))
    (when encryption-key
      (setq secret-image (decrypt-shifted-image secret-image encryption-key)))
    (let ((new-image (shift-image secret-image embed-bits)))
      new-image)))
