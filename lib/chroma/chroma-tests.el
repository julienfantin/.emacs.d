(require 'chroma)
(require 'ert)


;;; Type constructors

(assert (equal "#fff" (oref (chroma-hex :hex "#fff") hex)))

(let* ((n 127)
       (f (/ 127.0 255.0))
       (nog (chroma-rgb :r n :g n :b n))
       (fog (chroma-rgb :r f :g f :b f)))
  (assert (equal f (oref nog r)))
  (assert (equal f (oref nog g)))
  (assert (equal f (oref nog b)))
  (assert (equal f (oref fog r)))
  (assert (equal f (oref fog g)))
  (assert (equal f (oref fog b)))
  (assert (equal nog (chroma-rgb f f f)))
  (assert (equal nog (chroma-rgb (make-list 3 f))))
  (assert (equal fog (chroma-rgb n n n)))
  (assert (equal fog (chroma-rgb (make-list 3 n)))))

(let* ((n 127)
       (f (/ 127.0 360.0))
       (m 50)
       (mf (/ 50.0 100.0))
       (nog (chroma-hsl :h n :s m :l m))
       (fog (chroma-hsl :h f :s mf :l mf)))
  (assert (equal f (oref nog h)))
  (assert (equal mf (oref nog s)))
  (assert (equal mf (oref nog l)))
  (assert (equal f (oref fog h)))
  (assert (equal mf (oref fog s)))
  (assert (equal mf (oref fog l)))
  (assert (equal nog (chroma-hsl f mf mf)))
  (assert (equal nog (chroma-hsl (list f mf mf))))
  (assert (equal fog (chroma-hsl n m m)))
  (assert (equal fog (chroma-hsl (list n m m)))))


;;; Methods

(let* ((og "#0b730c"))
  ;; hex
  (let ((c (chroma-hex :hex og)))
    (assert (string= og (chroma-to-string c)))
    (assert (string= og (chroma-to-string (chroma-to-hsl c))))
    (assert (string= og (chroma-to-string (chroma-to-rgb c)))))
  ;; rgb
  (seq-let (r g b) (color-name-to-rgb og)
    (let ((c (chroma-rgb :r r :g g :b b)))
      (assert (string= og (chroma-to-string c)))
      (assert (string= og (chroma-to-string (chroma-to-hsl c))))
      (assert (string= og (chroma-to-string (chroma-to-hex c))))))
  ;; hsl
  (seq-let (h s l) (apply #'color-rgb-to-hsl (color-name-to-rgb og))
    (let ((c (chroma-hsl :h h :s s :l l)))
      (assert (string= og (chroma-to-string c)))
      (assert (string= og (chroma-to-string (chroma-to-rgb c))))
      (assert (string= og (chroma-to-string (chroma-to-hex c)))))))


;;; Function
;;;; lighten

(assert
 (string=
  (let* ((hex  (chroma-hex :hex "#0b730c"))
         (hex* (chroma-lighten hex 50)))
    (chroma-to-string hex*))
  "#89f38a"))

(assert
 (string=
  (seq-let (r g b) (color-name-to-rgb "#0b730c")
    (let* ((rgb  (chroma-rgb :r r :g g :b b))
           (rgb* (chroma-lighten rgb 50)))
      (chroma-to-string rgb*)))
  "#89f38a"))

(assert
 (string=
  (seq-let (h s l) (apply #'color-rgb-to-hsl (color-name-to-rgb "#0b730c"))
    (let* ((hsl  (chroma-hsl :h h :s s :l l))
           (hsl* (chroma-lighten hsl 50)))
      (chroma-to-string hsl*)))
  "#89f38a"))

;;;; Darken

(assert
 (string=
  (let* ((hex  (chroma-hex :hex "#89f38a"))
         (hex* (chroma-darken hex 50)))
    (chroma-to-string hex*))
  "#0b710c"))

(assert
 (string=
  (seq-let (r g b) (color-name-to-rgb "#89f38a")
    (let* ((rgb  (chroma-rgb :r r :g g :b b))
           (rgb* (chroma-darken rgb 50)))
      (chroma-to-string rgb*)))
  "#0b710c"))

(assert
 (string=
  (seq-let (h s l) (apply #'color-rgb-to-hsl (color-name-to-rgb "#89f38a"))
    (let* ((hsl  (chroma-hsl :h h :s s :l l))
           (hsl* (chroma-darken hsl 50)))
      (chroma-to-string hsl*)))
  "#0b710c"))


;;; Color theory

(assert
 (equal
  (chroma-darken (chroma-darken (chroma-hex :hex "white") 10) 10)
  (chroma--iterate (chroma-hex :hex "white") 2 #'chroma-darken 10)))

(assert
 (equal
  (list
   (chroma-hex :hex "white")
   (chroma-darken (chroma-hex :hex "white") 10)
   (chroma-darken (chroma-darken (chroma-hex :hex "white") 10) 10))
  (chroma--iterate* (chroma-hex :hex "white") 2 #'chroma-darken 10)))
