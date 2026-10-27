;;; FLATDIMS.lsp  (v2 - adds full multileader fix)
;;; Flattens all model space dimensions, leaders, and multileaders to Z=0
;;; and resets their normals to face up.
;;; Fixes "was not parallel to the UCS" grip errors.
;;; One U undoes the whole run.

;; Keep X and Y, set Z to 0
(defun FD:flat (p) (list (car p) (cadr p) 0.0))

;; Angle fix for dims that faced down / were tilted
(defun FD:ang (a nrm / v)
  (setq v (trans (list (cos a) (sin a) 0.0) nrm 0 T))
  (angle '(0 0) (list (car v) (cadr v))))

;; Length of a 3D vector
(defun FD:len (v) (distance '(0.0 0.0 0.0) v))

;; Direction vector -> flat unit vector in the XY plane
(defun FD:unit2d (v / l)
  (setq l (distance '(0.0 0.0) (list (car v) (cadr v))))
  (if (> l 1e-12) (list (/ (car v) l) (/ (cadr v) l) 0.0) v))

;; Multileader data:
;;   10 12 15 110      points (content base, text location, block position,
;;                     plane origin, leader vertices) -> Z = 0
;;   111 112           plane X / Y axis directions    -> flat unit vectors
;;   11 13 14          mixed codes: if length is 1 it's a direction/normal,
;;                     otherwise it's a point (leader break points)
;;                     normals (mostly Z)   -> exactly 0,0,1 (or 0,0,-1)
;;                     directions (mostly XY) -> flat unit vectors
;;                     points                -> Z = 0
(defun FD:mleader (ed)
  (mapcar
    '(lambda (p / c v)
       (setq c (car p) v (cdr p))
       (cond
         ((not (and (listp v) (= (length v) 3))) p)
         ((member c '(10 12 15 110)) (cons c (FD:flat v)))
         ((member c '(111 112))      (cons c (FD:unit2d v)))
         ((member c '(11 13 14))
          (cond
            ((not (equal (FD:len v) 1.0 1e-6)) (cons c (FD:flat v)))
            ((> (abs (caddr v)) 0.5)
             (cons c (if (minusp (caddr v)) '(0.0 0.0 -1.0) '(0.0 0.0 1.0))))
            (T (cons c (FD:unit2d v)))))
         (T p)))
    ed))

(defun c:FLATDIMS (/ ss i e ed typ n fail nrm wc oc)
  (command "_.UNDO" "_BE")
  (command "_.UCS" "_W")
  (setq n 0 fail 0 i 0)
  (if (setq ss (ssget "_X"
         '((0 . "DIMENSION,ARC_DIMENSION,LEADER,MULTILEADER")
           (410 . "Model"))))
    (repeat (sslength ss)
      (setq e   (ssname ss i)
            ed  (entget e)
            typ (cdr (assoc 0 ed))
            nrm (cond ((cdr (assoc 210 ed))) ('(0.0 0.0 1.0))))
      (if (= typ "MULTILEADER")
        (setq ed (FD:mleader ed))
        (progn
          (if (wcmatch typ "*DIMENSION")
            (setq wc '(10 13 14 15) oc '(11 12 16))
            (setq wc '(10) oc nil))
          (setq ed
            (mapcar
              '(lambda (p / c)
                 (setq c (car p))
                 (cond
                   ((and (member c wc) (listp (cdr p)) (= (length p) 4))
                    (cons c (FD:flat (cdr p))))
                   ((and (member c oc) (listp (cdr p)) (= (length p) 4))
                    (cons c (FD:flat (trans (cdr p) nrm 0))))
                   ((and (wcmatch typ "*DIMENSION") (member c '(50 53)))
                    (cons c (FD:ang (cdr p) nrm)))
                   ((= c 210) '(210 0.0 0.0 1.0))
                   ((and (= c 211) (= typ "LEADER"))
                    (cons c (FD:flat (cdr p))))
                   (T p)))
              ed))))
      (if (entmod ed)
        (progn (entupd e) (setq n (1+ n)))
        (setq fail (1+ fail)))
      (setq i (1+ i))))
  (command "_.UNDO" "_E")
  (princ (strcat "\nFLATDIMS: " (itoa n) " dims/leaders/multileaders flattened, "
                 (itoa fail) " failed (locked layer?)."))
  (princ))

(princ "\nFLATDIMS loaded. Type FLATDIMS to run.")
(princ)
