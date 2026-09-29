;;; FLATDIMS.lsp
;;; Flattens all model space dimensions and leaders to Z=0
;;; and resets their normal (210) to face up (0,0,1).
;;; Fixes "was not parallel to the UCS" grip errors.
;;; One U undoes the whole run.

(defun FD:flat (p) (list (car p) (cadr p) 0.0))

(defun FD:ang (a nrm / v)
  (setq v (trans (list (cos a) (sin a) 0.0) nrm 0 T))
  (angle '(0 0) (list (car v) (cadr v))))

(defun c:FLATDIMS (/ ss i e ed typ n nrm wc oc)
  (command "_.UNDO" "_BE")
  (command "_.UCS" "_W")
  (setq n 0 i 0)
  (if (setq ss (ssget "_X"
         '((0 . "DIMENSION,ARC_DIMENSION,LEADER,MULTILEADER")
           (410 . "Model"))))
    (repeat (sslength ss)
      (setq e   (ssname ss i)
            ed  (entget e)
            typ (cdr (assoc 0 ed))
            nrm (cond ((cdr (assoc 210 ed))) ('(0.0 0.0 1.0))))
      (cond ((wcmatch typ "*DIMENSION") (setq wc '(10 13 14 15) oc '(11 12 16)))
            ((= typ "LEADER")          (setq wc '(10) oc nil))
            (T                         (setq wc '(10 12 15 110) oc nil)))
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
               ((and (= c 210) (/= typ "MULTILEADER"))
                '(210 0.0 0.0 1.0))
               ((and (= c 211) (= typ "LEADER"))
                (cons c (FD:flat (cdr p))))
               (T p)))
          ed))
      (if (entmod ed) (progn (entupd e) (setq n (1+ n))))
      (setq i (1+ i))))
  (command "_.UNDO" "_E")
  (princ (strcat "\nFLATDIMS: " (itoa n) " objects flattened and normals reset."))
  (princ))

(princ "\nFLATDIMS loaded. Type FLATDIMS to run.")
(princ)
