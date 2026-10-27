;;; FLATMORE.lsp  (v3)
;;; Commands:
;;;   FLATMORE   Intentional second pass for strongly tilted LWPOLYLINE objects.
;;;   FLATPOINTS Flatten reviewed POINT objects only within a user-set Z range.
;;;
;;; FLATPOINTS default range: -5.0 through +5.0 drawing units, inclusive.
;;; Points outside the range are preserved and counted.
;;; Both commands require confirmation and use one undo group.

(defun FM:z0 (p) (list (car p) (cadr p) 0.0))

(defun FM:nrm (ed)
  (cond ((cdr (assoc 210 ed))) ('(0.0 0.0 1.0))))

(defun FM:vertical (nrm)
  (if (minusp (caddr nrm))
    '(0.0 0.0 -1.0)
    '(0.0 0.0 1.0)))

(defun FM:tilted-p (nrm)
  (not (equal (abs (caddr nrm)) 1.0 1e-14)))

(defun FM:project-ocs (p nrm)
  (FM:z0 (trans (FM:z0 (trans p nrm 0)) 0 (FM:vertical nrm))))

(defun FM:set-normal (ed nrm)
  (if (assoc 210 ed)
    (subst (cons 210 (FM:vertical nrm)) (assoc 210 ed) ed)
    ed))

(defun FM:flat-lwpoly (e / ed nrm elev new q)
  (setq ed   (entget e)
        nrm  (FM:nrm ed)
        elev (cond ((cdr (assoc 38 ed))) (0.0)))
  (setq new
    (mapcar
      '(lambda (p)
         (cond
           ((= (car p) 10)
            (setq q (FM:project-ocs
                      (list (cadr p) (caddr p) elev) nrm))
            (list 10 (car q) (cadr q)))
           ((= (car p) 38) '(38 . 0.0))
           (T p)))
      ed))
  (setq new (FM:set-normal new nrm))
  (if (entmod new) (progn (entupd e) T) nil))

(defun FM:flat-point (e / ed nrm new)
  (setq ed  (entget e)
        nrm (FM:nrm ed))
  (setq new
    (mapcar
      '(lambda (p)
         (if (and (= (car p) 10)
                  (listp (cdr p))
                  (= (length p) 4))
           (cons 10 (FM:z0 (cdr p)))
           p))
      ed))
  (setq new (FM:set-normal new nrm))
  (if (entmod new) (progn (entupd e) T) nil))

;;; ---------- FLATMORE ----------

(defun c:FLATMORE (/ ss candidates i e ed nrm ans ok fail)
  (setq candidates (ssadd) i 0)
  (if (setq ss (ssget "_X" '((0 . "LWPOLYLINE") (410 . "Model"))))
    (repeat (sslength ss)
      (setq e (ssname ss i)
            ed (entget e)
            nrm (FM:nrm ed))
      (if (FM:tilted-p nrm) (ssadd e candidates))
      (setq i (1+ i))))

  (princ (strcat "\nFLATMORE found " (itoa (sslength candidates))
                 " tilted LWPOLYLINE objects."))
  (if (= 0 (sslength candidates))
    (princ "\nFLATMORE: nothing to change.")
    (progn
      (initget "Yes No")
      (setq ans (getkword
        "\nProject these polylines to World Z=0? [Yes/No] <No>: "))
      (if (= ans "Yes")
        (progn
          (command "_.UNDO" "_BE")
          (setq i 0 ok 0 fail 0)
          (repeat (sslength candidates)
            (if (FM:flat-lwpoly (ssname candidates i))
              (setq ok (1+ ok))
              (setq fail (1+ fail)))
            (setq i (1+ i)))
          (command "_.UNDO" "_E")
          (princ (strcat "\nFLATMORE: " (itoa ok)
                         " flattened, " (itoa fail)
                         " failed (locked layer?).")))
        (princ "\nFLATMORE: canceled; no objects changed."))))
  (princ))

;;; ---------- FLATPOINTS ----------

(defun c:FLATPOINTS (/ ss eligible outside i e ed p z nrm cutoff input
                        ans ok fail)
  (initget 6)
  (setq input (getreal
    "\nMaximum absolute POINT Z to flatten <5.0>: "))
  (setq cutoff (if input input 5.0)
        eligible (ssadd)
        outside 0
        i 0)

  (if (setq ss (ssget "_X" '((0 . "POINT") (410 . "Model"))))
    (repeat (sslength ss)
      (setq e   (ssname ss i)
            ed  (entget e)
            p   (cdr (assoc 10 ed))
            z   (caddr p)
            nrm (FM:nrm ed))
      ;; Only points that actually need cleanup are candidates.
      (if (or (/= z 0.0) (FM:tilted-p nrm))
        (if (<= (abs z) cutoff)
          (ssadd e eligible)
          (setq outside (1+ outside))))
      (setq i (1+ i))))

  (princ (strcat "\nFLATPOINTS review, cutoff +/-"
                 (rtos cutoff 2 6) ":"))
  (princ (strcat "\n  Eligible within range: "
                 (itoa (sslength eligible))))
  (princ (strcat "\n  Outside range, preserved: "
                 (itoa outside)))

  (if (= 0 (sslength eligible))
    (princ "\nFLATPOINTS: nothing to change.")
    (progn
      (initget "Yes No")
      (setq ans (getkword
        "\nFlatten eligible POINT objects to Z=0? [Yes/No] <No>: "))
      (if (= ans "Yes")
        (progn
          (command "_.UNDO" "_BE")
          (setq i 0 ok 0 fail 0)
          (repeat (sslength eligible)
            (if (FM:flat-point (ssname eligible i))
              (setq ok (1+ ok))
              (setq fail (1+ fail)))
            (setq i (1+ i)))
          (command "_.UNDO" "_E")
          (princ (strcat "\nFLATPOINTS: " (itoa ok)
                         " flattened, " (itoa fail)
                         " failed (locked layer?).")))
        (princ "\nFLATPOINTS: canceled; no objects changed."))))
  (princ))

(princ "\nFLATMORE loaded. Commands: FLATMORE, FLATPOINTS.")
(princ)
