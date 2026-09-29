;;; FLATALL.lsp  (v3)
;;; Commands in this file:
;;;   FLATALL    Flatten model space 2D objects to Z=0 and fix slight tilt.
;;;   FLATHATCH  Rebuild slightly tilted hatches flat at Z=0.
;;;   FINDTILT   Select model space objects whose normal is not exactly 0,0,1.
;;;
;;; FLATALL handles:
;;;   LINE, LWPOLYLINE, 2D POLYLINE, ARC, CIRCLE, ELLIPSE,
;;;   TEXT, MTEXT, INSERT (blocks, xrefs, attributes), HATCH
;;; Skips and reports:
;;;   - associative arrays (explode + flatten + re-array manually)
;;;   - 3D polylines, polyface / polygon meshes
;;;   - objects clearly tilted in 3D
;;;   - hatches with more than microscopic tilt (use FLATHATCH)
;;; Never selects solids, surfaces, 3D faces, or meshes.
;;; Does NOT flatten geometry inside block definitions.
;;; Each command: one U undoes the whole run.

;;; ---------- helpers ----------

;; Keep X and Y, set Z to 0
(defun FA:z0 (p) (list (car p) (cadr p) 0.0))

;; Object normal (default 0,0,1)
(defun FA:nrm (ed) (cond ((cdr (assoc 210 ed))) ('(0.0 0.0 1.0))))

;; Exact vertical normal, keeping up/down direction
(defun FA:nn (nrm) (if (minusp (caddr nrm)) '(0.0 0.0 -1.0) '(0.0 0.0 1.0)))

;; T if the object faces (almost) straight up or down
(defun FA:upright-p (nrm tol) (equal (abs (caddr nrm)) 1.0 tol))

;; OCS point on old normal -> OCS point on exact vertical normal, Z=0
(defun FA:ocs (p nrm)
  (FA:z0 (trans (FA:z0 (trans p nrm 0)) 0 (FA:nn nrm))))

;; Replace the normal with the exact vertical one
(defun FA:setn (ed nrm)
  (if (assoc 210 ed)
    (subst (cons 210 (FA:nn nrm)) (assoc 210 ed) ed)
    ed))

;; Set Z to 0 on the listed WCS point codes. once = T only fixes the first match.
(defun FA:zero (ed codes once / done)
  (mapcar
    '(lambda (p)
       (if (and (member (car p) codes)
                (listp (cdr p))
                (= (length p) 4)
                (not (and once (member (car p) done))))
         (progn (setq done (cons (car p) done))
                (cons (car p) (FA:z0 (cdr p))))
         p))
    ed))

;; Convert the listed OCS point codes with FA:ocs
(defun FA:fixocs (ed codes nrm)
  (mapcar
    '(lambda (p)
       (if (and (member (car p) codes) (listp (cdr p)) (= (length p) 4))
         (cons (car p) (FA:ocs (cdr p) nrm))
         p))
    ed))

;; T if TEXT / ATTRIB uses an alignment point (code 11)
(defun FA:just-p (ed / vc)
  (setq vc (if (= (cdr (assoc 0 ed)) "ATTRIB") 74 73))
  (or (/= 0 (cond ((cdr (assoc 72 ed))) (0)))
      (/= 0 (cond ((cdr (assoc vc ed))) (0)))))

;; T if the INSERT is an associative array
(defun FA:array-p (ed / r)
  (and (= (cdr (assoc 0 ed)) "INSERT")
       (setq r (member '(102 . "{ACAD_REACTORS") ed))
       (vl-some
         '(lambda (p)
            (and (= (car p) 330)
                 (= (cdr (assoc 0 (entget (cdr p)))) "ACDBASSOCDEPENDENCY")))
         (cdr r))))

;; 2D heavy polyline vertices
(defun FA:vtx (e nrm el / s sd)
  (setq s (entnext e))
  (while (and s (/= "SEQEND" (cdr (assoc 0 (setq sd (entget s))))))
    (entmod
      (mapcar
        '(lambda (p)
           (if (= (car p) 10)
             (cons 10 (FA:ocs (list (cadr p) (caddr p) el) nrm))
             p))
        sd))
    (setq s (entnext s))))

;; Block attributes
(defun FA:att (e / s sd an)
  (setq s (entnext e))
  (while (and s (/= "SEQEND" (cdr (assoc 0 (setq sd (entget s))))))
    (setq an (FA:nrm sd))
    (if (FA:upright-p an 1e-8)
      (entmod (FA:setn (FA:fixocs sd (if (FA:just-p sd) '(10 11) '(10)) an) an)))
    (setq s (entnext s))))

;; Flatten ONE entity. tol = how much tilt is allowed.
;; Returns "ok", "tilt" (flattened + tilt fixed), "array", "skip", or "fail".
(defun FA:flat1 (e tol / ed typ flg nrm el tilt)
  (setq ed  (entget e)
        typ (cdr (assoc 0 ed))
        flg (cond ((cdr (assoc 70 ed))) (0))
        nrm (FA:nrm ed))
  (cond
    ;; associative array
    ((FA:array-p ed) "array")
    ;; 3D polyline (8), polygon mesh (16), polyface mesh (64)
    ((and (= typ "POLYLINE") (/= 0 (logand 88 flg))) "skip")
    ;; clearly tilted in 3D (line position doesn't use the normal)
    ((and (/= typ "LINE") (not (FA:upright-p nrm tol))) "skip")
    ;; hatch boundaries are not converted, so only allow microscopic tilt
    ((and (= typ "HATCH") (not (FA:upright-p nrm 1e-12))) "skip")
    (T
     (setq tilt (not (equal nrm (FA:nn nrm) 1e-14)))
     (setq ed
       (cond
         ((= typ "LINE")
          (FA:setn (FA:zero ed '(10 11) nil) nrm))
         ((= typ "LWPOLYLINE")
          (setq el (cond ((cdr (assoc 38 ed))) (0.0)))
          (FA:setn
            (mapcar
              '(lambda (p / q)
                 (cond
                   ((= (car p) 10)
                    (setq q (FA:ocs (list (cadr p) (caddr p) el) nrm))
                    (list 10 (car q) (cadr q)))
                   ((= (car p) 38) '(38 . 0.0))
                   (T p)))
              ed)
            nrm))
         ((= typ "POLYLINE")
          (setq el (cadddr (assoc 10 ed)))
          (FA:setn (FA:zero ed '(10) nil) nrm))
         ((member typ '("ARC" "CIRCLE" "INSERT"))
          (FA:setn (FA:fixocs ed '(10) nrm) nrm))
         ((= typ "TEXT")
          (FA:setn (FA:fixocs ed (if (FA:just-p ed) '(10 11) '(10)) nrm) nrm))
         ((member typ '("ELLIPSE" "MTEXT"))
          (FA:setn (FA:zero ed '(10 11) nil) nrm))
         ((= typ "HATCH")
          (FA:setn (FA:zero ed '(10) T) nrm))
         (T ed)))
     (if (entmod ed)
       (progn
         (cond
           ((= typ "POLYLINE") (FA:vtx e nrm el))
           ((and (= typ "INSERT") (= 1 (cdr (assoc 66 ed)))) (FA:att e)))
         (entupd e)
         (if tilt "tilt" "ok"))
       "fail"))))

;;; ---------- FLATALL ----------

(defun c:FLATALL (/ ss i r n tf skip arr fail)
  (command "_.UNDO" "_BE")
  (command "_.UCS" "_W")
  (setq n 0 tf 0 skip 0 arr 0 fail 0 i 0)
  (if (setq ss (ssget "_X"
         '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,ELLIPSE,TEXT,MTEXT,INSERT,HATCH")
           (410 . "Model"))))
    (repeat (sslength ss)
      (setq r (FA:flat1 (ssname ss i) 1e-8))
      (cond
        ((= r "ok")    (setq n (1+ n)))
        ((= r "tilt")  (setq n (1+ n) tf (1+ tf)))
        ((= r "array") (setq arr (1+ arr)))
        ((= r "skip")  (setq skip (1+ skip)))
        (T             (setq fail (1+ fail))))
      (setq i (1+ i))))
  (command "_.UNDO" "_E")
  (princ (strcat "\nFLATALL: " (itoa n) " flattened (" (itoa tf) " had tilt fixed)"
                 ", " (itoa arr) " arrays skipped"
                 ", " (itoa skip) " 3D/tilted skipped"
                 ", " (itoa fail) " failed (locked layer?)."))
  (princ))

;;; ---------- FLATHATCH ----------
;;; For each slightly tilted hatch:
;;;   1. HATCHGENERATEBOUNDARY makes boundary objects
;;;   2. boundaries are flattened with FA:flat1
;;;   3. -HATCH makes a new flat hatch from them
;;;   4. MATCHPROP copies pattern, scale, angle, layer, color from the old hatch
;;;   5. new hatch is placed under the old one in draw order
;;;   6. old hatch and temporary boundaries are deleted
;;; Skips gradient fills and hatches that are clearly tilted in 3D.

(defun FA:newents (last / e out typ)
  (setq out (ssadd)
        e   (if last (entnext last) (entnext)))
  (while e
    (setq typ (cdr (assoc 0 (entget e))))
    (if (not (member typ '("VERTEX" "SEQEND" "ATTRIB")))
      (ssadd e out))
    (setq e (entnext e)))
  out)

(defun FA:erase (ss / j)
  (setq j 0)
  (repeat (sslength ss) (entdel (ssname ss j)) (setq j (1+ j))))

(defun c:FLATHATCH (/ *error* oce ohp ss i h hd nrm last bnd j nh n skip fail)
  (setq oce (getvar "CMDECHO") ohp (getvar "HPASSOC"))
  (defun *error* (msg)
    (setvar "CMDECHO" oce) (setvar "HPASSOC" ohp)
    (command-s "_.UNDO" "_E")
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*"))
      (princ (strcat "\nFLATHATCH error: " msg)))
    (princ))
  (setvar "CMDECHO" 0)
  (setvar "HPASSOC" 0)
  (command "_.UNDO" "_BE")
  (command "_.UCS" "_W")
  (setq n 0 skip 0 fail 0 i 0)
  (if (setq ss (ssget "_X" '((0 . "HATCH") (410 . "Model"))))
    (repeat (sslength ss)
      (setq h   (ssname ss i)
            hd  (entget h)
            nrm (FA:nrm hd))
      (cond
        ;; not tilted: nothing to do
        ((equal nrm (FA:nn nrm) 1e-14))
        ;; gradient fill or clearly 3D: skip
        ((or (= 1 (cdr (assoc 450 hd))) (not (FA:upright-p nrm 1e-6)))
         (setq skip (1+ skip)))
        (T
         ;; 1. generate boundary
         (setq last (entlast))
         (command "_.HATCHGENERATEBOUNDARY" h "")
         (setq bnd (FA:newents last))
         (if (= 0 (sslength bnd))
           (setq fail (1+ fail))
           (progn
             ;; 2. flatten boundary
             (setq j 0)
             (repeat (sslength bnd)
               (FA:flat1 (ssname bnd j) 1e-6)
               (setq j (1+ j)))
             ;; 3. new hatch
             (setq last (entlast))
             (command "_.-HATCH" "_S" bnd "" "")
             (setq nh (entlast))
             (if (and nh
                      (not (eq nh last))
                      (= "HATCH" (cdr (assoc 0 (entget nh)))))
               (progn
                 ;; 4. copy properties, 5. keep draw order
                 (command "_.MATCHPROP" h nh "")
                 (command "_.DRAWORDER" nh "" "_U" h "")
                 ;; 6. clean up
                 (entdel h)
                 (FA:erase bnd)
                 (setq n (1+ n)))
               (progn
                 (FA:erase bnd)
                 (setq fail (1+ fail))))))))
      (setq i (1+ i))))
  (command "_.UNDO" "_E")
  (setvar "CMDECHO" oce)
  (setvar "HPASSOC" ohp)
  (princ (strcat "\nFLATHATCH: " (itoa n) " hatches rebuilt flat"
                 ", " (itoa skip) " skipped (gradient or 3D)"
                 ", " (itoa fail) " failed."))
  (princ))

;;; ---------- FINDTILT ----------

(defun c:FINDTILT (/ ss i e nrm out)
  (setq out (ssadd) i 0)
  (if (setq ss (ssget "_X" '((410 . "Model"))))
    (repeat (sslength ss)
      (setq e   (ssname ss i)
            nrm (assoc 210 (entget e)))
      (if (and nrm (not (equal (abs (cadddr nrm)) 1.0 1e-14)))
        (ssadd e out))
      (setq i (1+ i))))
  (sssetfirst nil out)
  (princ (strcat "\nFINDTILT: " (itoa (sslength out)) " tilted objects selected."))
  (princ))

(princ "\nFLATALL loaded. Commands: FLATALL, FLATHATCH, FINDTILT.")
(princ)
