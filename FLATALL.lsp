;;; FLATALL.lsp  (v8 - adds XLINE cleanup)
;;; Commands in this file:
;;;   FLATALL    Flatten model space 2D objects to Z=0 and fix slight tilt.
;;;   FLATHATCH  Rebuild slightly tilted hatches flat at Z=0.
;;;   FINDTILT   Select tilted model space objects and report types.
;;;   FINDNONZERO Select nonzero/3D model space objects and report types.
;;;
;;; FLATALL handles:
;;;   LINE, XLINE, LWPOLYLINE, 2D POLYLINE, ARC, CIRCLE, ELLIPSE,
;;;   TEXT, MTEXT, ATTDEF, INSERT (blocks, xrefs, attributes), HATCH,
;;;   SOLID (2D solid fills), TRACE, planar SPLINE
;;;
;;; Skips and reports:
;;;   - associative arrays
;;;   - 3D polylines, non-planar splines, polyface / polygon meshes
;;;   - objects clearly tilted in 3D
;;;   - hatches with more than microscopic tilt (use FLATHATCH)
;;;   - vertical/nearly vertical XLINEs whose XY projection collapses
;;;
;;; Never touches POINT objects.
;;; Never selects 3D solids, surfaces, 3D faces, or meshes.
;;; Does NOT flatten geometry inside block definitions.
;;; One U undoes the completed FLATALL run.

;;; ---------- helpers ----------

;; Keep X and Y, set Z to 0.
(defun FA:z0 (p)
  (list (car p) (cadr p) 0.0))

;; Object normal (default 0,0,1).
(defun FA:nrm (ed)
  (cond
    ((cdr (assoc 210 ed)))
    ('(0.0 0.0 1.0))))

;; Exact vertical normal, keeping up/down direction.
(defun FA:nn (nrm)
  (if (minusp (caddr nrm))
    '(0.0 0.0 -1.0)
    '(0.0 0.0 1.0)))

;; T if the object faces almost straight up or down.
(defun FA:upright-p (nrm tol)
  (equal (abs (caddr nrm)) 1.0 tol))

;; Old OCS -> WCS -> World Z=0 -> exact vertical OCS.
(defun FA:ocs (p nrm)
  (FA:z0
    (trans
      (FA:z0 (trans p nrm 0))
      0
      (FA:nn nrm))))

;; Direction vector -> flat unit vector in the XY plane.
;; Existing spline-tangent helper; unchanged behavior.
(defun FA:unit2d (v / l)
  (setq l
    (distance
      '(0.0 0.0)
      (list (car v) (cadr v))))
  (if (> l 1e-12)
    (list (/ (car v) l) (/ (cadr v) l) 0.0)
    v))

;; Replace the normal with the exact vertical one.
(defun FA:setn (ed nrm)
  (if (assoc 210 ed)
    (subst
      (cons 210 (FA:nn nrm))
      (assoc 210 ed)
      ed)
    ed))

;; Set Z=0 on listed WCS point codes.
;; once=T fixes only the first occurrence of each listed code.
(defun FA:zero (ed codes once / done)
  (mapcar
    '(lambda (p)
       (if
         (and
           (member (car p) codes)
           (listp (cdr p))
           (= (length p) 4)
           (not (and once (member (car p) done))))
         (progn
           (setq done (cons (car p) done))
           (cons (car p) (FA:z0 (cdr p))))
         p))
    ed))

;; Convert listed OCS point codes with FA:ocs.
(defun FA:fixocs (ed codes nrm)
  (mapcar
    '(lambda (p)
       (if
         (and
           (member (car p) codes)
           (listp (cdr p))
           (= (length p) 4))
         (cons (car p) (FA:ocs (cdr p) nrm))
         p))
    ed))

;; T if TEXT / ATTRIB / ATTDEF uses alignment point code 11.
(defun FA:just-p (ed / typ vc)
  (setq typ (cdr (assoc 0 ed))
        vc
          (if (member typ '("ATTRIB" "ATTDEF"))
            74
            73))
  (or
    (/= 0 (cond ((cdr (assoc 72 ed))) (0)))
    (/= 0 (cond ((cdr (assoc vc ed))) (0)))))

;; T if INSERT is an associative array.
(defun FA:array-p (ed / r)
  (and
    (= (cdr (assoc 0 ed)) "INSERT")
    (setq r (member '(102 . "{ACAD_REACTORS") ed))
    (vl-some
      '(lambda (p)
         (and
           (= (car p) 330)
           (=
             (cdr (assoc 0 (entget (cdr p))))
             "ACDBASSOCDEPENDENCY")))
      (cdr r))))

;; Flatten 2D heavy-polyline vertices.
(defun FA:vtx (e nrm el / s sd)
  (setq s (entnext e))
  (while
    (and
      s
      (/= "SEQEND" (cdr (assoc 0 (setq sd (entget s))))))
    (entmod
      (mapcar
        '(lambda (p)
           (if (= (car p) 10)
             (cons 10
               (FA:ocs
                 (list (cadr p) (caddr p) el)
                 nrm))
             p))
        sd))
    (setq s (entnext s))))

;; Flatten block attributes.
(defun FA:att (e / s sd an)
  (setq s (entnext e))
  (while
    (and
      s
      (/= "SEQEND" (cdr (assoc 0 (setq sd (entget s))))))
    (setq an (FA:nrm sd))
    (if (FA:upright-p an 1e-8)
      (entmod
        (FA:setn
          (FA:fixocs
            sd
            (if (FA:just-p sd) '(10 11) '(10))
            an)
          an)))
    (setq s (entnext s))))

;; Flatten XLINE base point and direction.
;; Code 10 = WCS base point.
;; Code 11 = WCS unit direction vector.
;; Skip directions whose XY projection collapses.
(defun FA:flatx (e ed / bp dv lenxy new tilted)
  (setq bp (assoc 10 ed)
        dv (assoc 11 ed))
  (cond
    ((or (null bp) (null dv))
     "fail")
    (T
     (setq dv (cdr dv)
           lenxy
             (sqrt
               (+ (* (car dv) (car dv))
                  (* (cadr dv) (cadr dv)))))
     (if (<= lenxy 1e-12)
       "skip"
       (progn
         (setq tilted (/= (caddr dv) 0.0)
               new
                 (subst
                   (cons 10 (FA:z0 (cdr bp)))
                   bp
                   ed)
               new
                 (subst
                   (cons 11
                     (list
                       (/ (car dv) lenxy)
                       (/ (cadr dv) lenxy)
                       0.0))
                   (assoc 11 new)
                   new))
         (if (entmod new)
           (progn
             (entupd e)
             (if tilted "tilt" "ok"))
           "fail"))))))

;; Flatten one entity.
;; Returns "ok", "tilt", "array", "skip", or "fail".
(defun FA:flat1 (e tol / ed typ flg nrm el tilt)
  (setq ed   (entget e)
        typ  (cdr (assoc 0 ed))
        flg  (cond ((cdr (assoc 70 ed))) (0))
        nrm  (FA:nrm ed))
  (cond
    ;; Associative arrays remain untouched.
    ((FA:array-p ed)
     "array")

    ;; XLINE uses a WCS base point and direction.
    ((= typ "XLINE")
     (FA:flatx e ed))

    ;; 3D polyline (8), polygon mesh (16), polyface mesh (64).
    ((and (= typ "POLYLINE") (/= 0 (logand 88 flg)))
     "skip")

    ;; Non-planar spline: neither planar (8) nor linear (16).
    ((and (= typ "SPLINE") (= 0 (logand 24 flg)))
     "skip")

    ;; Clearly tilted objects remain untouched.
    ((and
       (/= typ "LINE")
       (not (FA:upright-p nrm tol)))
     "skip")

    ;; Hatch boundaries are not converted here.
    ((and
       (= typ "HATCH")
       (not (FA:upright-p nrm 1e-12)))
     "skip")

    (T
     (setq tilt
       (not (equal nrm (FA:nn nrm) 1e-14)))

     (setq ed
       (cond
         ((= typ "LINE")
          (FA:setn
            (FA:zero ed '(10 11) nil)
            nrm))

         ((= typ "LWPOLYLINE")
          (setq el
            (cond ((cdr (assoc 38 ed))) (0.0)))
          (FA:setn
            (mapcar
              '(lambda (p / q)
                 (cond
                   ((= (car p) 10)
                    (setq q
                      (FA:ocs
                        (list (cadr p) (caddr p) el)
                        nrm))
                    (list 10 (car q) (cadr q)))
                   ((= (car p) 38)
                    '(38 . 0.0))
                   (T p)))
              ed)
            nrm))

         ((= typ "POLYLINE")
          (setq el (cadddr (assoc 10 ed)))
          (FA:setn
            (FA:zero ed '(10) nil)
            nrm))

         ;; Spline control/fit points and start/end tangents.
         ((= typ "SPLINE")
          (FA:setn
            (mapcar
              '(lambda (p)
                 (cond
                   ((not
                      (and
                        (listp (cdr p))
                        (= (length p) 4)))
                    p)
                   ((member (car p) '(10 11))
                    (cons (car p) (FA:z0 (cdr p))))
                   ((member (car p) '(12 13))
                    (cons (car p) (FA:unit2d (cdr p))))
                   (T p)))
              ed)
            nrm))

         ((member typ '("SOLID" "TRACE"))
          (FA:setn
            (FA:fixocs ed '(10 11 12 13) nrm)
            nrm))

         ((member typ '("ARC" "CIRCLE" "INSERT"))
          (FA:setn
            (FA:fixocs ed '(10) nrm)
            nrm))

         ((member typ '("TEXT" "ATTDEF"))
          (FA:setn
            (FA:fixocs
              ed
              (if (FA:just-p ed) '(10 11) '(10))
              nrm)
            nrm))

         ((member typ '("ELLIPSE" "MTEXT"))
          (FA:setn
            (FA:zero ed '(10 11) nil)
            nrm))

         ((= typ "HATCH")
          (FA:setn
            (FA:zero ed '(10) T)
            nrm))

         (T ed)))

     (if (entmod ed)
       (progn
         (cond
           ((= typ "POLYLINE")
            (FA:vtx e nrm el))
           ((and
              (= typ "INSERT")
              (= 1 (cdr (assoc 66 ed))))
            (FA:att e)))
         (entupd e)
         (if tilt "tilt" "ok"))
       "fail"))))

;;; ---------- FLATALL ----------

(defun c:FLATALL (/ ss i r n tf skip arr fail)
  (command "_.UNDO" "_BE")
  (command "_.UCS" "_W")
  (setq n 0
        tf 0
        skip 0
        arr 0
        fail 0
        i 0)
  (if
    (setq ss
      (ssget "_X"
        '((0 . "LINE,XLINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,ELLIPSE,TEXT,MTEXT,ATTDEF,INSERT,HATCH,SOLID,TRACE,SPLINE")
          (410 . "Model"))))
    (repeat (sslength ss)
      (setq r (FA:flat1 (ssname ss i) 1e-8))
      (cond
        ((= r "ok")
         (setq n (1+ n)))
        ((= r "tilt")
         (setq n (1+ n)
               tf (1+ tf)))
        ((= r "array")
         (setq arr (1+ arr)))
        ((= r "skip")
         (setq skip (1+ skip)))
        (T
         (setq fail (1+ fail))))
      (setq i (1+ i))))
  (command "_.UNDO" "_E")
  (princ
    (strcat
      "\nFLATALL: " (itoa n)
      " flattened (" (itoa tf) " had tilt fixed)"
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

;; Extract hatch style, pattern settings, and definition lines.
;; This section starts at code 75 and ends before seed-point code 98.
(defun FH:pattern (ed / tail out)
  (setq tail (member (assoc 75 ed) ed))
  (while (and tail (/= 98 (caar tail)))
    (setq out (cons (car tail) out)
          tail (cdr tail)))
  (if tail (reverse out)))

;; Replace that section without changing the new hatch's boundaries.
(defun FH:splice (ed block / tail out)
  (setq tail ed)
  (while (and tail (/= 75 (caar tail)))
    (setq out (cons (car tail) out)
          tail (cdr tail)))
  (while (and tail (/= 98 (caar tail)))
    (setq tail (cdr tail)))
  (if tail
    (append (reverse out) block tail)))

;; Copy embedded pattern data, then read it back for verification.
(defun FH:copy-pattern (source target / sd td block new check)
  (setq sd    (entget source)
        td    (entget target)
        block (FH:pattern sd))
  (if (and block
           (assoc 2 sd)
           (assoc 70 sd)
           (setq new (FH:splice td block)))
    (progn
      (setq new
        (subst (assoc 2 sd) (assoc 2 new) new))
      (setq new
        (subst (assoc 70 sd) (assoc 70 new) new))
      (if (entmod new)
        (progn
          (entupd target)
          (setq check (entget target))
          (and
            (equal (assoc 2 sd) (assoc 2 check))
            (equal (assoc 70 sd) (assoc 70 check))
            (equal block (FH:pattern check) 1e-12)))))))

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
                 ;; Copy general properties first.
                 (command "_.MATCHPROP" h nh "")

                 ;; Explicitly copy and verify the embedded pattern.
                 (if (FH:copy-pattern h nh)
                   (progn
                     (command "_.DRAWORDER" nh "" "_U" h "")
                     (entdel h)
                     (FA:erase bnd)
                     (setq n (1+ n)))
                   (progn
                     ;; Reject the replacement; preserve the original.
                     (entdel nh)
                     (FA:erase bnd)
                     (setq fail (1+ fail))
                     (princ
                       (strcat
                         "\nOriginal hatch retained: pattern verification failed."
                         " Handle "
                         (cdr (assoc 5 hd)))))))
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

;; T if a MULTILEADER's plane or normals are tilted.
;; Plane axes (111, 112) must have Z = 0.
;; Unit-length code 11 vectors must be exactly flat (Z = 0) or exactly vertical.
(defun FA:mltilt-p (ed)
  (vl-some
    '(lambda (p / c v z)
       (setq c (car p) v (cdr p))
       (and (listp v) (= (length v) 3)
            (setq z (abs (caddr v)))
            (or (and (member c '(111 112)) (> z 1e-12))
                (and (= c 11)
                     (equal (distance '(0.0 0.0 0.0) v) 1.0 1e-6)
                     (> z 1e-12)
                     (not (equal z 1.0 1e-12))))))
    ed))

(defun c:FINDTILT (/ ss i e ed typ nrm out cnt a)
  (setq out (ssadd) i 0 cnt nil)
  (if (setq ss (ssget "_X" '((410 . "Model"))))
    (repeat (sslength ss)
      (setq e   (ssname ss i)
            ed  (entget e)
            typ (cdr (assoc 0 ed))
            nrm (assoc 210 ed))
      (if (or
            (and nrm
                 (not
                   (equal
                     (cdr nrm)
                     (FA:nn (cdr nrm))
                     1e-14)))
            (and (= "MULTILEADER" typ)
                 (FA:mltilt-p ed))
            (and (= "XLINE" typ)
                 (/= 0.0 (cadddr (assoc 11 ed)))))
        (progn
          (ssadd e out)
          ;; count by object type
          (if (setq a (assoc typ cnt))
            (setq cnt (subst (cons typ (1+ (cdr a))) a cnt))
            (setq cnt (cons (cons typ 1) cnt)))))
      (setq i (1+ i))))
  (sssetfirst nil out)
  (princ (strcat "\nFINDTILT: " (itoa (sslength out)) " tilted objects selected."))
  (foreach a (reverse cnt)
    (princ (strcat "\n  " (car a) ": " (itoa (cdr a)))))
  (princ))

;;; ---------- FINDNONZERO ----------

;;; Paste this section into FLATALL.lsp before its final load-message lines.
;;; Read-only diagnostic: selects model-space entities with nonzero World Z.
;;; Also always reports true 3D polylines, even when every vertex is at Z=0.
;;; Does not modify any object.

(defun FNZ:nz (x) (> (abs x) 1e-8))

(defun FNZ:nrm (ed)
  (cond ((cdr (assoc 210 ed))) ('(0.0 0.0 1.0))))

(defun FNZ:point3-p (p)
  (and (listp p) (= (length p) 3)))

(defun FNZ:wz-p (p)
  (and (FNZ:point3-p p) (FNZ:nz (caddr p))))

;; OCS point -> WCS; T when resulting World Z is nonzero.
(defun FNZ:ocsz-p (p nrm)
  (and (FNZ:point3-p p)
       (FNZ:nz (caddr (trans p nrm 0)))))

;; Check all occurrences of specified point codes as WCS points.
(defun FNZ:wcodes-p (ed codes)
  (vl-some
    '(lambda (p)
       (and (member (car p) codes)
            (FNZ:wz-p (cdr p))))
    ed))

;; Check all occurrences of specified point codes as OCS points.
(defun FNZ:ocodes-p (ed codes nrm)
  (vl-some
    '(lambda (p)
       (and (member (car p) codes)
            (FNZ:ocsz-p (cdr p) nrm)))
    ed))

;; T if INSERT is an associative array.
(defun FNZ:array-p (ed / r x xd)
  (and (= (cdr (assoc 0 ed)) "INSERT")
       (setq r (member '(102 . "{ACAD_REACTORS") ed))
       (vl-some
         '(lambda (p)
            (and (= (car p) 330)
                 (setq x (cdr p))
                 (setq xd (entget x))
                 (= (cdr (assoc 0 xd)) "ACDBASSOCDEPENDENCY")))
         (cdr r))))

;; Check old-style POLYLINE vertices through SEQEND.
(defun FNZ:poly-verts-nz-p (e / s sd hit)
  (setq s (entnext e) hit nil)
  (while (and s (not hit)
              (/= "SEQEND" (cdr (assoc 0 (setq sd (entget s))))))
    (if (and (= "VERTEX" (cdr (assoc 0 sd)))
             (FNZ:wz-p (cdr (assoc 10 sd))))
      (setq hit T))
    (setq s (entnext s)))
  hit)

;; MULTILEADER point data is nested and uses repeated point codes.
;; This intentionally checks point-like codes only, not unit direction vectors.
(defun FNZ:mleader-nz-p (ed)
  (vl-some
    '(lambda (p)
       (and (member (car p) '(10 12 15 110))
            (FNZ:wz-p (cdr p))))
    ed))

;; Return display type when entity should be reported; otherwise nil.
(defun FNZ:check (e / ed typ nrm flg elev)
  (setq ed   (entget e)
        typ  (cdr (assoc 0 ed))
        nrm  (FNZ:nrm ed)
        flg  (cond ((cdr (assoc 70 ed))) (0)))
  (cond
    ;; True 3D polyline: always report, by design.
    ((and (= typ "POLYLINE") (/= 0 (logand 8 flg))) "3D POLYLINE")
    ;; Mesh/polyface: report separately.
    ((and (= typ "POLYLINE") (/= 0 (logand 80 flg))) "MESH/POLYFACE")
    ((= typ "XLINE")
     (if (or (FNZ:wcodes-p ed '(10))
             (/= 0.0 (cadddr (assoc 11 ed))))
       "XLINE"))
    ((= typ "LINE")
     (if (FNZ:wcodes-p ed '(10 11)) "LINE"))
    ((= typ "LWPOLYLINE")
     (setq elev (cond ((cdr (assoc 38 ed))) (0.0)))
     (if
       (vl-some
         '(lambda (p)
            (and (= (car p) 10)
                 (FNZ:ocsz-p
                   (list (cadr p) (caddr p) elev)
                   nrm)))
         ed)
       "LWPOLYLINE"))
    ((= typ "POLYLINE")
     (if (or (FNZ:wcodes-p ed '(10)) (FNZ:poly-verts-nz-p e)) "POLYLINE"))
    ((= typ "POINT")
     (if (FNZ:wcodes-p ed '(10)) "POINT"))
    ((= typ "INSERT")
     (if (FNZ:ocodes-p ed '(10) nrm)
       (if (FNZ:array-p ed) "ARRAY" "INSERT")))
    ((member typ '("ARC" "CIRCLE"))
     (if (FNZ:ocodes-p ed '(10) nrm) typ))
    ((= typ "ELLIPSE")
     (if (FNZ:wcodes-p ed '(10)) "ELLIPSE"))
    ((= typ "SPLINE")
     (if (FNZ:wcodes-p ed '(10 11)) "SPLINE"))
    ((member typ '("SOLID" "TRACE"))
     (if (FNZ:ocodes-p ed '(10 11 12 13) nrm) typ))
    ((= typ "HATCH")
     (if (FNZ:ocodes-p ed '(10) nrm) "HATCH"))
    ((member typ '("TEXT" "ATTDEF"))
     (if (FNZ:ocodes-p ed '(10 11) nrm) typ))
    ((= typ "MTEXT")
     (if (FNZ:wcodes-p ed '(10)) "MTEXT"))
    ((wcmatch typ "*DIMENSION")
     (if (or (FNZ:wcodes-p ed '(10 13 14 15))
             (FNZ:ocodes-p ed '(11 12 16) nrm))
       "DIMENSION"))
    ((= typ "LEADER")
     (if (FNZ:wcodes-p ed '(10)) "LEADER"))
    ((= typ "MULTILEADER")
     (if (FNZ:mleader-nz-p ed) "MULTILEADER"))
    ((= typ "3DFACE")
     (if (FNZ:wcodes-p ed '(10 11 12 13)) "3DFACE"))
    ((= typ "REGION") "REGION")
    ((= typ "3DSOLID") "3DSOLID")
    (T nil)))

(defun c:FINDNONZERO (/ ss out cnt i e label a)
  (setq out (ssadd) cnt nil i 0)
  (if (setq ss (ssget "_X" '((410 . "Model"))))
    (repeat (sslength ss)
      (setq e (ssname ss i)
            label (FNZ:check e))
      (if label
        (progn
          (ssadd e out)
          (if (setq a (assoc label cnt))
            (setq cnt (subst (cons label (1+ (cdr a))) a cnt))
            (setq cnt (cons (cons label 1) cnt)))))
      (setq i (1+ i))))
  (sssetfirst nil out)
  (princ (strcat "\nFINDNONZERO: " (itoa (sslength out))
                 " nonzero/3D objects selected."))
  (foreach a (reverse cnt)
    (princ (strcat "\n  " (car a) ": " (itoa (cdr a)))))
  (princ))

(princ "\nFLATALL loaded. Commands: FLATALL, FLATHATCH, FINDTILT, FINDNONZERO.")
(princ)
