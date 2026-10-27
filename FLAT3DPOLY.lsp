;;; FLAT3DPOLY.lsp
;;; Intentional conversion of model-space 3D polylines into lightweight
;;; 2D polylines at World Z=0.
;;;
;;; Workflow:
;;;   1. Finds true 3D POLYLINE entities automatically.
;;;   2. Skips polygon meshes and polyface meshes.
;;;   3. Shows counts and requires Yes/No confirmation (default No).
;;;   4. Rebuilds each object as one LWPOLYLINE using the original World X/Y
;;;      vertex sequence; preserves open/closed status and common properties.
;;;   5. Deletes the original only after the replacement is created.
;;;   6. One U undoes the completed run.
;;;
;;; Limitations:
;;;   - Elevation data is intentionally discarded.
;;;   - 3D polyline segments are straight; the replacement remains segmented.
;;;   - Vertex-level XData or unusual custom data is not transferred.

(defun F3:get (code ed)
  (cdr (assoc code ed)))

(defun F3:add-if (code ed out / p)
  (if (setq p (assoc code ed))
    (append out (list p))
    out))

;; Common entity properties worth retaining on the replacement.
(defun F3:props (ed / out)
  (setq out nil)
  (foreach code '(8 6 48 60 62 370 390 420 430 440)
    (setq out (F3:add-if code ed out)))
  out)

;; Gather vertex World XY values through SEQEND.
(defun F3:vertices (e / s sd pts p)
  (setq s (entnext e) pts nil)
  (while (and s
              (/= "SEQEND" (cdr (assoc 0 (setq sd (entget s))))))
    (if (and (= "VERTEX" (cdr (assoc 0 sd)))
             (setq p (cdr (assoc 10 sd))))
      (setq pts (cons (list (car p) (cadr p)) pts)))
    (setq s (entnext s)))
  (reverse pts))

;; Convert one true 3D polyline. Returns T when replacement succeeds.
(defun F3:convert (e / ed flg pts data new)
  (setq ed   (entget e)
        flg  (cond ((F3:get 70 ed)) (0))
        pts  (F3:vertices e))
  (if (< (length pts) 2)
    nil
    (progn
      (setq data
        (append
          (list '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity"))
          (F3:props ed)
          (list '(100 . "AcDbPolyline")
                (cons 90 (length pts))
                (cons 70 (if (/= 0 (logand 1 flg)) 1 0))
                '(38 . 0.0)
                '(210 0.0 0.0 1.0))))
      (foreach p pts
        (setq data (append data (list (cons 10 p)))))
      (if (setq new (entmakex data))
        (progn
          (entdel e)
          (entupd new)
          T)
        nil))))

(defun c:FLAT3DPOLY (/ *error* olderr undo-open ss candidates i e ed flg
                        layers lay a ans ok fail)
  (setq olderr *error* undo-open nil)
  (defun *error* (msg)
    (if undo-open (command-s "_.UNDO" "_E"))
    (setq *error* olderr)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\nFLAT3DPOLY error: " msg)))
    (princ))

  (setq candidates (ssadd) layers nil i 0)
  (if (setq ss (ssget "_X" '((0 . "POLYLINE") (410 . "Model"))))
    (repeat (sslength ss)
      (setq e    (ssname ss i)
            ed   (entget e)
            flg  (cond ((F3:get 70 ed)) (0)))
      ;; Bit 8 = 3D polyline. Bits 16/64 = mesh/polyface, which are excluded.
      (if (and (/= 0 (logand 8 flg))
               (= 0 (logand 80 flg)))
        (progn
          (ssadd e candidates)
          (setq lay (cond ((F3:get 8 ed)) ("0")))
          (if (setq a (assoc lay layers))
            (setq layers (subst (cons lay (1+ (cdr a))) a layers))
            (setq layers (cons (cons lay 1) layers)))))
      (setq i (1+ i))))

  (princ (strcat "\nFLAT3DPOLY found " (itoa (sslength candidates))
                 " true 3D polylines."))
  (foreach a (reverse layers)
    (princ (strcat "\n  Layer " (car a) ": " (itoa (cdr a)))))

  (if (= 0 (sslength candidates))
    (princ "\nFLAT3DPOLY: nothing to convert.")
    (progn
      (initget "Yes No")
      (setq ans
        (getkword
          "\nReplace these with 2D lightweight polylines at World Z=0? [Yes/No] <No>: "))
      (if (= ans "Yes")
        (progn
          (command "_.UNDO" "_BE")
          (setq undo-open T i 0 ok 0 fail 0)
          (repeat (sslength candidates)
            (if (F3:convert (ssname candidates i))
              (setq ok (1+ ok))
              (setq fail (1+ fail)))
            (setq i (1+ i)))
          (command "_.UNDO" "_E")
          (setq undo-open nil)
          (princ (strcat "\nFLAT3DPOLY: " (itoa ok)
                         " converted, " (itoa fail)
                         " failed (locked layer or invalid geometry?).")))
        (princ "\nFLAT3DPOLY: canceled; no objects changed."))))

  (setq *error* olderr)
  (princ))

(princ "\nFLAT3DPOLY loaded. Type FLAT3DPOLY to run.")
(princ)
