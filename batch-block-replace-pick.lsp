;;; ================================================================
;;; BATCH-BLOCK-REPLACE-PICK.LSP
;;; Like BATCH-BLOCK-REPLACE, but you SELECT the source blocks on
;;; screen instead of typing their names.
;;;
;;; The drawing you currently have open is the SOURCE. You select
;;; the block(s) you want to propagate, then pick target drawings.
;;; For each target: old references of those blocks are deleted,
;;; their definitions purged, and the selected source blocks copied
;;; in at their exact paper space location/scale/rotation.
;;; Targets are processed in the background via ObjectDBX.
;;; For AutoCAD / Civil 3D
;;; ================================================================
;;;
;;; COMMAND: BATCH-BLOCK-REPLACE-PICK
;;;
;;; ASSUMPTIONS:
;;;   - Old and new blocks share the same names
;;;   - Select the source blocks in paper space (the layout)
;;;   - Blocks live in paper space in the targets too
;;;
;;; WORKFLOW:
;;;   1. Open the SOURCE sheet (the one with correct blocks)
;;;   2. Run the command
;;;   3. Select the block(s) on screen, press Enter
;;;   4. Multi-select the TARGET drawings
;;;   5. Confirm — runs in the background
;;;
;;; IMPORTANT: back up targets; test on 2-3 sheets first.
;;; ================================================================


(vl-load-com)

(if (not *bbr-last-folder*)
  (setq *bbr-last-folder* ""))


;;; ================================================================
;;; UTILITIES
;;; ================================================================

(defun _BBP:trim (s)
  (vl-string-trim " \t\n\r" s))

(defun _BBP:trail-slash (p)
  (setq p (vl-string-translate "/" "\\" p))
  (if (and (> (strlen p) 0)
           (/= (substr p (strlen p)) "\\"))
    (strcat p "\\")
    p))

(defun _BBP:read-lines (filepath / fd line lines)
  (setq lines '())
  (if (setq fd (open filepath "r"))
    (progn
      (while (setq line (read-line fd))
        (setq lines (cons line lines)))
      (close fd)
      (reverse lines))
    nil))

(defun _BBP:filename (fullpath / dir)
  (setq dir (vl-filename-directory fullpath))
  (if (and dir (> (strlen dir) 0))
    (substr fullpath (+ (strlen dir) 2))
    fullpath))


;;; ================================================================
;;; FILE PICKER: PowerShell native multi-select dialog
;;; ================================================================
(defun _BBP:pick-files-powershell (initialDir / wsh cmd tmpOut
                                    lines paths exitCode)
  (setq tmpOut (strcat (vl-string-translate "/" "\\" (getenv "TEMP"))
                       "\\bbp_selected.txt"))
  (if (findfile tmpOut) (vl-file-delete tmpOut))

  (if (or (not initialDir) (= initialDir ""))
    (setq initialDir "")
    (setq initialDir (vl-string-translate "\\" "/" initialDir)))

  (setq cmd (strcat
    "powershell -ExecutionPolicy Bypass -WindowStyle Hidden -Command \""
    "Add-Type -AssemblyName System.Windows.Forms; "
    "$d = New-Object System.Windows.Forms.OpenFileDialog; "
    "$d.Title = 'Select TARGET drawings'; "
    "$d.Filter = 'DWG Files (*.dwg)|*.dwg|All Files (*.*)|*.*'; "
    "$d.Multiselect = $true; "
    (if (/= initialDir "")
      (strcat "$d.InitialDirectory = '" initialDir "'; ")
      "")
    "if ($d.ShowDialog() -eq 'OK') { "
    "$d.FileNames | Set-Content -Path '"
    (vl-string-translate "\\" "/" tmpOut)
    "' -Encoding ASCII "
    "}"
    "\""))

  (setq wsh (vlax-create-object "WScript.Shell"))
  (setq exitCode
    (vl-catch-all-apply
      'vlax-invoke-method
      (list wsh 'Run cmd 0 :vlax-true)))
  (vlax-release-object wsh)

  (if (vl-catch-all-error-p exitCode)
    nil
    (if (findfile tmpOut)
      (progn
        (setq lines (_BBP:read-lines tmpOut))
        (vl-file-delete tmpOut)
        (setq paths
          (vl-remove-if
            '(lambda (x) (or (= (_BBP:trim x) "")
                             (< (strlen (_BBP:trim x)) 4)))
            (mapcar '_BBP:trim lines)))
        (if paths paths nil))
      nil)))


;;; ================================================================
;;; FILE PICKER FALLBACK: getfiled loop
;;; ================================================================
(defun _BBP:pick-files-loop (initialDir / path paths count defPath)
  (princ "\n  Select TARGET DWG files one at a time.")
  (princ "\n  Press Cancel when done.\n")
  (setq paths '() count 0)
  (setq defPath
    (if (and initialDir (/= initialDir ""))
      (strcat initialDir "\\") ""))
  (while
    (setq path (getfiled "Select TARGET DWG — Cancel when done"
                         defPath "dwg" 0))
    (setq paths (cons path paths))
    (setq count (1+ count))
    (setq defPath (strcat (vl-filename-directory path) "\\"))
    (princ (strcat "\n  " (itoa count) ". " (_BBP:filename path))))
  (if paths (reverse paths) nil))


;;; ================================================================
;;; Process one target via ObjectDBX.
;;; srcDoc      = active document (source of the copied blocks)
;;; srcRefs     = list of selected VLA block references in srcDoc
;;; blockNamesUC = uppercase names to delete/purge in target
;;; Returns (deletedCount copiedCount) or nil on failure.
;;; ================================================================
(defun _BBP:process-target (srcDoc srcRefs blockNamesUC dbxVer
                            targetPath / acadObj tgtDbx
                            lay ent toDelete blocks blkDef
                            tgtPS arr i copied deleted bn)

  (setq acadObj (vlax-get-acad-object))
  (setq tgtDbx
    (vl-catch-all-apply
      'vla-GetInterfaceObject (list acadObj dbxVer)))

  (if (vl-catch-all-error-p tgtDbx)
    nil

    (progn
      (if (vl-catch-all-error-p
            (vl-catch-all-apply 'vla-Open (list tgtDbx targetPath)))
        (progn
          (vlax-release-object tgtDbx)
          nil)

        (progn
          (setq deleted 0 copied 0)

          ;; --- 1) Delete old block references (paper space) ---
          (setq toDelete '())
          (vlax-for lay (vla-get-Layouts tgtDbx)
            (if (/= (strcase (vla-get-Name lay)) "MODEL")
              (vlax-for ent (vla-get-Block lay)
                (if (and (= (vla-get-ObjectName ent) "AcDbBlockReference")
                         (member (strcase (vla-get-EffectiveName ent))
                                 blockNamesUC))
                  (setq toDelete (cons ent toDelete))))))
          (foreach ent toDelete
            (if (not (vl-catch-all-error-p
                       (vl-catch-all-apply 'vla-Delete (list ent))))
              (setq deleted (1+ deleted))))

          ;; --- 2) Purge old definitions ---
          (setq blocks (vla-get-Blocks tgtDbx))
          (foreach bn blockNamesUC
            (setq blkDef
              (vl-catch-all-apply 'vla-Item (list blocks bn)))
            (if (not (vl-catch-all-error-p blkDef))
              (vl-catch-all-apply 'vla-Delete (list blkDef))))

          ;; --- 3) Copy selected source blocks into target ---
          (setq tgtPS nil)
          (vlax-for lay (vla-get-Layouts tgtDbx)
            (if (and (not tgtPS)
                     (/= (strcase (vla-get-Name lay)) "MODEL"))
              (setq tgtPS (vla-get-Block lay))))

          (if (and tgtPS srcRefs)
            (progn
              (setq arr (vlax-make-safearray
                          vlax-vbObject
                          (cons 0 (1- (length srcRefs)))))
              (setq i 0)
              (foreach r srcRefs
                (vlax-safearray-put-element arr i r)
                (setq i (1+ i)))
              ;; CopyObjects from the ACTIVE document into the DBX target
              (if (not (vl-catch-all-error-p
                         (vl-catch-all-apply
                           'vla-CopyObjects
                           (list srcDoc arr tgtPS))))
                (setq copied (length srcRefs)))))

          ;; --- 4) Save and release ---
          (vl-catch-all-apply 'vla-SaveAs (list tgtDbx targetPath))
          (vlax-release-object tgtDbx)

          (list deleted copied))))))


;;; ================================================================
;;; COMMAND: BATCH-BLOCK-REPLACE-PICK
;;; ================================================================
(defun c:BATCH-BLOCK-REPLACE-PICK (/ *error* acadObj dbxVer
                                    srcDoc srcDocPath
                                    ss i ent vlaObj enm
                                    srcRefs blockNamesUC
                                    targetPaths
                                    f result deleted copied
                                    processed errors startTime elapsed)

  (defun *error* (msg)
    (if (/= msg "Function cancelled")
      (princ (strcat "\nError: " msg)))
    (princ))

  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq srcDoc  (vla-get-ActiveDocument acadObj))
  (setq dbxVer
    (strcat "ObjectDBX.AxDbDocument."
            (substr (getvar "ACADVER") 1 2)))

  (princ "\n")
  (princ "\n======================================================")
  (princ "\n   BATCH-BLOCK-REPLACE-PICK")
  (princ "\n======================================================")
  (princ "\n  Current drawing is the SOURCE.")

  ;; --------------------------------------------------------
  ;; Step 1: Select source blocks on screen
  ;; --------------------------------------------------------
  (princ "\n\n  Select the block(s) to replace, then press Enter.")
  (setq ss (ssget '((0 . "INSERT"))))

  (if (not ss)
    (progn (princ "\nNo blocks selected. Cancelled.") (exit)))

  ;; Collect VLA refs + unique uppercase names
  (setq srcRefs '() blockNamesUC '() i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq vlaObj (vlax-ename->vla-object ent))
    (setq srcRefs (cons vlaObj srcRefs))
    (setq enm (strcase (vla-get-EffectiveName vlaObj)))
    (if (not (member enm blockNamesUC))
      (setq blockNamesUC (cons enm blockNamesUC)))
    (setq i (1+ i)))
  (setq srcRefs (reverse srcRefs))

  (princ (strcat "\n  Selected " (itoa (length srcRefs))
                 " block(s), " (itoa (length blockNamesUC))
                 " unique name(s):"))
  (foreach bn blockNamesUC (princ (strcat "\n    - " bn)))

  ;; Remember source folder
  (setq srcDocPath (vla-get-FullName srcDoc))
  (if (and srcDocPath (/= srcDocPath ""))
    (setq *bbr-last-folder* (vl-filename-directory srcDocPath)))

  ;; --------------------------------------------------------
  ;; Step 2: Pick target drawings
  ;; --------------------------------------------------------
  (setq targetPaths (_BBP:pick-files-powershell *bbr-last-folder*))
  (if (not targetPaths)
    (progn
      (princ "\n  Multi-select unavailable, using single-file picker.")
      (setq targetPaths (_BBP:pick-files-loop *bbr-last-folder*))))

  (if (not targetPaths)
    (progn (princ "\nNo targets selected. Cancelled.") (exit)))

  ;; Exclude the source (active) drawing from targets — it can't be
  ;; saved by ObjectDBX while open in the editor anyway
  (if (and srcDocPath (/= srcDocPath ""))
    (setq targetPaths
      (vl-remove-if
        '(lambda (p) (= (strcase p) (strcase srcDocPath)))
        targetPaths)))

  (if (not targetPaths)
    (progn (princ "\nNo valid targets (source excluded). Cancelled.") (exit)))

  ;; --------------------------------------------------------
  ;; Confirm
  ;; --------------------------------------------------------
  (princ (strcat "\n\n  Source:  " (_BBP:filename srcDocPath)))
  (princ (strcat "\n  Blocks:  " (itoa (length srcRefs))))
  (princ (strcat "\n  Targets: " (itoa (length targetPaths))))
  (princ "\n  Action:  delete old -> purge -> copy from source")
  (princ "\n")
  (initget "Yes No")
  (if (= "No" (getkword "  Proceed? [Yes/No] <Yes>: "))
    (progn (princ "\nCancelled.") (exit)))

  ;; --------------------------------------------------------
  ;; Process targets
  ;; --------------------------------------------------------
  (setq processed 0 errors 0)
  (setq startTime (getvar "MILLISECS"))

  (foreach f targetPaths
    (princ (strcat "\n  " (_BBP:filename f)))
    (setq result
      (_BBP:process-target srcDoc srcRefs blockNamesUC dbxVer f))
    (if result
      (progn
        (setq deleted (car result) copied (cadr result))
        (setq processed (1+ processed))
        (princ (strcat " [del:" (itoa deleted)
                       " copied:" (itoa copied) "] ...OK")))
      (progn
        (setq errors (1+ errors))
        (princ " ...FAILED"))))

  ;; --------------------------------------------------------
  ;; Report
  ;; --------------------------------------------------------
  (setq elapsed (/ (- (getvar "MILLISECS") startTime) 1000.0))
  (princ "\n")
  (princ "\n======================================================")
  (princ "\n          BATCH-BLOCK-REPLACE-PICK COMPLETE")
  (princ "\n------------------------------------------------------")
  (princ (strcat "\n  Processed: " (itoa processed)))
  (princ (strcat "\n  Errors:    " (itoa errors)))
  (princ (strcat "\n  Time:      " (rtos elapsed 2 1) " sec"))
  (princ "\n======================================================")
  (princ "\n")
  (princ "\n  Open a couple of targets to confirm the blocks")
  (princ "\n  replaced correctly and sit in the right spot.")
  (princ)
  (princ))


;;; ================================================================
;;; Load
;;; ================================================================
(princ "\n")
(princ "\n  BATCH-BLOCK-REPLACE-PICK loaded.")
(princ "\n  Command: BATCH-BLOCK-REPLACE-PICK")
(princ "\n  (Open the source sheet, run it, select blocks on screen.)")
(princ "\n")
(princ)
