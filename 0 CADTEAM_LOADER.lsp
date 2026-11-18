; CAD Team AutoLISP Loader
; Loaded by acaddoc.lsp for every opened or newly created drawing.

(defun _cadteam-load (filename / filepath result)
  (setq filepath (findfile filename))
  (cond
    ((null filepath)
      (princ (strcat "\nCAD Team Loader: NOT FOUND - " filename)))
    (T
      (setq result (vl-catch-all-apply 'load (list filepath)))
      (if (vl-catch-all-error-p result)
        (princ (strcat "\nCAD Team Loader: FAILED - " filename))
        (princ (strcat "\nCAD Team Loader: loaded " filename))
      )
    )
  )
)

(defun c:CADHELP (/)
  (textscr)
  (princ "\nCAD TEAM COMMANDS")
  (princ "\n-----------------")
  (princ "\nCADHELP       Show this command list.")
  (princ "\nBATCH-BLOCK-REPLACE-PICK  Replace selected paper-space blocks across target DWGs. Back up and test first.")
  (princ "\nCADSETCHECK   Report current CAD and drafting settings without changing them.")
  (princ "\nCADSETTINGS   Apply agreed performance, plotting, and DIMASSOC settings.")
  (princ "\nDRAFTSETTINGS Apply standardized OSNAP, polar, 3D OSNAP, and selection settings.")
  (princ "\nFINDNONZERO   Select and report model-space objects with nonzero Z or 3D geometry.")
  (princ "\nFINDTILT      Select and report tilted model-space objects by object type.")
  (princ "\nFLATALL       Flatten common 2D model-space objects to Z=0 and fix slight tilt.")
  (princ "\nFLATDIMS      Flatten dims, leaders, and mleaders to Z=0 and fix UCS grip errors.")
  (princ "\nFLATMORE      Flatten remaining strongly tilted lightweight polylines after review.")
  (princ "\nFLATHATCH     Rebuild tilted hatches flat at Z=0.")
  (princ "\nFLATPOINTS    Flatten reviewed points within a Z range; default cutoff is +/-5.0.")
  (princ "\nFLAT3DPOLY    Convert reviewed 3D polylines to lightweight 2D polylines at Z=0.")
  (princ "\nXRINDEX       Set INDEXCTL=3 in the active source XREF; save afterward.")
  (princ "\nZEROBAR       Clean drawing: ZERO, Blocks, Regapps, Audit, Blocks, Regapps.")
  (princ)
)

(_cadteam-load "ZEROBAR_Cleanup.lsp")
(_cadteam-load "CADSETTINGS_v4.lsp")
(_cadteam-load "batch-block-replace-pick.lsp")
(_cadteam-load "FLATDIMS.lsp")
(_cadteam-load "FLATALL.lsp")
(_cadteam-load "FLATMORE.lsp")
(_cadteam-load "FLAT3DPOLY.lsp")

(princ "\nCAD Team Loader complete. Type CADHELP for available commands.")
(princ)

; ---------------------------------------------------------------------------
; Usage logging
; A LISP reactor writes one CSV row per run of a whitelisted command, so the
; team can see how much time the tools save. Logging never interrupts a
; command: file errors skip the row silently.
; ---------------------------------------------------------------------------

(setq *CADLOG-Folder* "C:\\dev\\LISP\\Logs\\") ; TODO: change to synced SharePoint folder
(setq *CADLOG-Commands*
  '("BATCH-BLOCK-REPLACE-PICK" "CADHELP" "CADSETCHECK" "CADSETTINGS"
    "DRAFTSETTINGS" "FINDNONZERO" "FINDTILT" "FLAT3DPOLY" "FLATALL"
    "FLATDIMS" "FLATHATCH" "FLATMORE" "FLATPOINTS" "XRINDEX" "ZEROBAR"))

; Return the command name in an expression string such as "(C:FLATALL)",
; or nil if it isn't on the whitelist.
(defun CADLOG:CmdName (expr / name i)
  (if (= (type expr) 'STR)
    (progn
      (setq name (strcase (vl-string-left-trim "( " expr)))
      (if (setq i (vl-string-search " " name)) (setq name (substr name 1 i)))
      (if (setq i (vl-string-search ")" name)) (setq name (substr name 1 i)))
      (if (wcmatch name "C:*") (setq name (substr name 3)))
      (if (member name *CADLOG-Commands*) name)
    )
  )
)

; Create FOLDER and any missing parent folders.
(defun CADLOG:MkDirs (folder / i)
  (setq i 0)
  (while (setq i (vl-string-search "\\" folder (1+ i)))
    (vl-mkdir (substr folder 1 i))
  )
  (vl-mkdir folder)
)

; Append one row to this user's log for the current month. A new file gets
; the header row first. Returns quietly if the file can't be opened.
(defun CADLOG:WriteRow (cmd status dur / folder path new fh)
  (setq folder (vl-string-right-trim "\\" *CADLOG-Folder*)
        path   (strcat folder "\\TaskLog_" (getvar "LOGINNAME") "_"
                       (menucmd "M=$(edtime,$(getvar,date),YYYY-MO)") ".csv"))
  (if (not (vl-file-directory-p folder))
    (CADLOG:MkDirs folder)
  )
  (setq new (not (findfile path)))
  (if (setq fh (open path "a"))
    (progn
      (vl-catch-all-apply
        (function
          (lambda ()
            (if new
              (write-line "Timestamp,User,Command,Status,DurationSec,Drawing" fh)
            )
            (write-line
              (strcat (menucmd "M=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM:SS)")
                      "," (getvar "LOGINNAME")
                      "," cmd
                      "," status
                      "," (rtos dur 2 1)
                      ",\"" (getvar "DWGNAME") "\"")
              fh
            )
          )
        )
        nil
      )
      (close fh)
    )
  )
)

; Write the row for the run started in CADLOG:OnStart, then clear the start.
; Skips events with no matching start.
(defun CADLOG:Finish (status / cmd dur)
  (if (and *CADLOG-Cmd* *CADLOG-Start*)
    (progn
      (setq cmd            *CADLOG-Cmd*
            dur            (/ (- (getvar "MILLISECS") *CADLOG-Start*) 1000.0)
            *CADLOG-Cmd*   nil
            *CADLOG-Start* nil)
      (vl-catch-all-apply 'CADLOG:WriteRow (list cmd status dur))
    )
  )
  (princ)
)

; :vlr-lispWillStart - remember the command and start time. Every start
; resets both, so a stale start never pairs with a later evaluation.
(defun CADLOG:OnStart (reactor params)
  (setq *CADLOG-Cmd*   (CADLOG:CmdName (car params))
        *CADLOG-Start* (if *CADLOG-Cmd* (getvar "MILLISECS")))
  (princ)
)

; :vlr-lispEnded - log the run as completed.
(defun CADLOG:OnEnded (reactor params)
  (CADLOG:Finish "Completed")
)

; :vlr-lispCancelled - log the run as cancelled.
(defun CADLOG:OnCancelled (reactor params)
  (CADLOG:Finish "Cancelled")
)

; One reactor per drawing: LISP globals are per document, and the reactor
; itself is the flag, so reloading the loader doesn't add another.
(vl-load-com)
(if (not *CADLOG-Reactor*)
  (setq *CADLOG-Reactor*
    (vlr-lisp-reactor nil
      '((:vlr-lispWillStart . CADLOG:OnStart)
        (:vlr-lispEnded     . CADLOG:OnEnded)
        (:vlr-lispCancelled . CADLOG:OnCancelled))))
)
(princ)
