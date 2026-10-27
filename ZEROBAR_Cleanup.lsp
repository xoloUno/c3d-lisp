; ZEROBAR Cleanup
; Command: ZEROBAR
; Runs: PURGE Z, E, O, B, R; AUDIT Yes; PURGE B, R

(defun c:ZEROBAR (/ *error* oldcmdecho)
  (setq oldcmdecho (getvar "CMDECHO"))

  (defun *error* (msg)
    (setvar "CMDECHO" oldcmdecho)
    (if (and msg
             (not (member msg '("Function cancelled" "quit / exit abort"))))
      (princ (strcat "\nZEROBAR cleanup stopped: " msg))
    )
    (princ)
  )

  ; Leave command prompts visible so results can be reviewed with F2.
  (setvar "CMDECHO" 1)
  (princ "\nZEROBAR cleanup started...")

  ; ZERO: Zero-length geometry, Empty text, Orphaned data.
  (command "_.-PURGE" "Z")
  (command "_.-PURGE" "E")
  (command "_.-PURGE" "O")

  ; B and R: Blocks and Regapps.
  (command "_.-PURGE" "B" "*" "N")
  (command "_.-PURGE" "R" "*" "N")

  ; A: Audit and fix errors.
  (command "_.AUDIT" "Y")

  ; Final B and R pass.
  (command "_.-PURGE" "B" "*" "N")
  (command "_.-PURGE" "R" "*" "N")

  (setvar "CMDECHO" oldcmdecho)
  (princ "\nZEROBAR cleanup complete. Review with F2, then save the drawing.")
  (princ)
)

(princ "\nZEROBAR Cleanup loaded. Type ZEROBAR to run.")
(princ)
