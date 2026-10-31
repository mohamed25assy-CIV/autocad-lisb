(defun c:SYA (/ ss i ent)
  (vl-load-com)

  ;; Ask user to select reinforcing bars only
  (setq ss (ssget '((0 . "RBCR_EN_BAR")))) 

  (if ss
    (progn
      (setq i 0)
      ;; Loop through selection
      (repeat (sslength ss)
        (setq ent (ssname ss i))

        ;; Run ASD bar symbol command for each bar
        (command "DEF_PULL" ent "")

        ;; Increment
        (setq i (+ 1 i))
      )
    )
    (prompt "\nNo reinforcing bars selected!")
  )
  (princ)
)