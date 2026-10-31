;;; ------------------------------------------------------------
;;; Command: CBC
;;; Copies selected object(s) from the current layout to all
;;; other layouts at the exact same coordinates.
;;; Usage: APPLOAD this file, open a Layout tab, type CBC.
;;; ------------------------------------------------------------
(defun c:CBC (/ ss doc layouts curLayout cnt objList i ent lay targetBlock obj)
  (vl-load-com)

  ;; Get current layout name
  (setq curLayout (getvar "CTAB"))

  ;; Make sure we are in a paper space layout
  (if (= curLayout "Model")
    (progn
      (princ "\nPlease run this command from a Layout tab, not the Model tab.")
      (princ)
    )
    (progn
      ;; Select objects
      (princ "\nSelect object(s) to copy to all layouts: ")
      (setq ss (ssget))

      (if ss
        (progn
          (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
          (setq layouts (vla-get-Layouts doc))
          (setq cnt 0)

          ;; Build a list of VLA objects from the selection set
          (setq objList '())
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq objList (cons (vlax-ename->vla-object ent) objList))
            (setq i (1+ i))
          )

          ;; Loop through all layouts
          (vlax-for lay layouts
            (if (and
                  (/= (strcase (vla-get-Name lay)) "MODEL")
                  (/= (strcase (vla-get-Name lay)) (strcase curLayout))
                )
              (progn
                (setq targetBlock (vla-get-Block lay))
                ;; Copy each object into the target layout
                (foreach obj objList
                  (vla-CopyObjects
                    doc
                    (vlax-make-variant
                      (vlax-safearray-fill
                        (vlax-make-safearray vlax-vbObject '(0 . 0))
                        (list obj)
                      )
                    )
                    targetBlock
                  )
                )
                (setq cnt (1+ cnt))
              )
            )
          )

          (princ (strcat "\nDone. Objects copied to " (itoa cnt) " layout(s)."))
        )
        (princ "\nNo objects selected.")
      )
    )
  )
  (princ)
)

(princ "\nType CBC to run the command.")
(princ)
