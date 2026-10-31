;;; ------------------------------------------------------------
;;; YQPIPE - Draw 3D HDPE-style pipes, one layer per diameter
;;; Command: YQPIPE
;;; Units: diameter and radius are entered in drawing units (meters)
;;; ------------------------------------------------------------

(setq yq:palette '(1 2 3 4 5 6 30 40 50 90 130 150 190 210 240))

(defun yq:reset ()
  (setq yq:lay "piping between"
        yq:clr 6
        yq:dia "1.2"
        yq:rad "1.2"
        yq:lnk "1")
)

(if (not yq:lay) (yq:reset))

;;; ---------- DCL ----------
(defun yq:writedcl ( / f path)
  (setq path (vl-filename-mktemp "yqpipe.dcl"))
  (setq f (open path "w"))
  (foreach l
    '("yqpipe : dialog {"
      "  label = \"[YQArch]-Draw Pipes\";"
      "  : row {"
      "    : edit_box { key = \"lay\"; label = \"Pipe Layer:\"; edit_width = 20; }"
      "    : button { key = \"pick\"; label = \"Pick\"; width = 8; fixed_width = true; }"
      "  }"
      "  : row {"
      "    : text { key = \"clrtxt\"; label = \"Layer Color:\"; width = 20; }"
      "    : image_button { key = \"img\"; width = 12; height = 1.2; fixed_width = true; color = 0; }"
      "  }"
      "  : edit_box { key = \"dia\"; label = \"Pipe Diameter (m):\"; edit_width = 8; }"
      "  : edit_box { key = \"rad\"; label = \"Pipe Bending Radius (m):\"; edit_width = 8; }"
      "  : toggle { key = \"lnk\"; label = \"Link Vertexes\"; }"
      "  : button { key = \"reset\"; label = \"Reset Default Values\"; }"
      "  : errtile { width = 40; }"
      "  ok_cancel;"
      "}")
    (write-line l f))
  (close f)
  path
)

(defun yq:showclr ()
  (set_tile "clrtxt" (strcat "Layer Color:  " (itoa yq:clr)))
  (start_image "img")
  (fill_image 0 0 (dimx_tile "img") (dimy_tile "img") yq:clr)
  (end_image)
)

(defun yq:fill ()
  (set_tile "lay" yq:lay)
  (set_tile "dia" yq:dia)
  (set_tile "rad" yq:rad)
  (set_tile "lnk" yq:lnk)
  (yq:showclr)
)

(defun yq:save ()
  (setq yq:lay (get_tile "lay")
        yq:dia (get_tile "dia")
        yq:rad (get_tile "rad")
        yq:lnk (get_tile "lnk"))
)

(defun yq:accept ()
  (yq:save)
  (cond
    ((= (vl-string-trim " " yq:lay) "")
     (set_tile "error" "Layer name cannot be empty."))
    ((<= (atof yq:dia) 0.0)
     (set_tile "error" "Pipe diameter must be greater than zero."))
    ((< (atof yq:rad) 0.0)
     (set_tile "error" "Bending radius cannot be negative."))
    (T (done_dialog 1))
  )
)

(defun yq:dialog ( / dcl id what c)
  (setq dcl  (yq:writedcl)
        id   (load_dialog dcl)
        what 3)
  (while (= what 3)
    (if (not (new_dialog "yqpipe" id))
      (setq what 0)
      (progn
        (yq:fill)
        (action_tile "pick"   "(yq:save)(done_dialog 3)")
        (action_tile "reset"  "(yq:reset)(yq:fill)")
        (action_tile "accept" "(yq:accept)")
        (action_tile "cancel" "(done_dialog 0)")
        (setq what (start_dialog))
        (if (= what 3)
          (if (setq c (acad_colordlg yq:clr nil))
            (setq yq:clr c)))
      )
    )
  )
  (unload_dialog id)
  (vl-file-delete dcl)
  (= what 1)
)

;;; ---------- Layers ----------
(defun yq:layername (d)
  (strcat (vl-string-trim " " yq:lay) " - Dia " (itoa (fix (+ 0.5 (* d 1000.0)))))
)

(defun yq:usedcolors ( / e prefix res)
  (setq prefix (strcat (strcase (vl-string-trim " " yq:lay)) " - DIA ")
        e (tblnext "LAYER" T))
  (while e
    (if (wcmatch (strcase (cdr (assoc 2 e))) (strcat prefix "*"))
      (setq res (cons (abs (cdr (assoc 62 e))) res)))
    (setq e (tblnext "LAYER")))
  res
)

(defun yq:ensurelayer (name / used col)
  (if (not (tblsearch "LAYER" name))
    (progn
      (setq used (yq:usedcolors)
            col  yq:clr)
      (if (member col used)
        (setq col (car (vl-remove-if '(lambda (x) (member x used)) yq:palette))))
      (if (not col) (setq col yq:clr))
      (entmake (list '(0 . "LAYER")
                     '(100 . "AcDbSymbolTableRecord")
                     '(100 . "AcDbLayerTableRecord")
                     (cons 2 name)
                     '(70 . 0)
                     (cons 62 col)
                     '(6 . "Continuous")))
    )
  )
)

;;; ---------- Geometry ----------
(defun yq:makepath (pts lname)
  (entmakex
    (append
      (list '(0 . "LWPOLYLINE")
            '(100 . "AcDbEntity")
            '(100 . "AcDbPolyline")
            (cons 8 lname)
            (cons 90 (length pts))
            '(70 . 0)
            (cons 38 (caddr (car pts))))
      (mapcar '(lambda (p) (list 10 (car p) (cadr p))) pts)))
)

(defun yq:sweeppipe (pts d r fillet / path circ)
  (setq path (yq:makepath pts (getvar "CLAYER")))
  (if (and fillet (> (length pts) 2) (> r 0.0))
    (progn
      (setvar "FILLETRAD" r)
      (command "_.FILLET" "_P" path)))
  (command "_.CIRCLE" (car pts) "_D" d)
  (setq circ (entlast))
  (command "_.SWEEP" "_MO" "_SO" circ "" path)
)

;;; ---------- Main ----------
(defun c:YQPIPE ( / d r lname pts p oldlay oldosm oldecho oldfr link)
  (if (yq:dialog)
    (progn
      (setq d    (atof yq:dia)
            r    (atof yq:rad)
            link (= yq:lnk "1"))
      (while (not
               (setq p (getpoint "\nSpecify pipe start point: "))
               ) (princ))
      (setq pts (list p))
      (while (setq p (getpoint (last pts) "\nSpecify next point <Enter to finish>: "))
        (setq pts (append pts (list p)))
        (if (> (length pts) 1)
          (grdraw (nth (- (length pts) 2) pts) p 7)))
      (if (< (length pts) 2)
        (princ "\nAt least two points are required.")
        (progn
          (setq lname   (yq:layername d)
                oldlay  (getvar "CLAYER")
                oldosm  (getvar "OSMODE")
                oldecho (getvar "CMDECHO")
                oldfr   (getvar "FILLETRAD"))
          (command "_.UNDO" "_BE")
          (setvar "CMDECHO" 0)
          (yq:ensurelayer lname)
          (setvar "CLAYER" lname)
          (setvar "OSMODE" 0)
          (if link
            (yq:sweeppipe pts d r T)
            (progn
              (setq i 0)
              (while (< i (1- (length pts)))
                (yq:sweeppipe (list (nth i pts) (nth (1+ i) pts)) d r nil)
                (setq i (1+ i)))))
          (command "_.UNDO" "_E")
          (setvar "FILLETRAD" oldfr)
          (setvar "OSMODE" oldosm)
          (setvar "CLAYER" oldlay)
          (setvar "CMDECHO" oldecho)
          (redraw)
          (princ (strcat "\nPipe drawn on layer: " lname))
        )
      )
    )
  )
  (princ)
)

(princ "\nYQPIPE loaded. Type YQPIPE to start.")
(princ)