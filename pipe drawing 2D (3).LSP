;;; ============================================================
;;; ASPP - Draw 2D pipes (wide polyline) with centerline,
;;;        diameter text, flow arrows, weld/joint marks,
;;;        elbow fittings, reducers, tees and a BOQ table.
;;;        One layer/color per diameter.
;;; Commands:
;;;   ASPP     - draw pipes
;;;   ASPPFIT  - place a reducer or a tee
;;;   ASPPBOQ  - select drawn pipes/fittings -> one BOQ table
;;; Units   : diameter, radius, text height and spacings are
;;;           entered in drawing units (meters). Layer name uses
;;;           diameter x 1000.
;;; Modes   : Pick Points  - pick the pipe path point by point
;;;           Select Lines - convert existing LINE/POLYLINE
;;; Elbows  : each bend is queried for a fitting angle; the bend
;;;           radius is taken from the pipe type factor x diameter
;;;           (or from the dialog radius when it is > 0).
;;; Data    : every pipe and fitting carries XDATA (app "ASPP")
;;;           that ASPPBOQ reads back.
;;; ============================================================

(vl-load-com)

;;; ---------- Settings ----------
(setq yq:palette '(1 2 3 4 5 6 30 40 50 90 130 150 190 210 240))
(setq yq:txtprefix "%%c")   ; diameter symbol
(setq yq:txtfactor 0.3)     ; auto text height = factor x diameter
(setq yq:arwlen 0.6)        ; arrow length = factor x diameter
(setq yq:arwwid 1.2)        ; arrow head width = factor x diameter
(setq yq:weldlen 1.25)      ; weld mark length = factor x diameter
(setq yq:weldwid 0.06)      ; weld mark width  = factor x diameter
(setq yq:std '(90.0 45.0 22.5 11.25))  ; standard elbow angles (deg)
(setq yq:fitclr 30)         ; color of the "- Fitting" layer

;;; Pipe types:
;;; (name text-code default-pipe-length joint-word bend-radius-factor)
;;; bend radius = factor x diameter (centerline radius)
(setq yq:types
  '(("HDPE"         "HDPE"  6.0  "Weld"  1.5)
    ("Ductile Iron" "DI"    6.0  "Joint" 1.0)
    ("GRP"          "GRP"   6.0  "Joint" 1.5)
    ("Steel"        "STEEL" 12.0 "Weld"  1.5)
    ("PVC-U"        "PVC"   6.0  "Joint" 1.5)))

(setq yq:boqdata nil)
(setq yq:fitdata nil)
(setq yq:step nil)

(defun yq:reset ()
  (setq yq:lay  "piping between"
        yq:clr  6
        yq:typ  "0"
        yq:dia  "1.2"
        yq:rad  "0"
        yq:txh  "0.35"
        yq:spc  "50"
        yq:asp  "20"
        yq:jsp  "6.0"
        yq:jnt  "1"
        yq:arw  "1"
        yq:rev  "0"
        yq:lnk  "1"
        yq:ask  "1"
        yq:dboq "0"
        yq:mode "1"
        yq:ers  "0")
)

(defun yq:fitreset ()
  (setq yq:ftype "1"
        yq:ftyp  "0"
        yq:fd1   "1.2"
        yq:fd2   "0.8"
        yq:flen  "0"
        yq:fjn   "1"
        yq:flb   "1")
)

(if (not yq:lay)   (yq:reset))
(if (not yq:ftype) (yq:fitreset))
(if (not yq:txh)   (setq yq:txh "0.35"))
(if (not yq:spc)   (setq yq:spc "50"))
(if (not yq:asp)   (setq yq:asp "20"))
(if (not yq:arw)   (setq yq:arw "1"))
(if (not yq:rev)   (setq yq:rev "0"))
(if (not yq:typ)   (setq yq:typ "0"))
(if (not yq:jsp)   (setq yq:jsp "6.0"))
(if (not yq:jnt)   (setq yq:jnt "1"))
(if (not yq:ask)   (setq yq:ask "1"))
(if (not yq:dboq)  (setq yq:dboq "0"))
(if (not yq:mode)  (setq yq:mode "1"))
(if (not yq:ers)   (setq yq:ers "0"))

;;; ---------- DCL : main pipe dialog ----------
(defun yq:writedcl ( / f path)
  (setq path (vl-filename-mktemp "yqpipe.dcl"))
  (setq f (open path "w"))
  (foreach l
    '("yqpipe : dialog {"
      "  label = \"ASSY PIPE\";"
      "  : row {"
      "    : edit_box { key = \"lay\"; label = \"Pipe Layer:\"; edit_width = 20; }"
      "    : button { key = \"pick\"; label = \"Pick\"; width = 8; fixed_width = true; }"
      "  }"
      "  : row {"
      "    : text { key = \"clrtxt\"; label = \"Layer Color:\"; width = 20; }"
      "    : image_button { key = \"img\"; width = 12; height = 1.2; fixed_width = true; color = 0; }"
      "  }"
      "  : popup_list { key = \"typ\"; label = \"Pipe Type:\"; edit_width = 20; }"
      "  : edit_box { key = \"dia\"; label = \"Pipe Diameter (m):\"; edit_width = 8; }"
      "  : edit_box { key = \"rad\"; label = \"Pipe Bending Radius (m, 0 = auto from diameter):\"; edit_width = 8; }"
      "  : edit_box { key = \"txh\"; label = \"Diameter Text Height (m, 0 = auto):\"; edit_width = 8; }"
      "  : edit_box { key = \"spc\"; label = \"Text Spacing (m, 0 = one text):\"; edit_width = 8; }"
      "  : edit_box { key = \"asp\"; label = \"Arrow Spacing (m):\"; edit_width = 8; }"
      "  : edit_box { key = \"jsp\"; label = \"Joint Spacing (m, 0 = elbows only):\"; edit_width = 8; }"
      "  : row {"
      "    : toggle { key = \"jnt\"; label = \"Weld / Joint Marks\"; }"
      "    : toggle { key = \"arw\"; label = \"Flow Arrows\"; }"
      "    : toggle { key = \"rev\"; label = \"Reverse Flow\"; }"
      "  }"
      "  : row {"
      "    : toggle { key = \"lnk\"; label = \"Link Vertexes\"; }"
      "    : toggle { key = \"ask\"; label = \"Ask Elbow Angles\"; }"
      "    : toggle { key = \"boq\"; label = \"BOQ Table (this run)\"; }"
      "  }"
      "  : boxed_radio_row {"
      "    label = \"Draw Mode\";"
      "    : radio_button { key = \"m1\"; label = \"Pick Points\"; }"
      "    : radio_button { key = \"m2\"; label = \"Select Lines\"; }"
      "  }"
      "  : toggle { key = \"ers\"; label = \"Erase Source Lines (Select Lines mode)\"; }"
      "  : button { key = \"reset\"; label = \"Reset Default Values\"; }"
      "  : errtile { width = 56; }"
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

(defun yq:fill ( / t1)
  (start_list "typ")
  (foreach t1 yq:types (add_list (car t1)))
  (end_list)
  (set_tile "typ" yq:typ)
  (set_tile "lay" yq:lay)
  (set_tile "dia" yq:dia)
  (set_tile "rad" yq:rad)
  (set_tile "txh" yq:txh)
  (set_tile "spc" yq:spc)
  (set_tile "asp" yq:asp)
  (set_tile "jsp" yq:jsp)
  (set_tile "jnt" yq:jnt)
  (set_tile "arw" yq:arw)
  (set_tile "rev" yq:rev)
  (set_tile "lnk" yq:lnk)
  (set_tile "ask" yq:ask)
  (set_tile "boq" yq:dboq)
  (set_tile "ers" yq:ers)
  (set_tile "m1" (if (= yq:mode "1") "1" "0"))
  (set_tile "m2" (if (= yq:mode "2") "1" "0"))
  (mode_tile "ers" (if (= yq:mode "2") 0 1))
  (yq:showclr)
)

(defun yq:save ()
  (setq yq:lay  (get_tile "lay")
        yq:typ  (get_tile "typ")
        yq:dia  (get_tile "dia")
        yq:rad  (get_tile "rad")
        yq:txh  (get_tile "txh")
        yq:spc  (get_tile "spc")
        yq:asp  (get_tile "asp")
        yq:jsp  (get_tile "jsp")
        yq:jnt  (get_tile "jnt")
        yq:arw  (get_tile "arw")
        yq:rev  (get_tile "rev")
        yq:lnk  (get_tile "lnk")
        yq:ask  (get_tile "ask")
        yq:dboq (get_tile "boq")
        yq:ers  (get_tile "ers"))
)

;;; Pipe type changed: load its default pipe length
(defun yq:typchg (v)
  (setq yq:typ v)
  (set_tile "jsp" (rtos (nth 2 (nth (atoi v) yq:types)) 2 1))
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
    ((< (atof yq:txh) 0.0)
     (set_tile "error" "Text height cannot be negative."))
    ((< (atof yq:spc) 0.0)
     (set_tile "error" "Text spacing cannot be negative."))
    ((and (= yq:arw "1") (<= (atof yq:asp) 0.0))
     (set_tile "error" "Arrow spacing must be greater than zero."))
    ((< (atof yq:jsp) 0.0)
     (set_tile "error" "Joint spacing cannot be negative."))
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
        (action_tile "typ"    "(yq:typchg $value)")
        (action_tile "m1"     "(setq yq:mode \"1\")(mode_tile \"ers\" 1)")
        (action_tile "m2"     "(setq yq:mode \"2\")(mode_tile \"ers\" 0)")
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

;;; ---------- DCL : fitting dialog ----------
(defun yq:writefitdcl ( / f path)
  (setq path (vl-filename-mktemp "yqfit.dcl"))
  (setq f (open path "w"))
  (foreach l
    '("yqfit : dialog {"
      "  label = \"ASSY PIPE - FITTINGS\";"
      "  : boxed_radio_row {"
      "    label = \"Fitting Type\";"
      "    : radio_button { key = \"f1\"; label = \"Reducer\"; }"
      "    : radio_button { key = \"f2\"; label = \"Tee\"; }"
      "  }"
      "  : text { key = \"hint\"; width = 54; }"
      "  : popup_list { key = \"ftyp\"; label = \"Pipe Type:\"; edit_width = 20; }"
      "  : edit_box { key = \"d1\"; label = \"Diameter 1 (m):\"; edit_width = 8; }"
      "  : edit_box { key = \"d2\"; label = \"Diameter 2 (m):\"; edit_width = 8; }"
      "  : edit_box { key = \"flen\"; label = \"Fitting Length (m, 0 = auto):\"; edit_width = 8; }"
      "  : row {"
      "    : toggle { key = \"fjn\"; label = \"Weld / Joint Marks\"; }"
      "    : toggle { key = \"flb\"; label = \"Fitting Label\"; }"
      "  }"
      "  : errtile { width = 54; }"
      "  ok_cancel;"
      "}")
    (write-line l f))
  (close f)
  path
)

(defun yq:fithint ()
  (set_tile "hint"
    (if (= yq:ftype "1")
      "Reducer: from Diameter 1 (start) to Diameter 2 (end)"
      "Tee: Diameter 1 = main run, Diameter 2 = branch"))
)

(defun yq:fitfill ( / t1)
  (start_list "ftyp")
  (foreach t1 yq:types (add_list (car t1)))
  (end_list)
  (set_tile "ftyp" yq:ftyp)
  (set_tile "d1" yq:fd1)
  (set_tile "d2" yq:fd2)
  (set_tile "flen" yq:flen)
  (set_tile "fjn" yq:fjn)
  (set_tile "flb" yq:flb)
  (set_tile "f1" (if (= yq:ftype "1") "1" "0"))
  (set_tile "f2" (if (= yq:ftype "2") "1" "0"))
  (yq:fithint)
)

(defun yq:fitsave ()
  (setq yq:ftyp (get_tile "ftyp")
        yq:fd1  (get_tile "d1")
        yq:fd2  (get_tile "d2")
        yq:flen (get_tile "flen")
        yq:fjn  (get_tile "fjn")
        yq:flb  (get_tile "flb"))
)

(defun yq:fitaccept ()
  (yq:fitsave)
  (cond
    ((<= (atof yq:fd1) 0.0)
     (set_tile "error" "Diameter 1 must be greater than zero."))
    ((<= (atof yq:fd2) 0.0)
     (set_tile "error" "Diameter 2 must be greater than zero."))
    ((and (= yq:ftype "1") (equal (atof yq:fd1) (atof yq:fd2) 1e-9))
     (set_tile "error" "Reducer diameters must be different."))
    ((< (atof yq:flen) 0.0)
     (set_tile "error" "Fitting length cannot be negative."))
    (T (done_dialog 1))
  )
)

(defun yq:fitdialog ( / dcl id what)
  (setq dcl  (yq:writefitdcl)
        id   (load_dialog dcl)
        what 0)
  (if (new_dialog "yqfit" id)
    (progn
      (yq:fitfill)
      (action_tile "f1" "(setq yq:ftype \"1\")(yq:fithint)")
      (action_tile "f2" "(setq yq:ftype \"2\")(yq:fithint)")
      (action_tile "accept" "(yq:fitaccept)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq what (start_dialog))))
  (unload_dialog id)
  (vl-file-delete dcl)
  (= what 1)
)

;;; ---------- Layers ----------
(defun yq:mm (d)
  (itoa (fix (+ 0.5 (* d 1000.0))))
)

(defun yq:baselay ()
  (vl-string-trim " " yq:lay)
)

(defun yq:layername (d)
  (strcat (yq:baselay) " - Dia " (yq:mm d))
)

(defun yq:usedcolors ( / e prefix res)
  (setq prefix (strcat (strcase (yq:baselay)) " - DIA ")
        e (tblnext "LAYER" T))
  (while e
    (if (wcmatch (strcase (cdr (assoc 2 e))) (strcat prefix "*"))
      (setq res (cons (abs (cdr (assoc 62 e))) res)))
    (setq e (tblnext "LAYER")))
  res
)

(defun yq:mklayer (name col ltype)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name)
                   '(70 . 0)
                   (cons 62 col)
                   (cons 6 ltype))))
)

;;; Pipe layer: new diameter gets a color not used by other diameters
(defun yq:ensurelayer (name / used col)
  (if (not (tblsearch "LAYER" name))
    (progn
      (setq used (yq:usedcolors)
            col  yq:clr)
      (if (member col used)
        (setq col (car (vl-remove-if '(lambda (x) (member x used)) yq:palette))))
      (if (not col) (setq col yq:clr))
      (yq:mklayer name col "Continuous")
    )
  )
)

;;; Load CENTER linetype if needed; returns the name to use
(defun yq:ltype ( / oldfd oldecho lfile)
  (if (not (tblsearch "LTYPE" "CENTER"))
    (progn
      (setq oldfd   (getvar "FILEDIA")
            oldecho (getvar "CMDECHO")
            lfile   (if (= (getvar "MEASUREMENT") 1) "acadiso.lin" "acad.lin"))
      (setvar "FILEDIA" 0)
      (setvar "CMDECHO" 0)
      (vl-catch-all-apply
        '(lambda () (command "_.-LINETYPE" "_L" "CENTER" lfile "")))
      (setvar "FILEDIA" oldfd)
      (setvar "CMDECHO" oldecho)
    )
  )
  (if (tblsearch "LTYPE" "CENTER") "CENTER" "Continuous")
)

(defun yq:ensureaux (jword / lt)
  (setq lt (yq:ltype))
  (yq:mklayer (strcat (yq:baselay) " - Centerline") 7 lt)
  (yq:mklayer (strcat (yq:baselay) " - Text") 7 "Continuous")
  (yq:mklayer (strcat (yq:baselay) " - Flow Arrow") 7 "Continuous")
  (yq:mklayer (strcat (yq:baselay) " - " jword) 7 "Continuous")
  (yq:mklayer (strcat (yq:baselay) " - Elbow") 7 "Continuous")
  (yq:mklayer (strcat (yq:baselay) " - BOQ") 7 "Continuous")
  (yq:mklayer (strcat (yq:baselay) " - Fitting") yq:fitclr "Continuous")
  lt
)

;;; ---------- XDATA tagging ----------
(defun yq:regapp ()
  (if (not (tblsearch "APPID" "ASPP"))
    (regapp "ASPP"))
)

(defun yq:tag (en xd / ed)
  (yq:regapp)
  (setq ed (entget en))
  (entmod (append ed (list (list -3 (cons "ASPP" xd)))))
)

;;; ---------- Math helpers ----------
(defun yq:d2 (a b)
  (distance (list (car a) (cadr a)) (list (car b) (cadr b)))
)

(defun yq:acos (x)
  (setq x (max -1.0 (min 1.0 x)))
  (atan (sqrt (- 1.0 (* x x))) x)
)

(defun yq:tan (x) (/ (sin x) (cos x)))

(defun yq:cf (k cfg) (cdr (assoc k cfg)))

(defun yq:round2 (x)
  (/ (fix (+ 0.5 (* x 100.0))) 100.0)
)

;;; Point at distance dist from p in direction ang (keeps Z)
(defun yq:padd (p ang dist / z)
  (setq z (if (caddr p) (caddr p) 0.0))
  (list (+ (car p) (* (cos ang) dist))
        (+ (cadr p) (* (sin ang) dist))
        z)
)

;;; Angle normalised so text is never upside down
(defun yq:readang (ang / a)
  (setq a ang)
  (while (< a 0.0) (setq a (+ a (* 2.0 pi))))
  (while (>= a (* 2.0 pi)) (setq a (- a (* 2.0 pi))))
  (if (and (> a (+ (/ pi 2.0) 1e-4)) (< a (+ (* 1.5 pi) 1e-4)))
    (- a pi)
    a)
)

;;; Angle to text: 90 -> "90", 22.5 -> "22.5", 11.25 -> "11.25"
(defun yq:angstr (a / s)
  (if (equal a (fix a) 1e-6)
    (itoa (fix a))
    (progn
      (setq s (rtos a 2 2))
      (if (= (substr s (strlen s)) "0")
        (substr s 1 (1- (strlen s)))
        s)))
)

(defun yq:drop (n lst)
  (while (and lst (> n 0))
    (setq lst (cdr lst)
          n   (1- n)))
  lst
)

;;; ---------- Geometry ----------
;;; rlist = bend radius for every interior vertex (0 = sharp corner).
;;; Returns list of (point bulge) with fillet arcs at bent vertices.
(defun yq:buildverts (pts rlist / res i a b c l1 l2 u1 u2 th tl mx cr bl p1 p2 r)
  (setq res (list (list (car pts) 0.0))
        i   1)
  (while (< i (1- (length pts)))
    (setq a  (nth (1- i) pts)
          b  (nth i pts)
          c  (nth (1+ i) pts)
          l1 (yq:d2 a b)
          l2 (yq:d2 b c)
          r  (if (nth (1- i) rlist) (nth (1- i) rlist) 0.0))
    (if (or (< l1 1e-9) (< l2 1e-9))
      (setq res (cons (list b 0.0) res))
      (progn
        (setq u1 (list (/ (- (car a) (car b)) l1) (/ (- (cadr a) (cadr b)) l1))
              u2 (list (/ (- (car c) (car b)) l2) (/ (- (cadr c) (cadr b)) l2))
              th (yq:acos (+ (* (car u1) (car u2)) (* (cadr u1) (cadr u2)))))
        (if (or (<= r 0.0) (< th 0.001) (> th (- pi 0.001)))
          (setq res (cons (list b 0.0) res))
          (progn
            (setq tl (/ r (yq:tan (/ th 2.0)))
                  mx (/ (min l1 l2) 2.0))
            (if (> tl mx) (setq tl mx))
            (setq p1 (list (+ (car b) (* (car u1) tl)) (+ (cadr b) (* (cadr u1) tl)) (caddr b))
                  p2 (list (+ (car b) (* (car u2) tl)) (+ (cadr b) (* (cadr u2) tl)) (caddr b))
                  cr (- (* (- (car b) (car a)) (- (cadr c) (cadr b)))
                        (* (- (cadr b) (cadr a)) (- (car c) (car b))))
                  bl (yq:tan (/ (- pi th) 4.0)))
            (if (< cr 0.0) (setq bl (- bl)))
            (setq res (cons (list p2 0.0) (cons (list p1 bl) res)))
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (reverse (cons (list (last pts) 0.0) res))
)

;;; width = constant width (pipe diameter or 0 for centerline)
(defun yq:makepoly (verts width lname ltype ltscale / extra)
  (setq extra (if ltype (list (cons 6 ltype) (cons 48 ltscale)) nil))
  (entmakex
    (append
      (list '(0 . "LWPOLYLINE")
            '(100 . "AcDbEntity")
            (cons 8 lname))
      extra
      (list '(100 . "AcDbPolyline")
            (cons 90 (length verts))
            '(70 . 0)
            (cons 43 width)
            (cons 38 (caddr (car (car verts)))))
      (apply 'append
        (mapcar '(lambda (v)
                   (list (list 10 (car (car v)) (cadr (car v)))
                         (cons 42 (cadr v))))
                verts))))
)

;;; Two-vertex wide polyline tapering from width w1 (at a) to w2 (at b)
(defun yq:taper (a b w1 w2 lname / z)
  (setq z (if (caddr a) (caddr a) 0.0))
  (entmakex
    (list '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          (cons 8 lname)
          '(100 . "AcDbPolyline")
          '(90 . 2)
          '(70 . 0)
          (cons 38 z)
          (cons 10 (list (car a) (cadr a))) (cons 40 w1) (cons 41 w2) (cons 42 0.0)
          (cons 10 (list (car b) (cadr b))) (cons 40 0.0) (cons 41 0.0) (cons 42 0.0)))
)

;;; ---------- Elbow planning (asks the user for each bend) ----------
;;; Deflection angle (deg) at vertex b between segments a-b and b-c
(defun yq:defl (a b c / a1 a2)
  (setq a1 (atan (- (cadr b) (cadr a)) (- (car b) (car a)))
        a2 (atan (- (cadr c) (cadr b)) (- (car c) (car b))))
  (* (yq:angdiff a1 a2) (/ 180.0 pi))
)

;;; Nearest standard fitting angle within 3 deg, or nil
(defun yq:nearstd (defl / best bd s dd)
  (setq best nil bd 3.0)
  (foreach s yq:std
    (setq dd (abs (- s defl)))
    (if (<= dd bd) (setq bd dd best s)))
  best
)

;;; Red cross to show which vertex is being asked about
(defun yq:mark (p s)
  (grdraw (list (- (car p) s) (cadr p)) (list (+ (car p) s) (cadr p)) 1)
  (grdraw (list (car p) (- (cadr p) s)) (list (car p) (+ (cadr p) s)) 1)
)

;;; Returns one entry per interior vertex: (radius fitting-angle vertex)
;;; radius 0 = sharp corner / no elbow
(defun yq:planelbows (pts cfg / d ask i n a b c defl std def ang rad res s)
  (setq yq:step "asking elbows")
  (setq d   (yq:cf 'D cfg)
        ask (yq:cf 'ASK cfg)
        n   0
        i   1
        res nil)
  (while (< i (1- (length pts)))
    (setq a    (nth (1- i) pts)
          b    (nth i pts)
          c    (nth (1+ i) pts)
          defl (yq:defl a b c))
    (if (< defl 0.5)
      (setq res (cons (list 0.0 0.0 b) res))
      (progn
        (setq n   (1+ n)
              std (yq:nearstd defl)
              def (if std std (yq:round2 defl))
              ang def)
        (if ask
          (progn
            (yq:mark b (* 1.5 d))
            (initget 6 "Actual None")
            (setq s
              (getreal
                (strcat "\nElbow " (itoa n)
                        " (actual deflection " (rtos defl 2 1)
                        " deg) - enter fitting angle (90/45/22.5/11.25) or [Actual/None] <"
                        (yq:angstr def) ">: ")))
            (redraw)
            (cond
              ((null s) (setq ang def))
              ((= (type s) 'STR)
               (setq ang (if (= s "None") 0.0 (yq:round2 defl))))
              (T (setq ang (yq:round2 s))))))
        (setq rad
          (if (> ang 0.0)
            (if (> (yq:cf 'RMAN cfg) 0.0)
              (yq:cf 'RMAN cfg)
              (* (yq:cf 'RFAC cfg) d))
            0.0))
        (if (and (> ang 0.0) (> (abs (- ang defl)) 1.0))
          (princ (strcat "\n  Note: drawn geometry uses the actual deflection ("
                         (rtos defl 2 1) " deg); fitting recorded as "
                         (yq:angstr ang) " deg.")))
        (setq res (cons (list rad ang b) res))))
    (setq i (1+ i)))
  (reverse res)
)

;;; ---------- Curve helpers (centerline) ----------
(defun yq:curvelen (ent / e)
  (setq e (vlax-curve-getEndParam ent))
  (if e (vlax-curve-getDistAtParam ent e) 0.0)
)

;;; Point at distance s (clamped to the curve)
(defun yq:ptat (ent s / len)
  (setq len (yq:curvelen ent)
        s   (max 0.0 (min len s)))
  (vlax-curve-getPointAtDist ent s)
)

;;; Tangent angle of the curve at distance s (drawing direction)
(defun yq:angat (ent s / len p dv)
  (setq len (yq:curvelen ent)
        s   (max 0.0 (min len s))
        p   (vlax-curve-getParamAtDist ent s)
        dv  (if p (vlax-curve-getFirstDeriv ent p)))
  (if dv (atan (cadr dv) (car dv)) 0.0)
)

(defun yq:angdiff (a b / d)
  (setq d (abs (- a b)))
  (while (> d (* 2.0 pi)) (setq d (- d (* 2.0 pi))))
  (if (> d pi) (- (* 2.0 pi) d) d)
)

;;; T if an item of length tl centered at distance s lies on a straight part
(defun yq:straight (ent s tl / len s1 s2 a1 a2 am)
  (setq len (yq:curvelen ent)
        s1  (- s (/ tl 2.0))
        s2  (+ s (/ tl 2.0)))
  (if (or (< s1 0.0) (> s2 len))
    nil
    (progn
      (setq a1 (yq:angat ent s1)
            a2 (yq:angat ent s2)
            am (yq:angat ent s))
      (and (< (yq:angdiff a1 a2) 0.02)
           (< (yq:angdiff a1 am) 0.02)))
  )
)

;;; avoid = list of (distance . half-width) of items already placed.
;;; T if an item of length tl centered at s overlaps one of them
(defun yq:clash (s tl avoid / hit gap a)
  (setq hit nil
        gap (* 0.3 tl))
  (foreach a avoid
    (if (< (abs (- s (car a))) (+ (cdr a) (/ tl 2.0) gap))
      (setq hit T)))
  hit
)

(defun yq:good (ent s tl avoid)
  (and (yq:straight ent s tl)
       (not (yq:clash s tl avoid)))
)

;;; Nearest free straight spot to s0 (within +/- half a step), or nil
(defun yq:findspot (ent s0 tl step avoid / k res off)
  (setq k 0 res nil)
  (while (and (not res) (<= (* k tl 0.5) (/ step 2.0)))
    (setq off (* k tl 0.5))
    (cond
      ((yq:good ent (+ s0 off) tl avoid) (setq res (+ s0 off)))
      ((and (> k 0) (yq:good ent (- s0 off) tl avoid)) (setq res (- s0 off)))
    )
    (setq k (1+ k))
  )
  res
)

;;; ---------- Weld / joint marks ----------
;;; Distances (along the centerline) of the elbow arcs: ((s1 . s2) ...)
(defun yq:arcintervals (cl verts / i res v)
  (setq i 0 res nil)
  (foreach v verts
    (if (and (/= (cadr v) 0.0) (< i (1- (length verts))))
      (setq res (cons (cons (vlax-curve-getDistAtParam cl (float i))
                            (vlax-curve-getDistAtParam cl (float (1+ i))))
                      res)))
    (setq i (1+ i)))
  (reverse res)
)

;;; Straight runs between the elbows: ((a . b) ...)
(defun yq:runs (len arcs / prev res a)
  (setq prev 0.0 res nil)
  (foreach a arcs
    (setq res (cons (cons prev (car a)) res)
          prev (cdr a)))
  (reverse (cons (cons prev len) res))
)

;;; Joints every L meters inside one straight run (a -> b)
(defun yq:runjoints (a b l / res s k)
  (setq res nil k 1)
  (if (> l 0.0)
    (progn
      (setq s (+ a (* k l)))
      (while (< s (- b 0.3))
        (setq res (cons s res)
              k   (1+ k)
              s   (+ a (* k l))))))
  res
)

(defun yq:uniq (lst / res s)
  (setq res nil)
  (foreach s lst
    (if (or (not res) (> (- s (car res)) 0.02))
      (setq res (cons s res))))
  (reverse res)
)

;;; All weld distances: both ends of every elbow + every L m on straights
(defun yq:weldplan (cl verts l / len arcs prev res a)
  (setq len  (yq:curvelen cl)
        arcs (yq:arcintervals cl verts)
        prev 0.0
        res  nil)
  (foreach a arcs
    (setq res (append (yq:runjoints prev (car a) l) res))
    (setq res (cons (car a) (cons (cdr a) res)))
    (setq prev (cdr a)))
  (setq res (append (yq:runjoints prev len l) res))
  (yq:uniq (vl-sort res '<))
)

;;; Weld / joint mark: thick line through p, direction ang, sized by d
(defun yq:markat (p ang d lname / half w z a b)
  (setq half (* 0.5 yq:weldlen d)
        w    (* yq:weldwid d)
        z    (if (caddr p) (caddr p) 0.0)
        a    (list (- (car p) (* (cos ang) half)) (- (cadr p) (* (sin ang) half)))
        b    (list (+ (car p) (* (cos ang) half)) (+ (cadr p) (* (sin ang) half))))
  (entmakex
    (list '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          (cons 8 lname)
          '(100 . "AcDbPolyline")
          '(90 . 2)
          '(70 . 0)
          (cons 43 w)
          (cons 38 z)
          (cons 10 a) (cons 42 0.0)
          (cons 10 b) (cons 42 0.0)))
)

(defun yq:putweld (cl s d lname)
  (yq:markat (yq:ptat cl s) (+ (yq:angat cl s) (/ pi 2.0)) d lname)
)

;;; ---------- Text ----------
;;; Text centered on p (bottom-aligned), rotated readable, offset off
;;; along the text normal
(defun yq:textat (p ang h str lname off / rot nrm tp)
  (setq rot (yq:readang ang)
        nrm (+ rot (/ pi 2.0))
        tp  (list (+ (car p) (* (cos nrm) off))
                  (+ (cadr p) (* (sin nrm) off))
                  (if (caddr p) (caddr p) 0.0)))
  (entmakex
    (list '(0 . "TEXT")
          '(100 . "AcDbEntity")
          (cons 8 lname)
          '(100 . "AcDbText")
          (cons 10 tp)
          (cons 40 h)
          (cons 1 str)
          (cons 50 rot)
          '(72 . 1)
          (cons 11 tp)
          '(73 . 1)
          '(100 . "AcDbText")))
)

(defun yq:puttext (ent s h str lname)
  (yq:textat (yq:ptat ent s) (yq:angat ent s) h str lname (* 0.08 h))
)

;;; Middle-centered label with rotation rot
(defun yq:putlabel (tp h str lname rot)
  (entmakex
    (list '(0 . "TEXT")
          '(100 . "AcDbEntity")
          (cons 8 lname)
          '(100 . "AcDbText")
          (cons 10 tp)
          (cons 40 h)
          (cons 1 str)
          (cons 50 rot)
          '(72 . 1)
          (cons 11 tp)
          '(73 . 2)
          '(100 . "AcDbText")))
)

;;; Distribute diameter texts evenly along the centerline.
;;; Returns list of (distance . half-length) of the placed texts.
(defun yq:maketexts (cl d cfg avoid / len h str tl n step i s vis pos spc suf)
  (setq spc (yq:cf 'SPC cfg)
        suf (yq:cf 'SUF cfg)
        len (yq:curvelen cl)
        h   (yq:cf 'TH cfg)
        str (strcat yq:txtprefix (yq:mm d) suf)
        vis (+ 1 (strlen (yq:mm d)) (strlen suf))
        tl  (* 0.8 h vis)
        pos nil)
  (if (> len (* 1.2 tl))
    (progn
      (setq n    (if (> spc 0.0) (max 1 (fix (+ 0.5 (/ len spc)))) 1)
            step (/ len n)
            i    0)
      (while (< i n)
        (setq s (yq:findspot cl (* (+ i 0.5) step) tl step avoid))
        (if s
          (progn
            (yq:puttext cl s h str (yq:cf 'TXNAME cfg))
            (setq pos (cons (cons s (/ tl 2.0)) pos))))
        (setq i (1+ i)))
    )
  )
  (reverse pos)
)

;;; ---------- Elbow labels ----------
;;; arcs = ((s1 . s2) ...) in the same order as the elbows with radius > 0
(defun yq:labelelbows (cl elbs arcs d h lname / k e a s p b dx dy dl off tp)
  (setq k 0)
  (foreach e elbs
    (if (> (car e) 0.0)
      (progn
        (setq a (nth k arcs))
        (if a
          (progn
            (setq s  (/ (+ (car a) (cdr a)) 2.0)
                  p  (yq:ptat cl s)
                  b  (caddr e)
                  dx (- (car b) (car p))
                  dy (- (cadr b) (cadr p))
                  dl (sqrt (+ (* dx dx) (* dy dy)))
                  off (+ (* 0.5 d) (* 1.2 h)))
            (setq tp
              (if (> dl 1e-9)
                (list (+ (car p) (* (/ dx dl) off))
                      (+ (cadr p) (* (/ dy dl) off))
                      (caddr p))
                p))
            (yq:putlabel tp (* 0.8 h)
                         (strcat (yq:angstr (cadr e)) "%%d") lname 0.0)))
        (setq k (1+ k))))
  )
)

;;; ---------- Flow arrows ----------
;;; Arrow head = 2-vertex wide polyline tapering from width aw to 0.
;;; The tip points along the drawing direction, or against it if rev.
(defun yq:putarrow (ent s al aw lname rev / p ang a b)
  (setq p   (yq:ptat ent s)
        ang (yq:angat ent s))
  (if rev (setq ang (+ ang pi)))
  (setq a (yq:padd p ang (* -0.5 al))
        b (yq:padd p ang (* 0.5 al)))
  (yq:taper a b aw 0.0 lname)
)

;;; Arrows every asp meters (evenly distributed), avoiding welds/texts
(defun yq:makearrows (cl d cfg avoid / len al aw n step i s asp)
  (setq asp (yq:cf 'ASP cfg)
        len (yq:curvelen cl)
        al  (* yq:arwlen d)
        aw  (* yq:arwwid d))
  (if (> len (* 1.5 al))
    (progn
      (setq n    (max 1 (fix (+ 0.5 (/ len asp))))
            step (/ len n)
            i    0)
      (while (< i n)
        (setq s (yq:findspot cl (* (+ i 0.5) step) al step avoid))
        (if s (yq:putarrow cl s al aw (yq:cf 'ARNAME cfg) (yq:cf 'REV cfg)))
        (setq i (1+ i)))
    )
  )
)

;;; ---------- BOQ data ----------
;;; Pipe record: (key type mm length pipes welds elbows)
;;;   elbows = ((angle . count) ...)
(defun yq:boqadd (type mm len pipes welds angs / key rec el a x new)
  (setq key (strcat type "|" mm)
        rec (assoc key yq:boqdata))
  (if (not rec) (setq rec (list key type mm 0.0 0 0 nil)))
  (setq el (nth 6 rec))
  (foreach a angs
    (if (setq x (assoc a el))
      (setq el (subst (cons a (1+ (cdr x))) x el))
      (setq el (append el (list (cons a 1))))))
  (setq new (list key type mm
                  (+ (nth 3 rec) len)
                  (+ (nth 4 rec) pipes)
                  (+ (nth 5 rec) welds)
                  el))
  (if (assoc key yq:boqdata)
    (setq yq:boqdata (subst new (assoc key yq:boqdata) yq:boqdata))
    (setq yq:boqdata (append yq:boqdata (list new))))
)

;;; Fitting record: (key type kind diameters qty joints)
(defun yq:fitadd (type kind m1 m2 joints / key rec new)
  (setq key (strcat type "|" kind "|" m1 "|" m2)
        rec (assoc key yq:fitdata))
  (if (not rec) (setq rec (list key type kind (strcat m1 " x " m2) 0 0)))
  (setq new (list key type kind (nth 3 rec)
                  (1+ (nth 4 rec))
                  (+ (nth 5 rec) joints)))
  (if (assoc key yq:fitdata)
    (setq yq:fitdata (subst new (assoc key yq:fitdata) yq:fitdata))
    (setq yq:fitdata (append yq:fitdata (list new))))
)

;;; sym = text used for the degree sign ("%%d" in tables, " deg" on screen)
(defun yq:elbstr (el sym / s x)
  (setq el (vl-sort el '(lambda (a b) (> (car a) (car b))))
        s  "")
  (foreach x el
    (setq s (strcat s (if (= s "") "" ", ")
                    (yq:angstr (car x)) sym " x " (itoa (cdr x)))))
  (if (= s "") "-" s)
)

(defun yq:boqprint ( / r)
  (princ "\n\n---------------- Pipe BOQ ----------------")
  (foreach r yq:boqdata
    (princ (strcat "\n" (nth 1 r) " Pipe Dia " (nth 2 r) " mm :"
                   "  length " (rtos (nth 3 r) 2 2) " m"
                   " | pipes " (itoa (nth 4 r))
                   " | welds/joints " (itoa (nth 5 r))
                   " | elbows " (yq:elbstr (nth 6 r) " deg"))))
  (foreach r yq:fitdata
    (princ (strcat "\n" (nth 1 r) " " (nth 2 r) " " (nth 3 r) " mm :"
                   "  qty " (itoa (nth 4 r))
                   " | welds/joints " (itoa (nth 5 r)))))
  (princ "\n------------------------------------------\n")
)

;;; Insert an AutoCAD table with the BOQ (pipes + fittings in one table)
(defun yq:inserttable (pt h / doc spc np nf ncol nrows tbl row c r w tl tp tw layname)
  (setq doc   (vla-get-ActiveDocument (vlax-get-acad-object))
        spc   (if (= (vla-get-ActiveSpace doc) 1)
                (vla-get-ModelSpace doc)
                (vla-get-PaperSpace doc))
        np    (length yq:boqdata)
        nf    (length yq:fitdata)
        ncol  7
        nrows (+ np nf 3)
        tbl   (vla-AddTable spc (vlax-3d-point pt) nrows ncol (* 2.2 h) (* 12.0 h)))

  ;; Do not let AutoCAD rebuild the table while we fill the cells
  (vl-catch-all-apply
    '(lambda () (vla-put-RegenerateTableSuppressed tbl :vlax-true)))

  ;; Use the Standard table style so cell text is always visible
  (vl-catch-all-apply
    '(lambda () (vla-put-StyleName tbl "Standard")))

  ;; Column widths
  (setq c 0)
  (foreach w '(5.0 16.0 12.0 12.0 11.0 13.0 30.0)
    (vla-SetColumnWidth tbl c (* w h))
    (setq c (1+ c)))

  ;; Title
  (vla-SetText tbl 0 0 "Pipe BOQ")

  ;; Header
  (setq c 0)
  (foreach r '("Item" "Description" "Dia (mm)" "Length (m)" "Pipes / Qty"
               "Welds / Joints" "Elbows")
    (vla-SetText tbl 1 c r)
    (setq c (1+ c)))

  ;; Pipe rows
  (setq row 2 tl 0.0 tp 0 tw 0)
  (foreach r yq:boqdata
    (vla-SetText tbl row 0 (itoa (- row 1)))
    (vla-SetText tbl row 1 (strcat (nth 1 r) " Pipe"))
    (vla-SetText tbl row 2 (nth 2 r))
    (vla-SetText tbl row 3 (rtos (nth 3 r) 2 2))
    (vla-SetText tbl row 4 (itoa (nth 4 r)))
    (vla-SetText tbl row 5 (itoa (nth 5 r)))
    (vla-SetText tbl row 6 (yq:elbstr (nth 6 r) "%%d"))
    (setq tl  (+ tl (nth 3 r))
          tp  (+ tp (nth 4 r))
          tw  (+ tw (nth 5 r))
          row (1+ row)))

  ;; Fitting rows (reducers / tees)
  (foreach r yq:fitdata
    (vla-SetText tbl row 0 (itoa (- row 1)))
    (vla-SetText tbl row 1 (strcat (nth 1 r) " " (nth 2 r)))
    (vla-SetText tbl row 2 (nth 3 r))
    (vla-SetText tbl row 3 "-")
    (vla-SetText tbl row 4 (itoa (nth 4 r)))
    (vla-SetText tbl row 5 (itoa (nth 5 r)))
    (vla-SetText tbl row 6 "-")
    (setq tw  (+ tw (nth 5 r))
          row (1+ row)))

  ;; Total row (length and pipes = pipes only, joints = pipes + fittings)
  (vla-SetText tbl row 1 "Total")
  (vla-SetText tbl row 3 (rtos tl 2 2))
  (vla-SetText tbl row 4 (itoa tp))
  (vla-SetText tbl row 5 (itoa tw))

  ;; Explicit text height and alignment for every cell
  (setq r 0)
  (while (< r nrows)
    (setq c 0)
    (while (< c ncol)
      (vl-catch-all-apply
        '(lambda ()
           (vla-SetCellTextHeight tbl r c (if (= r 0) (* 1.3 h) h))
           (vla-SetCellAlignment tbl r c 5)))
      (setq c (1+ c)))
    (setq r (1+ r)))

  ;; Layer
  (setq layname (strcat (yq:baselay) " - BOQ"))
  (if (tblsearch "LAYER" layname)
    (vla-put-Layer tbl layname))

  ;; Rebuild the table contents so the cells show up
  (vl-catch-all-apply
    '(lambda () (vla-put-RegenerateTableSuppressed tbl :vlax-false)))
  (vl-catch-all-apply
    '(lambda () (vla-RecomputeTableBlock tbl :vlax-true)))
  (vl-catch-all-apply
    '(lambda () (vla-Update tbl)))
  (vla-Regen doc 1)

  tbl
)

;;; Print the BOQ and (optionally) insert the table
(defun yq:boqshow (force h / pt r)
  (if (or yq:boqdata yq:fitdata)
    (progn
      (yq:boqprint)
      (if (or force (= yq:dboq "1"))
        (progn
          (setq pt (getpoint "\nSpecify BOQ table insertion point <skip>: "))
          (if pt
            (progn
              (setq r (vl-catch-all-apply 'yq:inserttable (list pt h)))
              (if (vl-catch-all-error-p r)
                (princ (strcat "\nBOQ table could not be created: "
                               (vl-catch-all-error-message r)))))))))
  )
)

;;; ---------- One complete pipe ----------
;;; wide polyline + centerline + weld marks + elbow labels + texts + arrows
(defun yq:makepipe (pts cfg elbs / d h verts pen cl welds avoid texts hw arcs runs len
                    pipes lp angs r a s)
  (setq yq:step "pipe: build vertices")
  (setq d     (yq:cf 'D cfg)
        h     (yq:cf 'TH cfg)
        lp    (yq:cf 'JSP cfg)
        verts (yq:buildverts pts (mapcar 'car elbs)))
  (setq yq:step "pipe: wide polyline")
  (setq pen (yq:makepoly verts d (yq:cf 'LNAME cfg) nil nil))
  (setq yq:step "pipe: centerline")
  (setq cl (yq:makepoly verts 0.0 (yq:cf 'CLNAME cfg) (yq:cf 'LTYPE cfg) (* d 0.25)))
  (if cl
    (progn
      (setq yq:step "pipe: arcs and runs")
      (setq len   (yq:curvelen cl)
            arcs  (yq:arcintervals cl verts)
            runs  (yq:runs len arcs)
            welds (if (yq:cf 'JNT cfg)
                    (yq:weldplan cl verts lp)
                    nil)
            hw    (/ (* yq:weldwid d) 2.0))
      (setq yq:step "pipe: weld marks")
      (foreach s welds
        (yq:putweld cl s d (yq:cf 'WLNAME cfg)))
      (setq yq:step "pipe: elbow labels")
      (yq:labelelbows cl elbs arcs d h (yq:cf 'ELNAME cfg))
      (setq avoid (mapcar '(lambda (s) (cons s hw)) welds))
      (setq yq:step "pipe: diameter texts")
      (setq texts (yq:maketexts cl d cfg avoid))
      (setq yq:step "pipe: flow arrows")
      (if (yq:cf 'ARW cfg)
        (yq:makearrows cl d cfg (append avoid texts)))
      (setq yq:step "pipe: BOQ numbers")
      (setq pipes 0)
      (foreach r runs
        (if (> (- (cdr r) (car r)) 0.05)
          (setq pipes (+ pipes 1 (length (yq:runjoints (car r) (cdr r) lp))))))
      (setq angs nil)
      (foreach a elbs
        (if (> (car a) 0.0) (setq angs (cons (cadr a) angs))))
      (setq angs (reverse angs))
      (yq:boqadd (yq:cf 'TYPE cfg) (yq:mm d) len pipes (length welds) angs)
      (setq yq:step "pipe: tagging")
      (if pen
        (yq:tag pen
          (append
            (list (cons 1000 "PIPE")
                  (cons 1000 (yq:cf 'TYPE cfg))
                  (cons 1000 (yq:mm d))
                  (cons 1040 len)
                  (cons 1070 pipes)
                  (cons 1070 (length welds)))
            (mapcar '(lambda (x) (cons 1040 x)) angs))))
      (setq yq:step "pipe: done")
    )
  )
)

;;; Build a pipe from a point list (linked or one pipe per segment)
(defun yq:buildpath (pts cfg link / i)
  (if link
    (yq:makepipe pts cfg (yq:planelbows pts cfg))
    (progn
      (setq i 0)
      (while (< i (1- (length pts)))
        (yq:makepipe (list (nth i pts) (nth (1+ i) pts)) cfg nil)
        (setq i (1+ i)))))
)

;;; ---------- Fittings: reducer and tee ----------
;;; Reducer: tapered wide polyline from d1 (at p) to d2, length len
(defun yq:makereducer (p ang d1 d2 len h cfg / b ent mid rot nrm lp str)
  (setq b   (yq:padd p ang len)
        ent (yq:taper p b d1 d2 (yq:cf 'FLNAME cfg)))
  (if ent
    (yq:tag ent
      (list (cons 1000 "FIT")
            (cons 1000 "Reducer")
            (cons 1000 (yq:cf 'TYPE cfg))
            (cons 1000 (yq:mm d1))
            (cons 1000 (yq:mm d2))
            (cons 1070 2))))
  (if (yq:cf 'JNT cfg)
    (progn
      (yq:markat p (+ ang (/ pi 2.0)) d1 (yq:cf 'WLNAME cfg))
      (yq:markat b (+ ang (/ pi 2.0)) d2 (yq:cf 'WLNAME cfg))))
  (if (yq:cf 'LBL cfg)
    (progn
      (setq mid (yq:padd p ang (/ len 2.0))
            rot (yq:readang ang)
            nrm (+ rot (/ pi 2.0))
            lp  (yq:padd mid nrm (+ (/ (max d1 d2) 2.0) (* 0.9 h)))
            str (strcat "RED " yq:txtprefix (yq:mm d1) " x " yq:txtprefix (yq:mm d2)))
      (yq:putlabel lp h str (yq:cf 'TXNAME cfg) rot)))
  ent
)

;;; Tee: main run (width d1, length lm) centered at c, branch (width d2,
;;; length lb from the center) on the side given by "side" (cross product)
(defun yq:maketee (c ang d1 d2 lm lb side h cfg / a b nang e ent lp rot str)
  (setq a    (yq:padd c ang (- (/ lm 2.0)))
        b    (yq:padd c ang (/ lm 2.0))
        nang (if (>= side 0.0) (+ ang (/ pi 2.0)) (- ang (/ pi 2.0)))
        e    (yq:padd c nang lb)
        ent  (yq:taper a b d1 d1 (yq:cf 'FLNAME cfg)))
  (yq:taper c e d2 d2 (yq:cf 'FLNAME cfg))
  (if ent
    (yq:tag ent
      (list (cons 1000 "FIT")
            (cons 1000 "Tee")
            (cons 1000 (yq:cf 'TYPE cfg))
            (cons 1000 (yq:mm d1))
            (cons 1000 (yq:mm d2))
            (cons 1070 3))))
  (if (yq:cf 'JNT cfg)
    (progn
      (yq:markat a (+ ang (/ pi 2.0)) d1 (yq:cf 'WLNAME cfg))
      (yq:markat b (+ ang (/ pi 2.0)) d1 (yq:cf 'WLNAME cfg))
      (yq:markat e (+ nang (/ pi 2.0)) d2 (yq:cf 'WLNAME cfg))))
  (if (yq:cf 'LBL cfg)
    (progn
      (setq lp  (yq:padd c (+ nang pi) (+ (/ d1 2.0) (* 0.9 h)))
            rot (yq:readang ang)
            str (strcat "TEE " yq:txtprefix (yq:mm d1) " x " yq:txtprefix (yq:mm d2)))
      (yq:putlabel lp h str (yq:cf 'TXNAME cfg) rot)))
  ent
)

;;; ---------- Reading existing objects ----------
(defun yq:flag (ed / f)
  (setq f (cdr (assoc 70 ed)))
  (if f f 0)
)

(defun yq:dedupe (pts / res p)
  (setq res nil)
  (foreach p pts
    (if (or (not res) (> (distance p (car res)) 1e-6))
      (setq res (cons p res))))
  (reverse res)
)

;;; Vertex list of a LINE / LWPOLYLINE / POLYLINE
(defun yq:entpts (en / ed typ pts z e v g closed)
  (setq ed  (entget en)
        typ (cdr (assoc 0 ed))
        pts nil)
  (cond
    ((= typ "LINE")
     (setq pts (list (cdr (assoc 10 ed)) (cdr (assoc 11 ed)))))
    ((= typ "LWPOLYLINE")
     (setq z (if (assoc 38 ed) (cdr (assoc 38 ed)) 0.0))
     (foreach g ed
       (if (= (car g) 10)
         (setq pts (cons (list (cadr g) (caddr g) z) pts))))
     (setq pts (reverse pts))
     (if (and pts (= 1 (logand 1 (yq:flag ed))))
       (setq pts (append pts (list (car pts))))))
    ((= typ "POLYLINE")
     (setq closed (= 1 (logand 1 (yq:flag ed)))
           e      (entnext en))
     (while (and e (= (cdr (assoc 0 (setq v (entget e)))) "VERTEX"))
       (if (= 0 (logand 16 (yq:flag v)))
         (setq pts (cons (cdr (assoc 10 v)) pts)))
       (setq e (entnext e)))
     (setq pts (reverse pts))
     (if (and closed pts)
       (setq pts (append pts (list (car pts))))))
  )
  (yq:dedupe pts)
)

;;; ============================================================
;;; Command: ASPP  (draw pipes)
;;; ============================================================
(defun c:ASPP ( / d th ti cfg lname jword ltype link ss i en pts p n *error*)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nASPP error: " msg " | step: "
                     (if yq:step yq:step "start"))))
    (princ))
  (setq yq:step "dialog")
  (if (yq:dialog)
    (progn
      (setq yq:boqdata nil
            yq:fitdata nil
            d     (atof yq:dia)
            th    (if (> (atof yq:txh) 0.0) (atof yq:txh) (* yq:txtfactor d))
            ti    (nth (atoi yq:typ) yq:types)
            jword (nth 3 ti)
            lname (yq:layername d)
            link  (= yq:lnk "1"))
      (yq:ensurelayer lname)
      (setq ltype (yq:ensureaux jword))
      (setq cfg
        (list (cons 'D      d)
              (cons 'TH     th)
              (cons 'TXH    (atof yq:txh))
              (cons 'RMAN   (atof yq:rad))
              (cons 'RFAC   (nth 4 ti))
              (cons 'ASK    (= yq:ask "1"))
              (cons 'SPC    (atof yq:spc))
              (cons 'ASP    (atof yq:asp))
              (cons 'JSP    (atof yq:jsp))
              (cons 'JNT    (= yq:jnt "1"))
              (cons 'ARW    (= yq:arw "1"))
              (cons 'REV    (= yq:rev "1"))
              (cons 'TYPE   (car ti))
              (cons 'SUF    (strcat " " (cadr ti)))
              (cons 'LNAME  lname)
              (cons 'CLNAME (strcat (yq:baselay) " - Centerline"))
              (cons 'TXNAME (strcat (yq:baselay) " - Text"))
              (cons 'ARNAME (strcat (yq:baselay) " - Flow Arrow"))
              (cons 'WLNAME (strcat (yq:baselay) " - " jword))
              (cons 'ELNAME (strcat (yq:baselay) " - Elbow"))
              (cons 'LTYPE  ltype)))
      (cond
        ;; ---- Select existing lines / polylines ----
        ((= yq:mode "2")
         (princ "\nSelect lines or polylines to convert into pipes: ")
         (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
         (if (not ss)
           (princ "\nNothing selected.")
           (progn
             (setq i 0 n 0)
             (repeat (sslength ss)
               (setq en  (ssname ss i)
                     pts (yq:entpts en))
               (if (>= (length pts) 2)
                 (progn
                   (yq:buildpath pts cfg link)
                   (setq n (1+ n))
                   (if (= yq:ers "1") (entdel en))))
               (setq i (1+ i)))
             (princ (strcat "\n" (itoa n) " pipe path(s) drawn on layer: " lname)))))
        ;; ---- Pick points ----
        (T
         (while (not (setq p (getpoint "\nSpecify pipe start point: "))))
         (setq pts (list p))
         (while (setq p (getpoint (last pts) "\nSpecify next point <Enter to finish>: "))
           (grdraw (last pts) p 7)
           (setq pts (append pts (list p))))
         (if (< (length pts) 2)
           (princ "\nAt least two points are required.")
           (progn
             (yq:buildpath pts cfg link)
             (princ (strcat "\nPipe drawn on layer: " lname)))))
      )
      (redraw)
      (yq:boqshow nil (if (> (atof yq:txh) 0.0) (atof yq:txh) 0.35))
    )
  )
  (princ)
)

;;; ============================================================
;;; Command: ASPPFIT  (reducer / tee)
;;; ============================================================
(defun c:ASPPFIT ( / ti d1 d2 flen h jword cfg p1 p2 p3 ang len lb side ok isred *error*)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nASPPFIT error: " msg)))
    (princ))
  (if (yq:fitdialog)
    (progn
      (setq ti    (nth (atoi yq:ftyp) yq:types)
            jword (nth 3 ti)
            d1    (atof yq:fd1)
            d2    (atof yq:fd2)
            flen  (atof yq:flen)
            isred (= yq:ftype "1")
            h     (if (> (atof yq:txh) 0.0)
                    (atof yq:txh)
                    (* yq:txtfactor (max d1 d2))))
      (yq:ensureaux jword)
      (setq cfg
        (list (cons 'TYPE   (car ti))
              (cons 'JNT    (= yq:fjn "1"))
              (cons 'LBL    (= yq:flb "1"))
              (cons 'FLNAME (strcat (yq:baselay) " - Fitting"))
              (cons 'WLNAME (strcat (yq:baselay) " - " jword))
              (cons 'TXNAME (strcat (yq:baselay) " - Text"))))
      (setq ok T)
      (while ok
        (setq p1 (getpoint
                   (if isred
                     "\nSpecify reducer start point (Diameter 1 end) <exit>: "
                     "\nSpecify tee center point <exit>: ")))
        (if (not p1)
          (setq ok nil)
          (progn
            (setq p2 (getpoint p1
                       (if isred
                         "\nSpecify direction point (toward Diameter 2 end): "
                         "\nSpecify main run direction point: ")))
            (if (or (not p2) (< (distance p1 p2) 1e-9))
              (princ "\nDirection point required.")
              (progn
                (setq ang (angle p1 p2))
                (if isred
                  (progn
                    (setq len (if (> flen 0.0)
                                flen
                                (max (* 3.0 (abs (- d1 d2))) (max d1 d2))))
                    (yq:makereducer p1 ang d1 d2 len h cfg)
                    (princ (strcat "\nReducer " (yq:mm d1) " x " (yq:mm d2)
                                   " mm placed (length " (rtos len 2 2) " m).")))
                  (progn
                    (setq p3 (getpoint p1 "\nSpecify branch side point: "))
                    (if (not p3)
                      (princ "\nBranch side point required.")
                      (progn
                        (setq len  (if (> flen 0.0) flen (* 2.5 d1))
                              lb   (+ (/ d1 2.0) d2)
                              side (- (* (cos ang) (- (cadr p3) (cadr p1)))
                                      (* (sin ang) (- (car p3) (car p1)))))
                        (yq:maketee p1 ang d1 d2 len lb side h cfg)
                        (princ (strcat "\nTee " (yq:mm d1) " x " (yq:mm d2)
                                       " mm placed.")))))))))))
    )
  )
  (princ)
)

;;; ============================================================
;;; Command: ASPPBOQ  (select drawn items -> one BOQ table)
;;; ============================================================
(defun c:ASPPBOQ ( / ss i en ed xd v dflt h *error*)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nASPPBOQ error: " msg)))
    (princ))
  (yq:regapp)
  (setq yq:boqdata nil
        yq:fitdata nil)
  (princ "\nSelect the pipes and fittings for the BOQ (window / crossing / ALL): ")
  (setq ss (ssget '((-3 ("ASPP")))))
  (if (not ss)
    (princ "\nNo ASPP pipes or fittings found in the selection.")
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq en (ssname ss i)
              ed (entget en '("ASPP"))
              xd (cdr (cadr (assoc -3 ed)))
              v  (mapcar 'cdr xd))
        (cond
          ((= (car v) "PIPE")
           (yq:boqadd (nth 1 v) (nth 2 v) (nth 3 v) (nth 4 v) (nth 5 v)
                      (yq:drop 6 v)))
          ((= (car v) "FIT")
           (yq:fitadd (nth 2 v) (nth 1 v) (nth 3 v) (nth 4 v) (nth 5 v))))
        (setq i (1+ i)))
      (setq dflt (if (> (atof yq:txh) 0.0) (atof yq:txh) 0.35))
      (initget 6)
      (setq h (getreal (strcat "\nTable text height <" (rtos dflt 2 2) ">: ")))
      (if (not h) (setq h dflt))
      (yq:boqshow T h)))
  (princ)
)

(princ "\nASPP loaded.  Commands: ASPP (pipes) | ASPPFIT (reducer / tee) | ASPPBOQ (BOQ table).")
(princ)