;;; ------------------------------------------------------------
;;; CPP - Replace the layouts of the current drawing with all
;;; layouts from another DWG/DWT.
;;; Steps: read source layout names -> delete existing layouts
;;;        -> import source layouts -> clean up leftovers.
;;; Usage: APPLOAD this file, type CPP, then pick the source file.
;;; ------------------------------------------------------------
(vl-load-com)

;; Return the list of layout names (excluding Model) in a file, in tab order
(defun cpp:get-layouts (path / acad dbx res lst)
  (setq acad (vlax-get-acad-object)
        dbx  (vl-catch-all-apply
               'vla-getinterfaceobject
               (list acad
                     (strcat "ObjectDBX.AxDbDocument."
                             (substr (getvar "ACADVER") 1 2)))))
  (if (vl-catch-all-error-p dbx)
    (progn (prompt "\nCannot create ObjectDBX document.") nil)
    (progn
      (setq res (vl-catch-all-apply 'vla-open (list dbx path)))
      (if (vl-catch-all-error-p res)
        (prompt "\nCannot open the source file.")
        (progn
          (vlax-for lay (vla-get-layouts dbx)
            (if (/= (strcase (vla-get-name lay)) "MODEL")
              (setq lst (cons (cons (vla-get-taborder lay)
                                    (vla-get-name lay))
                              lst))))
          (setq lst (mapcar 'cdr
                            (vl-sort lst '(lambda (a b) (< (car a) (car b))))))))
      (vlax-release-object dbx)
      lst)))

;; Return upper-cased names of layouts in the current drawing (excluding Model)
(defun cpp:existing-layouts (/ lst)
  (vlax-for lay (vla-get-layouts
                  (vla-get-activedocument (vlax-get-acad-object)))
    (if (/= (strcase (vla-get-name lay)) "MODEL")
      (setq lst (cons (strcase (vla-get-name lay)) lst))))
  lst)

;; Delete layouts of the current drawing. If keep-names is non-nil, layouts
;; whose names are in that list (upper-case) are kept.
(defun cpp:delete-layouts (keep-names / layouts objs)
  (setq layouts (vla-get-layouts
                  (vla-get-activedocument (vlax-get-acad-object))))
  (vlax-for lay layouts
    (if (and (/= (strcase (vla-get-name lay)) "MODEL")
             (not (member (strcase (vla-get-name lay)) keep-names)))
      (setq objs (cons lay objs))))
  (foreach lay objs
    (vl-catch-all-apply 'vla-delete (list lay))))

;; Return items from position `from` to position `to` (1-based, inclusive)
(defun cpp:range (lst from to / out i)
  (setq i 1)
  (foreach item lst
    (if (and (>= i from) (<= i to))
      (setq out (cons item out)))
    (setq i (1+ i)))
  (reverse out))

(defun c:CPP (/ src names total i from to ans exist oldfd oldecho oldtab cnt skipped res)
  (setq src (getfiled "Select source drawing" "" "dwg;dwt" 0))
  (if src
    (if (setq names (cpp:get-layouts src))
      (progn
        ;; Ask for the range of layouts to import (tab order, 1-based)
        (setq total (length names))
        (prompt (strcat "\nSource file has " (itoa total) " layout(s):"))
        (setq i 0)
        (foreach n names
          (setq i (1+ i))
          (prompt (strcat "\n  " (itoa i) " - " n)))
        (initget 6) ; positive, non-zero
        (setq from (getint "\nImport FROM layout number <1>: "))
        (if (null from) (setq from 1))
        (initget 6)
        (setq to (getint (strcat "\nImport TO layout number <" (itoa total) ">: ")))
        (if (null to) (setq to total))
        (if (> to total) (setq to total))
        (if (> from total) (setq from total))
        (if (> from to)
          (setq from (+ from to)
                to   (- from to)
                from (- from to)))
        (setq names (cpp:range names from to))
        (initget "Yes No")
        (setq ans (getkword
                    "\nDelete ALL existing layouts in this drawing first? [Yes/No] <Yes>: "))
        (if (null ans) (setq ans "Yes"))
        (setq oldfd   (getvar "FILEDIA")
              oldecho (getvar "CMDECHO")
              oldtab  (getvar "CTAB")
              cnt     0)
        (setvar "FILEDIA" 0)
        (setvar "CMDECHO" 0)
        (setq res
          (vl-catch-all-apply
            '(lambda ()
               ;; Step 1: delete existing layouts (switch to Model first)
               (if (= ans "Yes")
                 (progn
                   (setvar "CTAB" "Model")
                   (cpp:delete-layouts nil)))
               ;; Step 2: import source layouts
               (setq exist (cpp:existing-layouts))
               (foreach n names
                 (if (member (strcase n) exist)
                   (setq skipped (cons n skipped))
                   (progn
                     (command "_.-LAYOUT" "_Template" src n)
                     (setq cnt (1+ cnt)))))
               ;; Step 3: remove leftovers (e.g. default layout that
               ;; AutoCAD refused to delete) that are not in the source
               (if (= ans "Yes")
                 (progn
                   (setvar "CTAB" "Model")
                   (cpp:delete-layouts (mapcar 'strcase names)))))))
        ;; Always restore system variables
        (setvar "FILEDIA" oldfd)
        (setvar "CMDECHO" oldecho)
        (if (member (strcase oldtab) (cons "MODEL" (mapcar 'strcase names)))
          (vl-catch-all-apply 'setvar (list "CTAB" oldtab))
          (vl-catch-all-apply 'setvar (list "CTAB" (car names))))
        (if (vl-catch-all-error-p res)
          (prompt (strcat "\nError: " (vl-catch-all-error-message res))))
        (prompt (strcat "\nImported " (itoa cnt) " layout(s)."))
        (if skipped
          (prompt (strcat "\nSkipped (name already exists): "
                          (apply 'strcat
                                 (mapcar '(lambda (s) (strcat s "; "))
                                         (reverse skipped)))))))
      (prompt "\nNo layouts found or file could not be read.")))
  (princ))

(prompt "\nCPP loaded. Type CPP to replace layouts from another drawing.")
(princ)
