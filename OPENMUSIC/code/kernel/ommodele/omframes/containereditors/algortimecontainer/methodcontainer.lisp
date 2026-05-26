;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;Method Editor definition.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)


;---------------------------------------------------------------------
;EDITOR
;---------------------------------------------------------------------

(defclass methodEditor (patchEditor)  
  ((win-mod :initform nil :initarg :win-mod :accessor win-mod)
   (quali-buttons :initform nil :initarg :quali-buttons :accessor quali-buttons)
   (iconID :initform 145 :accessor iconID)
   (compiled? :initform nil :accessor compiled?)
   (pack :initform nil :accessor pack)
   ))


(defmethod get-editor-panel-class ((self methodEditor))  'methodPanel)


(defparameter *abort-definition* nil)

;(defmethod close-editor-before ((self methodEditor))
;   (call-next-method)
;   (unless *abort-definition*
;     (modify-genfun (panel self))))

(defmethod editor-close? ((self methodeditor))
  (modify-genfun (panel self))
  (if *abort-definition*
      (let ((rep (om-y-or-n-dialog (format nil *abort-definition*))))
        rep)
    t))
      

(defmethod om-view-close-event ((self methodEditor))
   (unless *abort-definition*
     (cond
      ((equal (class-method-p (object (panel self))) 'init)
       (setf (editorFrame (find-class (internp (get-init-method-class-name (name (object (panel self))))
                                               (symbol-package (method-name (object (panel self))))))) nil))
      ((ommethod-p (object (panel self)))
       (setf (editorFrame (object (panel self))) nil))))
   (setf *abort-definition* nil))


;---------------------------------------------------------------------
;PANEL
;---------------------------------------------------------------------

(defclass methodPanel (patchPanel) 
   ((docu :initform "" :accessor docu))
   (:documentation "Method editors are instance of this class.
Elements of methodPanels are instaces of the boxframe class.#enddoc#
#seealso# (OMMethod boxframe) #seealso#
#docu# Used to store the method's documentation. #docu#"))


(defmethod modify-patch ((self methodPanel))
   "Set the compiled flag of the window to nil."
   (setf (compiled? (editor self)) nil))

;-----------------------------------------------------
;Code Generation and Method Definition from the window
;-----------------------------------------------------

(defun make-method-lambda-list (lis)
   "Cons a lambda list from a list 'lis' of input boxes."
   (let (standard optional rest keywrds)
     (mapc #'(lambda (boxframe)
               (let ((box (object boxframe)))
                 (cond
                  ((equal (keys box) nil) (push (list (read-from-string (name box)) (reference box)) standard))
                  ((equal (keys box) '&optional) (push (list (read-from-string (name box)) (defval box)) optional))
                  ((equal (keys box) '&rest) (push (read-from-string (name box)) rest))
                  ((equal (keys box) '&key) (push (list (read-from-string (name box)) (defval box)) keywrds))))) lis)
     (setf standard (reverse standard))
     (when optional
       (setf standard (append standard (list '&optional) (reverse optional))))
     (when rest
       (setf standard (append standard (list '&rest) (reverse rest))))
     (when keywrds
       (setf standard (append standard (list '&key) (reverse keywrds))))
     (cond
      ((> (length rest) 1) "Only one &rest argument allowed.")
      (t standard))))

(defun unify-list (l1 l2)
   (loop for item in l1 do
         (when l2 (pop l2)))
   (append l1 l2))

(defvar *pretraitement-passing* nil)

(defun make-new-om-function (name args numouts initvals doc icon qualy out-box fundoc &optional (pack *package-user*))
   "Define a method in Common Lisp."
   (let (old-function thebody thecode new-method oldflag)
     (setf *pretraitement-passing* nil)
     (setf old-function (and (fboundp name) (fdefinition name)))
     (when old-function
       (setf oldflag (class-method-p (car (get-elements old-function)))))
     (setf thebody (append (list 'values)
                           (mapcar #'(lambda (out)
                                       (gen-code (object out) 0)) out-box)))
     (setf thebody (list 'let* *let-list*  thebody))
     
     (cond
      
      ((equal oldflag 'init)
       (let ((class (find-class (second (car args)) nil))
             initbox)
         (when class
           (setf thebody `((call-next-method) (unless from-file ,thebody)))
           (eval  `(defmethod initialize-instance (,(car args) &rest args &key (from-file nil))
                            (declare(ignore args))
                     ,.thebody))
           (setf new-method (car (get-elements old-function)))
           (push/replace-init class new-method)
           (resave-class class)
           (setf initbox (car (find-class-boxes (graph-fun new-method) 'OMBoxCallNextInit)))
           (when initbox
             (setf *pretraitement-passing* t)
             (eval   `(defmethod pretraitement (,(car args) &rest args)
                        (setf args (call-next-method))
                        (unify-list (list ,.(cdr (decode initbox))) args)))
             (setf *pretraitement-passing* nil)))))
      ((equal oldflag 'get)
       (let ((class (find-class (second (car args)) nil)))
         (when class
           (eval `(defmethod ,(getsetfunname (string name))  ,args ,thebody ))
           (setf new-method (car (get-elements old-function)))
           (resave-class class))))
      ((equal oldflag 'set)
       (let ((class (find-class (second (car args)) nil)))
         (when class
           (eval `(defmethod (setf ,(getsetfunname (string name)))  ,(reverse args) ,thebody ))
           (setf new-method (car (get-elements old-function)))
           (resave-class class))))
      (t (setf thecode `(defmethod* ,name ,.qualy ,args :numouts ,numouts 
                          :initvals (list ,.initvals) :indoc (list ,.doc) :icon ,(car (list! icon)) :doc ,fundoc ,thebody ))
         (setf new-method (eval thecode))
         (when (and (not old-function)  (listp icon))
           (icon-for-user-package (fdefinition (method-name new-method)) (second icon)))
         (setf (protected-p new-method) nil)
         (when oldflag
           (setf (class-method-p new-method) oldflag))
         (when (and old-function (protected-p old-function))
           (funfromkernel pack old-function))
         ))
     new-method))


(defvar *curr-def-package* nil)


(defmethod modify-genfun ((self methodPanel))
   "This method is called when you close the window."
   (handler-bind 
       ((error #'(lambda (c) (declare (ignore c))
                   (print "Method Definition Error.")
                   (setf *abort-definition* "Method Definition Error. No modification will be made to the method.~%Close anyway ?")
                   (abort c)
                   )))
     (print (list "method compilation..."))
     (let* ((*package* (find-package :om))
            (funname (if  (or (equal (win-mod (editor self)) :abs)
                              (equal (win-mod (editor self)) :new))
                         (interne (name self))
                       (interne (name (object self)))))
            (controls (get-subframes self))
            (out-box (sort (find-class-boxes controls 'outFrame) #'< :key #'(lambda (item) (indice (object item)))))
            (in-boxes (sort (find-class-boxes controls 'TypedInFrame) #'< :key #'(lambda (item) (indice (object item)))))
            (lambda-list (make-method-lambda-list in-boxes))
            (initvals (mapcar #'(lambda (item) (get-default-input-val (object item))) in-boxes))
            (doc (mapcar #'(lambda (item) (docu (object item))) in-boxes))
            (fundoc (docu self))
            (iconid (iconid (editor self)))
            (*let-list* nil) thequaly themethod pictlist)
        
        (cond 
         ((equal (win-mod (editor self)) :abs)
          ;;; new generic function      
          (print "first method definition...")
          (cond 
           ((not out-box) 
            (setf *abort-definition* "Generic Functions must have at least one output!~%Close anyway? (If Yes, the function will not be defined)"))
          (t (if (stringp lambda-list)
                 (setf *abort-definition* (string+ "Error in Lambda list: " lambda-list " Close anyway? (If Yes, the function will not be defined)"))
               (let (newGenFun)
                 (setf themethod (make-new-om-function funname lambda-list 
                                                       (length out-box) initvals doc iconid nil out-box fundoc
                                                       (pack (editor self))))
                 (setf (create-info themethod) (list (om-get-date) (om-get-date)))
                 (setf (EditorFrame themethod) self)
                 (setf pictlist (pictu-list (object self)))
                 (setf (object self) themethod)
                 (setf newGenFun (fdefinition funname))
                 (setf (protected-p newGenFun) nil)
                 (omNG-add-element (pack (editor self)) newGenFun)
                 ;(upDateUsermenu) 
                 ; plus la peine ? (le menu est mis a jour avec window activate)
                 )))
           ))
         ; new method for existing generic function
        ((equal (win-mod (editor self)) :new)
         (print "defining new method...")
         (let ((old-methods (copy-list (get-elements (fdefinition funname)))))
           (if (om-checked-p (second (quali-buttons (editor self)))) (setf thequaly (list :before)))
           (if (om-checked-p (third (quali-buttons (editor self)))) (setf thequaly (list :after)))
           (if (om-checked-p (fourth (quali-buttons (editor self)))) (setf thequaly (list :around)))
           (setf themethod (make-new-om-function funname lambda-list (length out-box) 
                                                 initvals doc nil thequaly out-box nil))
           (setf (create-info themethod) (list (om-get-date) (om-get-date)))
           (unless *abort-definition*
             (loop for met in old-methods do
                   (when (method-equal met themethod)
                     (when (mypathname met)
                             (om-delete-file (mypathname met)))))
             (setf (EditorFrame themethod) self)
             (when (EditorFrame (fdefinition funname))
               (om-close-window (window (EditorFrame (fdefinition funname)))))
             (setf pictlist (pictu-list (object self)))
             (setf (object self) themethod)
             )))
         
         (t   ; modify existing method
          (print "modifying method definition...")
          (setf thequaly (qualifieurs (object self))) 
          (setf themethod (handler-bind ((error #'(lambda (c) (setf *abort-definition* (print c)))))
                            (make-new-om-function funname lambda-list (length out-box) 
                                                  initvals doc nil thequaly out-box nil)))
          (unless *abort-definition*
            (setf (create-info themethod) (list (car (create-info (object self))) (om-get-date)))
            (when (mypathname (object self))
              (om-delete-file (mypathname (object self))))
            (setf (EditorFrame themethod) self)
            (when (EditorFrame (fdefinition funname))
              (om-close-window (window (EditorFrame (fdefinition funname)))))
            (setf pictlist (pictu-list (object (editor self))))
            (setf (object self) themethod)
            )
          ))
        
        (unless *abort-definition*
          (setf (pictu-list (object self)) pictlist)
          (put-boxes-in-method themethod (mapcar #'(lambda (item) (object item)) controls))
          (unless (class-method-p themethod)
            (if (get-real-container (fdefinition funname))
                (set-path-element (get-real-container (fdefinition funname)) themethod)
              (set-path-element (pack (editor self)) themethod))
            ))
        )))

  
;--------------------------------------------------
;INITS
;--------------------------------------------------

;; Panel-level IO buttons are suppressed; the generic-function zoom bar
;; (see OM-ZOOM-EDITOR-BAR-BUILD methodEditor below) hosts them instead.
(defmethod add-window-buttons ((self methodPanel)) nil)

#|
(defmethod add-output-enabled ((self methodPanel) type) nil)
(defmethod add-input-enabled ((self methodPanel) type) nil)
|#

(defmethod add-output-enabled ((self methodPanel) type) t)
(defmethod add-input-enabled  ((self methodPanel) type) t)

;; Generic-function methods take TYPED inputs; route ADD-INPUT through
;; MAKE-NEW-TYPED-INPUT so the generic zoom-bar IN button (and any other
;; caller) yields a typed-input by default.
(defmethod add-input ((self methodPanel) position)
  (when (add-input-enabled self 'in)
    (let* ((boxes (get-subframes self))
           (i     (length (find-class-boxes boxes 'TypedInFrame))))
      (multiple-value-bind (sx sy vw vh) (om-zoom-viewport-logical self)
        (declare (ignore vw vh))
        (let ((pos (or position
                       (om-make-point (+ sx 10 (* i 50))
                                      (+ sy 10)))))
          (omG-add-element self
                           (let ((*make-frame-zoom-context*
                                  (and (typep self 'om-scroller) (om-zoom-of self))))
                             (make-frame-from-callobj
                              (make-new-typed-input
                               (unique-name-from-list-new
                                "input" (get-elements (object self)) :mode :num :space nil)
                               't (+ i 1) pos))))
          (set-field-size self)
          t)))))


;;; DROP ON INPUT BUTTON --> CREATE TYPED INPUT

(defclass input-maker () ())

#|
(defmethod do-default-action ((self input-maker) icon)
  (let* ((thescroller (om-view-container icon))
         (boxes (get-subframes thescroller))
         (i (length (find-class-boxes boxes 'TypedInFrame)))
         (pos (om-make-point (+ 5 (* i 50)) 45)))
    (omG-add-element thescroller
                     (let ((*make-frame-zoom-context*
                            (and (typep thescroller 'om-scroller) (om-zoom-of thescroller))))
                       (make-frame-from-callobj
                        (make-new-typed-input (unique-name-from-list-new "input" (get-elements (object thescroller)) :mode :num :space nil)
                                              't (+ i 1) pos))))))
|#

(defmethod do-default-action ((self input-maker) icon)
  (let* ((thescroller (or (and (typep icon 'methodEditor-input-button)
                               (target-panel icon))
                          (om-view-container icon)))
         (boxes (get-subframes thescroller))
         (i (length (find-class-boxes boxes 'TypedInFrame)))
         (pos (om-make-point (+ 5 (* i 50)) 45)))
    (omG-add-element thescroller
                     (let ((*make-frame-zoom-context*
                            (and (typep thescroller 'om-scroller) (om-zoom-of thescroller))))
                       (make-frame-from-callobj
                        (make-new-typed-input (unique-name-from-list-new "input" (get-elements (object thescroller)) :mode :num :space nil)
                                              't (+ i 1) pos))))))


;;; SPECIAL CLASS
(defclass typed-input-button (unaire-fun-view om-icon-button) ())

;; Variant used by the methodEditor zoom bar: holds an explicit reference
;; to the target methodPanel so PERFORM-DROP can locate the panel where
;; the typed input should be added. Vanilla typed-input-button relies on
;; (om-view-container target) returning the panel, which is only true
;; when the button is a direct child of the panel.
(defclass methodEditor-input-button (typed-input-button)
  ((target-panel :initform nil :initarg :target-panel :accessor target-panel)))

;; PERFORM-DROP methods for methodEditor-input-button live in
;; kernel/graphics/dragdrop/performdrop.lisp, alongside the analogous
;; methods for typed-input-button — that file is loaded after OMDRAG-DROP
;; is defined.


(defun make-typed-input-from-obj (object panel &optional value)
  (let* ((thetype (class-name object))
         (boxes (get-subframes panel))
         (i (- (length (find-class-boxes boxes 'TypedInFrame)) 1))
         (pos (om-make-point (+ 5 (* (+ 1 i) 50)) 45))
         (new-input (make-new-typed-input (unique-name-from-list-new "input" (get-elements (object panel)) :mode :num :space nil)
                                          thetype (+ i 1) pos)))
    (when value (setf (defval new-input) (clone value)))
    (omG-add-element panel
                     (let ((*make-frame-zoom-context*
                            (and (typep panel 'om-scroller) (om-zoom-of panel))))
                       (make-frame-from-callobj new-input)))
    t))
    


#|
(defmethod newgenfun-add-window-buttons ((self methodPanel))
  (om-add-subviews self
                   (om-make-view 'typed-input-button
                                 :object (make-instance 'input-maker)
                                 :icon1 "in"
                                 :icon2 "in-pushed"
                                 :position (om-make-point 30 5)
                                 :size (om-make-point 24 24)
                                 :drop-action #'(lambda (item)
                                                  (make-typed-input-from-obj (object item) self)))
                   (om-make-view 'om-icon-button
                                 :icon1 "out"
                                 :icon2 "out-pushed"
                                 :position (om-make-point 5 5)
                                 :size (om-make-point 24 24)
                                 :action
                                 #'(lambda(item) (declare (ignore item))
                                     (let* ((boxes (get-subframes self))
                                            (i (- (length (find-class-boxes boxes 'outFrame)) 1))
                                            (pos (om-make-point (+ 5 (* (+ 1 i) 50)) 240)))
                                       (omG-add-element self
                                                        (let ((*make-frame-zoom-context*
                                                               (and (typep self 'om-scroller) (om-zoom-of self))))
                                                          (make-frame-from-callobj
                                                           (make-new-output (mk-unique-name self "output")
                                                                            (+ i 1) pos))))
                                       (set-field-size self)
                                       )))
                   ))

;; Above: panel-level buttons moved to the GENERIC-FUNCTION-ZOOM-BAR
;; built by OM-ZOOM-EDITOR-BAR-BUILD methodEditor (defined below).
|#

(defmethod om-zoom-editor-bar-build ((editor methodEditor))
  "Generic-function zoom bar: same widgets as the default patch bar, but the
   IN button is a TYPED-INPUT-BUTTON whose DROP-ACTION creates a typed input
   matching the dropped object's class (click still adds a generic typed input
   via ADD-INPUT methodPanel)."
  (let* ((panel (panel editor))
         (zoom  (if (typep panel 'om-scroller) (om-zoom-of panel) 1.0))
         (pct   (round (* zoom 100)))
         (bar (om-make-view 'generic-function-zoom-bar
                            :position (om-make-point 0 0)
                            :size (om-make-point (w editor) +om-zoom-bar-h+)
                            :bg-color *controls-color*))
         (out-btn (om-make-view
                   'om-icon-button
                   :position (om-make-point 5 1)
                   :size (om-make-point 24 24)
                   :icon1 "out"
                   :icon2 "out-pushed"
                   :action #'(lambda (item)
                               (declare (ignore item))
                               (modify-patch panel)
                               (add-output panel nil))))
         #|
         (in-btn (om-make-view
                  'methodEditor-input-button
                  :object (make-instance 'input-maker)
                  :target-panel panel
                  :position (om-make-point 30 1)
                  :size (om-make-point 24 24)
                  :icon1 "in"
                  :icon2 "in-pushed"
                  :action #'(lambda (item)
                              (declare (ignore item))
                              (modify-patch panel)
                              (add-input panel nil))))
         |#
         (in-btn (om-make-view
                  'methodEditor-input-button
                  :object (make-instance 'input-maker)
                  :target-panel panel
                  :position (om-make-point 30 1)
                  :size (om-make-point 24 24)
                  :icon1 "in"
                  :icon2 "in-pushed"))
         (zoom-bg (om-make-dialog-item
                   'om-static-text
                   (om-make-point 60 1)
                   (om-make-point 95 24)
                   ""
                   :font *om-default-font1*
                   :bg-color *controls-color*))
         (zoom-label (om-make-dialog-item
                      'om-static-text
                      (om-make-point 65 4)
                      (om-make-point 35 18)
                      "Zoom"
                      :font *om-default-font1*
                      :bg-color *controls-color*))
         (numbox (om-make-view
                  'om-zoom-pop-up
                  :position (om-make-point 105 4)
                  :size (om-make-point 60 18)
                  :value pct
                  :presets '(50 100 150 200 300 400)
                  :font *om-default-font1*
                  :bg-color *om-white-color*
                  :di-action #'(lambda (widget new-pct)
                                 (declare (ignore widget))
                                 (om-zoom-numbox-apply panel (/ new-pct 100.0))))))
    (om-add-subviews bar out-btn in-btn zoom-bg zoom-label numbox)
    (om-add-subviews editor bar)
    (when panel
      (setf (capi:capi-object-property panel :om-zoom-numbox) numbox))
    (setf (capi:capi-object-property editor :om-zoom-editor-bar) bar)
    bar))

; Editor to define a Generic Function for the first time
(defun make-new-genfunwin (self name iconid doc &optional (package *package-user*))
   (declare (ignore self))
   (let* ((new-win (make-editor-window 'methodEditor
                                       (omNG-make-new-patch "genfunpatch") name nil 
                                       :winsize (om-make-point 500 400) 
                                       :winpos (om-make-point 50 38)
                                       :winshow nil
                                       ))
       (thescroll (panel new-win))
       (editor (editor new-win)))
     (setf (pack editor) package)
     (setf (iconid editor) iconid)
     (setf (docu thescroll) doc)
     (setf (win-mod editor) :abs)
     (setf (name thescroll) name)
     (om-select-window new-win)
     ;; (newgenfun-add-window-buttons thescroll)  ; superseded by methodEditor zoom bar
     (set-field-size thescroll)
     ))

;Editor to define a new method for a defined Generic Function
(defmethod make-new-method ((self OMgenericFunction))
   (let* ((outputs (numouts self))
          (lambda-lis (arglist self))
          (ind 0) intype thescroll
          (new-win (om-make-window 'EditorWindow 
                     :window-title (string+ (name self) " (new method)")
                     :position (om-make-point 50 38) 
                     :close t
                     :window-show nil
                     :size  (om-make-point 400 330)))
          (editor (om-make-view 'MethodEditor
                    :ref nil
                    :owner new-win
                    :object (omNG-make-new-patch "genfunpatch")
                    :position (om-make-point 0 0) 
                    :size (om-make-point 400 330)))
          (pb (om-make-dialog-item 'om-radio-button (om-make-point 40 8) (om-make-point 76 16) "primary" :checked-p t))
          (bb (om-make-dialog-item 'om-radio-button  (om-make-point 120 8) (om-make-point 72 16) "before" ))
          (ab (om-make-dialog-item 'om-radio-button (om-make-point 200 8) (om-make-point 72 16) "after" ))
          (arb (om-make-dialog-item 'om-radio-button (om-make-point 280 8) (om-make-point 76 16) "around" )))
     (setf (editor new-win) editor)
     (setf thescroll (panel new-win))
     (setf (object thescroll) (omNG-make-new-patch "genfunpatch"))
     (setf (name thescroll) (name self))
     (setf (win-mod editor) :new)
     (om-add-subviews thescroll pb bb ab arb)
     (setf (quali-buttons editor) (list  pb bb ab arb))
     (loop for item in lambda-lis  do
           (if (not (member item lambda-list-keywords :test 'equal))
             (let* ((inbox (make-new-typed-input (string-downcase (string item))
                                                 't ind (om-make-point (+ 5 (* ind 50)) 45))))
               (setf (enable inbox) nil)
               (setf (keys inbox) intype)
               (omG-add-element thescroll
                                (let ((*make-frame-zoom-context*
                                       (and (typep thescroll 'om-scroller) (om-zoom-of thescroll))))
                                  (make-frame-from-callobj inbox)))
               (incf ind))
             (setf intype item)))
     (loop for i from 0 to  (- outputs 1) do
           (omG-add-element thescroll
                            (let ((*make-frame-zoom-context*
                                   (and (typep thescroll 'om-scroller) (om-zoom-of thescroll))))
                              (make-frame-from-callobj
                               (make-new-output (mk-unique-name thescroll "output")
                                                i (om-make-point (+ 5 (* i 50)) 240))))))
     ;;;new
     (set-field-size thescroll)
     (om-select-window new-win)
     ))

; (make-new-method (fdefinition 'sss))


