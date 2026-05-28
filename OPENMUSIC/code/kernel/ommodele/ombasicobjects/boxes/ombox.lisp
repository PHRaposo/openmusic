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
;This file implements the meta-object OMBox.
;There are two main subclasses of OMBox : OMboxCall and OMBoxClass.
;The first ones are boxes in a Path (Maquette), the second ones are class-references in a class hierarchical tree.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)

;-------------------------------------------------
;The  OM Call Objects is the more general class
;subclass this classe to create a new connectobject
;--------------------------------------------------
#|
(defclass OMBox (OMBasicObject) 
   ((inputs :initform nil :initarg :inputs :accessor inputs)
    (reference :initform nil :initarg :reference :accessor reference)
    (frame-position :initform nil  :accessor frame-position)
    (frame-size :initform nil  :accessor frame-size)
    (frame-name :initform nil  :accessor frame-name))
   (:documentation "The OM meta-object OMBox is is the more general class for connectable objects.
There are two main type the connectable objects OMboxCall and OMBoxClass.
The first ones are boxes in a Path (Maquette), the second ones are class-references in a class hierarchical tree.#enddoc#
#seealso# (OMboxCall OMBoxClass) #seealso#
#inputs# A list of input instances objects see input-funbox class.#inputs#
#reference# The reference specifies the box's type, for exemple if the reference is a class the box is a factory of instances,
if the reference is a generic function the box is a call of generic function. Differents sub-classes of OMBox are defined
in relation with the reference.#reference#
#frame-position# Store the position of the graphic frame. #frame-position#
#frame-size# Store the size of the graphic simple frame. #frame-size#
#frame-name# Store the name of the graphic simple frame, it is not necessary the same as the box.#frame-name#")
   (:metaclass omstandardclass))
|#

;--------------------------------------------------
;Method redefinition of OMpersistantObject
;--------------------------------------------------

;-------------------Protocole------------------------------
(defgeneric numouts (box)  
   (:documentation "Return the box outputs number."))

(defgeneric get-input-class-frame (box)  
   (:documentation "Specify the class of input's frame of the box."))

(defgeneric get-out-class (box)  
   (:documentation "Specify the class of output's frame of the box."))

(defgeneric get-frame-class (box)  
   (:documentation "Specify the class of the box's frame."))

(defgeneric get-frame-name (box)  
   (:documentation "get the name that will be showed in the box's frame."))

(defgeneric make-frame-from-callobj (box)
   (:documentation "Cons the frame that visualize the object 'box'."))

(defgeneric remove-extra (patch box)
   (:documentation "Called when you remove the box 'box' from the patch 'patch'."))

(defmethod omng-MoveObject ((self OMBox) newpos)
   (declare (ignore newpos))  t)

;------------------
(defmethod numouts ((self OMBox)) "The default value is 1" 1)
(defmethod get-input-class-frame ((self OMBox)) "The default value is 'input-funboxframe" 'input-funboxframe)
(defmethod get-out-class ((self OMBox)) "The default value is 'outfleche" 'outfleche)
(defmethod get-icon-box-class ((self OMBox)) 'icon-box)
(defmethod get-frame-class ((self OMBox)) "The default value is 'boxframe" 'boxframe)

(defmethod get-frame-name ((self OMBox))
   "NIL if the frame does not show a name (i.e. 'om+ function or editors)"
   (unless (function-without-name-p (reference self))
     (let ((thename (string (reference self))))
       (string-downcase thename))))

(defmethod remove-extra ((self OMPatch) (box OMBox)) "Sub-class this method." nil)

;;; donne le rapport de la taille de la boite en fonction du nombre d'inputs/outputs
(defmethod boxinputs-sizefactor ((self OMBox)) 10)


(defmethod spec-obj-icon-size ((self t)) nil)

(defmethod def-icon-size ((self ombox))
  (if (function-without-name-p (reference self))
      (let ((icn (second (get&corrige-icon (icon self)))))
        (list (om-pict-width icn) (om-pict-height icn)))
    (or (spec-obj-icon-size (reference self))
        ;'(24 24)
        nil
        )))

(defmethod spec-input-frame ((self OMBox) index) nil)

(defmethod get-box-documentation ((self OMBox))
  (get-documentation self))

(defmethod make-frame-from-callobj ((self OMBox))
  "This method is used by multiple sub-classes grace of polymorphic function as
'get-frame-class, 'get-input-class-frame, etc. Differentes boxes are comments or editor redifine this method."
  (let* ((icon (icon self))
         (iconsize (icon-sizes icon (def-icon-size self)))
         (name (if (frame-name self) (frame-name self) (get-frame-name self)))
         (boxnamefont *ombox-font*)
         (numouts (numouts self))
         (index 0)
         (size-name (round (get-name-size name boxnamefont)))
         (h-name (if name (+ 3 (om-string-h boxnamefont)) 0))
         ;; ZOOM-SCALE: pick up destination panel zoom for new-frame metrics.
         (zoom (or *make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         input-frames module boxframex)

    (setf (inputs self) (update-inputs (reference self) (inputs self)))

    (setf boxframex (if (frame-size self)
                        (om-point-h (frame-size self))
                      (apply #'max (list (first iconsize)
                                         (* (boxinputs-sizefactor self) numouts)
                                         (* (boxinputs-sizefactor self) (length (inputs self)))
                                         size-name))))

    (let* ((module-logical-size (om-make-point boxframex (+ (second iconsize) 19 h-name)))
           (module-logical-pos  (frame-position self))
           (module-vsize (if scale-p (om-zoom-scale-point module-logical-size zoom) module-logical-size))
           (module-vpos  (if (and scale-p module-logical-pos)
                             (om-zoom-scale-point module-logical-pos zoom)
                             module-logical-pos))
           (io-size-v (if scale-p (max 1 (round (* 8 zoom))) 8))
           (io-y-v    (if scale-p (max 0 (round (* 1 zoom))) 1)))

      (setf input-frames
            (mapcar #'(lambda (input)
                        (let ((docstr (doc-string input)))
                          (setf index (+ index 1))
                          (let* ((x-log (- (* index (round boxframex (+ (length (inputs self)) 1))) 4))
                                 (x-v   (if scale-p (round (* x-log zoom)) x-log)))
                            (om-make-view (or (spec-input-frame self (- index 1)) (get-input-class-frame self))
                                          :object input
                                          :help-spec (string+ "<" (string-downcase (name input))
                                                              ">" (if (and docstr (not (string-equal docstr "")))
                                                                      (string+ " " (doc-string input)) ""))
                                          :size (om-make-point io-size-v io-size-v)
                                          :position (om-make-point x-v io-y-v)))))
                    (inputs self)))

      (setq module
            (om-make-view (get-frame-class self)
                          :position module-vpos
                          :size module-vsize
                          :object self
                          :subviews input-frames))

      (setf (inputframes module) input-frames)
      (make-outputs-of-frame self module)
      (setf (outframes module) (reverse (outframes module)))
      (setf (name module) name)
      (setf (frames self) (list module))
      ;; ZOOM-CTX: stamp logical metrics on the frame.
      ;; Stamp LOGICAL on the FRAME (not on the OBJECT) so frame-size of
      ;; OBJECT stays NIL until the user resizes; otherwise draw-before-box
      ;; would draw the gray rect on a fresh box.
      (setf (om-zoom-logical-size module) module-logical-size)
      (when module-logical-pos
        (setf (om-zoom-logical-pos module) module-logical-pos))

      (let* ((iw-v (if scale-p (max 1 (round (* (first iconsize) zoom))) (first iconsize)))
             (ih-v (if scale-p (max 1 (round (* (second iconsize) zoom))) (second iconsize)))
             (ix-v (round (- (om-point-h module-vsize) iw-v) 2))
             (iy-v (if scale-p (max 0 (round (* 10 zoom))) 10)))
        (om-add-subviews module (setf (iconView module)
                                      (om-make-view (get-icon-box-class self)
                                                    :iconID icon
                                                    :help-spec (get-box-documentation self)
                                                    :size (om-make-point iw-v ih-v)
                                                    :position (om-make-point ix-v iy-v))))
        (when name
          (let* ((scaled-font (if scale-p (om-zoom-scale-font boxnamefont zoom) boxnamefont))
                 (real-text-h (max 1 (om-string-h scaled-font)))
                 (nw-v (if scale-p (max 1 (round (* size-name zoom))) size-name))
                 (nh-v (+ 3 real-text-h))
                 (nx-v (+ ix-v (round (- iw-v nw-v) 2)))
                 (icon-bottom-v (+ iy-v ih-v))
                 (ny-v (- icon-bottom-v 1))
                 (nameview-instance
                  (om-make-dialog-item 'box-dialog-name
                                       (om-make-point nx-v ny-v)
                                       (om-make-point nw-v nh-v)
                                       name
                                       :value name
                                       :font scaled-font
                                       :help-spec (get-documentation self))))
            (when scale-p
              (setf (om-zoom-logical-font nameview-instance) boxnamefont))
            (om-add-subviews module (setf (nameView module) nameview-instance))))
        (when (allow-lock self)
          (add-lock-button module (allow-lock self)))
        (add-box-resize module)
        module))))

;--------------------------------------------------
;Other methods
;--------------------------------------------------

(defmethod is-connected? ((self OMBox) (source OMBox))
   "T if one input of 'self' is connected to one output of 'source'."
   (let ((list (inputs source)) rep)
     (loop while list do
           (let ((connec (connected? (pop list))))
             (when (and connec (equal (first connec) self))
               (setf rep t)
               (setf list nil))))
     rep))


(defmethod unconnected ((self OMBox) (source OMBox))
   "T if  inputs of 'self' are not connected."
   (let ((list (inputs source)))
     (loop for input in list do
           (let ((connec (connected? input)))
             (when (and connec (equal (first connec) self))
               (setf (connected? input) nil))))))

(defmethod get-output-text ((self t) i) "option-click to evalue or drag for connections")



(defmethod make-outputs-of-frame ((self OMBox) module)
  "Cons a list of views which are the outputs of the box."
  ;; (w module) and (h module) are already VISUAL (module was built with
  ;; pre-scaled :size in make-frame-from-callobj); only the literal offsets
  ;; (4, 9) and io-size (8) need scaling.
  ;; ZOOM-SCALE: literal offsets (4, 9) and io-size (8) scale with zoom.
  (let* ((numouts (numouts self))
         (zoom    (or *make-frame-zoom-context* 1.0))
         (scale-p (and (numberp zoom) (/= zoom 1.0)))
         (io-size (if scale-p (max 1 (round (* 8 zoom))) 8))
         (off-x   (if scale-p (round (* 4 zoom)) 4))
         (off-y   (if scale-p (round (* 9 zoom)) 9)))
    (loop for i from 0 to (- numouts 1) do
          (let ((thenewout (om-make-view (get-out-class self)
                                         :position (om-make-point
                                                    (- (* (+ i 1) (round (w module) (+ numouts 1))) off-x)
                                                    (- (h module) off-y))
                                         :size (om-make-point io-size io-size)
                                         :help-spec (get-output-text self i)
                                         :index i)))
            (push thenewout (outframes module))
            (om-add-subviews module thenewout)))))


(defmethod omNG-make-alias ((self OMBox))
   "Generic function to make boxes alias the method 'allow-alias' filters some type of boxes 
(i.e. functions non allow alias)."
   (if (allow-alias self)
     (omNG-make-new-boxalias self (om-add-points (om-make-point 20 20) (frame-position self))
                             (string+ (name self) "-alias")) 
     (not (dialog-message (string+ 
                           (get-object-insp-name self)
                           " objects don't accept alias")))))

(defmethod omng-make-new-boxalias ((self OMBox) posi name &optional (protect nil))
   (let* ((rep (make-instance 'OMBoxAlias 
                 :name name
                 :reference self 
                 :icon (icon self)
                 :protected-p protect)))
     (setf (frame-position rep) (borne-position posi))
     (push rep (frames self))
     (push rep (attached-objs (find-class (get-reference rep))))
     rep))

(defmethod omng-make-new-boxalias ((self OMClass) posi name &optional (protect nil))
   (let* ((rep (make-instance 'OMBoxAlias 
                 :name name
                 :reference self 
                 :icon (icon self)
                 :protected-p protect)))
     (setf (frame-position rep) (borne-position posi))
     (push rep (attached-objs self))
     rep))



