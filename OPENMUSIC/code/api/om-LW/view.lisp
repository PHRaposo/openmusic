;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Jean Bresson, Carlos Agon, Karim Haddad
;=========================================================================


;;===========================================================================
;DocFile
; OM-VIEW CLASSES : container panes for GUI windows
;DocFile
;;===========================================================================

(export '(

          om-view
          om-make-view
          om-transparent-view
          om-scroller
          om-field-size
          om-set-field-size
          om-scroll-position
          scroll-update
          om-set-scroll-position
          om-move-scroll-position
          om-h-scroll-position
          om-v-scroll-position
          om-set-h-scroll-position
          om-set-v-scroll-position
          om-view-scrolled
          update-for-subviews-changes
          om-view-contains-point-p
          om-find-view-containing-point
          om-convert-coordinates
          om-view-origin) :om-api)



(in-package :oa)


;;;======================
;;; VIEW
;;; General graphic pane
;;;======================

(defclass om-view (om-graphic-object capi::pinboard-layout)
  ((item-subviews :initarg :item-subviews :initform nil :accessor item-subviews)
   (main-pinboard-object :accessor main-pinboard-object :initarg :main-pinboard-object :initform nil))
  (:default-initargs
   ;; #+win32 :draw-pinboard-objects #+win32  :once ;:local-buffer :buffer :once 
   :draw-with-buffer t
   :highlight-style :standard
   :scroll-if-not-visible-p nil
   :fit-size-to-children nil
   ))

(defmethod om-view-p ((self om-view)) t)
(defmethod om-view-p ((self t)) nil)

;;; background pinboard for drawing on the view
;;; must keep the same size as the view
(defclass om-view-pinboard-object (capi::pinboard-object) ())

(defmethod om-get-view ((self om-view-pinboard-object)) (capi::element-parent self))
;;; GET THE VIEW WE SHOULD USE IN INTERACTIONS
(defmethod om-get-real-view ((self om-view-pinboard-object)) (capi::element-parent self))

(defmethod set-layout ((view om-view))
  (setf (capi:layout-description view)
        (remove-duplicates (remove nil (cons (main-pinboard-object view)
                                             (append (vsubviews view) (item-subviews view))))))
  ;; this fixed the problem with dialog item box display on windows...
  ;; #+mswindows (setf (static-layout-child-position view) (values (vx view) (vy view)))
  )

(defmethod set-layout ((view t)) nil)

(defmethod update-for-subviews-changes ((self om-view) &optional (recursive nil))
  (apply-in-pane-process self (lambda () (set-layout self)))
  (when recursive (mapc #'(lambda (view) (if (om-view-p view) (update-for-subviews-changes view t)))
                        (vsubviews self))))

(defmacro om-make-view (class &rest attributes
                        &key (position (om-make-point 0 0)) (size (om-make-point 32 32))
			owner subviews name font bg-color scrollbars retain-scrollbars field-size 
                        &allow-other-keys)
  `(let*  ((x (om-point-h ,position))
           (y (om-point-v ,position))
           (w (om-point-h ,size))
           (h (om-point-v ,size))
           (fw (if ,field-size (om-point-h ,field-size) 0))
           (fh (if ,field-size (om-point-v ,field-size) 0))
           (view (make-instance ,class
                               :name ,name
                               ;:x x :y y ;; for pinboard-objects
                               :width nil :height nil
                               :default-x x :default-y y :default-width w :default-height h
                               :visible-min-width nil :visible-min-height nil
                               :horizontal-scroll (or (equal ,scrollbars t) (equal ,scrollbars :h))
                               :vertical-scroll (or (equal ,scrollbars t) (equal ,scrollbars :v))
                               :scroll-width fw :scroll-height fh
                               :accepts-focus-p t
                               :vx x :vy y :vw w :vh h
                               :vcontainer ,owner
                               :allow-other-keys t
                               ;#+linux :foreground #+linux :black
                               ,.attributes
                               )))
     (when ,bg-color (om-set-bg-color view ,bg-color))
     (when ,owner (om-add-subviews ,owner view))
     (when ,subviews (mapc (lambda (sv) (om-add-subviews view sv)) ,subviews))
     
     (when (om-view-p view) 
       #+win32(unless (or ,bg-color (simple-pane-background view)) (om-set-bg-color view *om-white-color*))
       #+win32(setf (main-pinboard-object view) (make-instance 'om-view-pinboard-object))
       #+win32(setf (capi::static-layout-child-size (main-pinboard-object view)) (values w h))
       #+(or cocoa linux) (setf (capi::output-pane-display-callback view) 'om-draw-contents-callback)
       )
     (when (scroller-p view) 
       (setf (capi::simple-pane-scroll-callback view) 'scroll-update)
       (setf (fw view) fw (fh view) fh))
     (when (om-item-view-p view) 
       (om-set-font view (or ,font *om-default-font1*)))
     view))

(defmethod om-create-callback ((self om-view))
  (update-for-subviews-changes self nil)
  (setf (initialized-p self) t))

(defmethod om-set-view-position ((self om-graphic-object) pos-point) 
  (capi:apply-in-pane-process 
   self #'(lambda ()
            (setf (static-layout-child-position self) 
                  (values (om-point-h pos-point) (om-point-v pos-point)))))
  ;(set-hint-table self (list :default-x (om-point-h pos-point) :default-x (om-point-v pos-point))))
  (setf (vx self) (om-point-h pos-point)
        (vy self) (om-point-v pos-point)))

(defmethod om-view-position ((self om-graphic-object))
  (if (capi::interface-visible-p self)
      (capi:apply-in-pane-process self #'(lambda ()
             (multiple-value-bind (x y) (static-layout-child-position self)
               (setf (vx self) x) (setf (vy self) y)))))
  (om-make-point (vx self) (vy self)))

(defmethod om-set-view-size ((self om-graphic-object) size-point)
  (let ((w (or (om-point-h size-point) (vw self)))
        (h (or (om-point-v size-point) (vh self))))
    (capi:apply-in-pane-process self 
                                #'(lambda ()                                  
                                    (setf (static-layout-child-size self) (values  w h))
                                    #+win32(setf (static-layout-child-size (main-pinboard-object self)) (values w h))
                                    
                                    (set-hint-table self (list :default-width (om-point-h size-point)
                                                               :default-height (om-point-v size-point)))
                                    ))
    (setf (vw self) w)
    (setf (vh self) h))
  #+linux(om-invalidate-view self t);;;ICI
)

(defmethod om-view-size ((self om-graphic-object)) 
  (if (interface-visible-p self)
      (capi:apply-in-pane-process self #'(lambda ()
                (multiple-value-bind (w h) (static-layout-child-size self)
                  (setf (vw self) w)
                  (setf (vh self) h)))))
    (om-make-point (vw self) (vh self)))



;;;======================
;;; OM-TRANSPARENT-VIEW
;;;======================
(defclass om-transparent-view (om-view) ()
  (:default-initargs :background :transparent))

#-(or linux cocoa)
(defmethod (setf vcontainer) :around ((cont om-graphic-object) (view om-transparent-view)) 
  (call-next-method)
  (om-set-bg-color view (om-get-bg-color cont))
  (mapc #'(lambda (v) (setf (vcontainer v) view)) (om-subviews view)))

#-(or linux cocoa)
(defmethod (setf vcontainer) :around ((cont om-graphic-object) (view om-view)) 
  (call-next-method)
  (when (or (null (om-get-bg-color view)) (equal :transparent (c (om-get-bg-color view))))
    (om-set-bg-color view (om-get-bg-color cont))
    (mapc #'(lambda (v) (setf (vcontainer v) view)) (om-subviews view))))


;;;======================
;;; OM-SCROLLER
;;;======================
(defclass om-scroller (om-view)
   ((fw :initform 0 :initarg :fw :accessor fw)
    (fh :initform 0 :initarg :fh :accessor fh)
    (scrollbars :initform t :initarg :scrollbars :accessor scrollbars)
    (retain-scrollbars :initform nil :initarg :retain-scrollbars :accessor retain-scrollbars)
    (displayed-p :initform nil :initarg :displayed-p :accessor displayed-p)
    (tracked-scroll-x :initform 0 :accessor tracked-scroll-x)
    (tracked-scroll-y :initform 0 :accessor tracked-scroll-y)
    )
   ;(:default-initargs :simple-pane-scroll-callback 'scroll-update)
   (:default-initargs :background :white)
   )

(declaim (special *om-zoom-in-progress-p*))

;;; :simple-pane-scroll-callback
(defmethod scroll-update ((self om-scroller) dimension operation pos-list &rest options)
  (declare (ignore options))
  ;; Skip axis-only re-emits that auto-scroll fires during om-zoom-update;
  ;; they would clobber the tracked scroll with stale single-axis values.
  (when (and *om-zoom-in-progress-p*
             (member dimension '(:horizontal :vertical))
             (numberp pos-list))
    (return-from scroll-update nil))
  ;; Mirror pos-list into tracked-scroll on every :move event. CAPI's
  ;; :slug-position lags the real pan on Cocoa auto-scroll pinboard-layout,
  ;; so the tracked pair is the authoritative source for om-zoom anchoring.
  (when (eq operation :move)
    (cond
     ((and (consp pos-list) (numberp (first pos-list)) (numberp (second pos-list)))
      (setf (tracked-scroll-x self) (first pos-list)
            (tracked-scroll-y self) (second pos-list)))
     ((and (eq dimension :horizontal) (numberp pos-list))
      (setf (tracked-scroll-x self) pos-list))
     ((and (eq dimension :vertical) (numberp pos-list))
      (setf (tracked-scroll-y self) pos-list))))
  (case dimension
    (:vertical (setf pos-list (list 0 pos-list)))
    (:horizontal (setf pos-list (list pos-list 0)))
    (otherwise nil))
  (case operation
    (:move
     (when (and (car pos-list) (cadr pos-list))
       (let ((x (om-h-scroll-position self))
             (y (om-v-scroll-position self)))
         (om-invalidate-rectangle self x y (vw self) (vh self))
         #+win32(setf (static-layout-child-position (main-pinboard-object self))
				  (values x y))
         ))
     (om-view-scrolled self (car pos-list) (cadr pos-list)))
    #+win32 (:step
			(setf (static-layout-child-position (main-pinboard-object self)) 
			      (values (om-h-scroll-position self) (om-v-scroll-position self)))
			(om-view-scrolled self (om-h-scroll-position self) (om-v-scroll-position self))
			;;(om-invalidate-view self)
			)
    #+win32 (otherwise ;; :drag
			(setf (static-layout-child-position (main-pinboard-object self)) 
			      (values (om-h-scroll-position self) (om-v-scroll-position self)))
			(om-view-scrolled self (car pos-list) (cadr pos-list))
			;;(om-invalidate-view self)
			)
    ))

(defmethod om-view-scrolled ((self om-scroller) x y)
;#+linux(om-invalidate-view self t)
 nil)

(defmethod om-view-size ((self om-scroller)) 
  (call-next-method))

(defmethod om-set-view-size ((self om-scroller) size-point)
  (capi:apply-in-pane-process self  #'(lambda ()
       (setf (static-layout-child-size self) (values (om-point-h size-point) (om-point-v size-point)))
       ;; pas la peine ??
       #+win32(setf (static-layout-child-size (main-pinboard-object self)) (values (om-point-h size-point) (om-point-v size-point)))
       ))
  (setf (vw self) (om-point-h size-point))
  (setf (vh self) (om-point-v size-point)))

(defmethod scroller-p ((self om-scroller)) t)
(defmethod scroller-p ((self t)) nil)

(defmethod om-field-size ((self om-scroller))
  (if (and (interface-visible-p self) (not (locked self)))
      (om-make-point (or (capi::get-horizontal-scroll-parameters self :max-range) (vw self))
                     (or (capi::get-vertical-scroll-parameters self :max-range) (vh self)))
    (om-make-point (fw self) (fh self))))

(defmethod om-field-size ((self om-graphic-object))
  (om-view-size self))

(defmethod om-set-field-size ((self om-scroller) size)
  (setf (fw self) (om-point-h size) (fh self) (om-point-v size))
  ;(print (list self (om-point-v size) (om-height self)))
  (capi:apply-in-pane-process self #'(lambda ()
                                       (when (capi::simple-pane-horizontal-scroll self)
                                         (capi::set-horizontal-scroll-parameters self :min-range 0 :max-range (om-point-h size)))
                                       (when (capi::simple-pane-vertical-scroll self)
                                         (capi::set-vertical-scroll-parameters self :min-range 0 :max-range (om-point-v size)))))
  t)

#|
(capi::contain 
 (let ((pl (make-instance 'capi::pinboard-layout
                              :vertical-scroll t
                              :horizontal-scroll nil
                              :scroll-width 200 :scroll-height 200)))
   (capi:apply-in-pane-process pl #'(lambda ()
                                  (capi::set-horizontal-scroll-parameters pl :min-range 0 :max-range 200)
                                  (capi::set-vertical-scroll-parameters pl :min-range 0 :max-range 200)
                                  ))
   pl))
|#

;;MICRO-SCROLL ISSUE:
;pour empecher les micro-scroll sous linux -- a voir...

;#-linux
(defmethod om-scroll-position ((self om-scroller))
  (om-make-point (om-h-scroll-position self) (om-v-scroll-position self)))

;#+linux
;(defmethod om-set-scroll-position ((self om-scroller) pos) nil)


(defmethod om-set-scroll-position ((self t) pos) nil)

;#+(or cocoa win32)
(defmethod om-set-scroll-position ((self om-scroller) pos)
  (setf (tracked-scroll-x self) (om-point-h pos)
        (tracked-scroll-y self) (om-point-v pos))
  (capi::apply-in-pane-process
   self
   'capi::scroll self :pan :move
   (list (om-point-h pos) (om-point-v pos))))

#|
#+linux
;pour empecher les micro-scroll sous linux
(defmethod om-set-scroll-position ((self om-scroller) pos) nil)
|#

;#+linux
;for scrolling shortcuts only
(defmethod om-move-scroll-position ((self om-scroller) pos)
  (setf (tracked-scroll-x self) (om-point-h pos)
        (tracked-scroll-y self) (om-point-v pos))
  (capi::apply-in-pane-process
   self
   'capi::scroll self :pan :move
   (list (om-point-h pos) (om-point-v pos))))

(defmethod om-h-scroll-position ((self om-scroller))
  (or (capi::get-horizontal-scroll-parameters self :slug-position) 0))

(defmethod om-v-scroll-position ((self om-scroller))
  (or (capi::get-vertical-scroll-parameters self :slug-position) 0))

(defmethod om-set-h-scroll-position ((self om-scroller) pos)
  (setf (tracked-scroll-x self) pos)
  (capi::set-horizontal-scroll-parameters self :slug-position pos))

(defmethod om-set-v-scroll-position ((self om-scroller) pos)
  (setf (tracked-scroll-y self) pos)
  (capi::set-vertical-scroll-parameters self :slug-position pos))

;;; au cas ou...
(defmethod om-h-scroll-position ((self om-graphic-object)) 0)
(defmethod om-v-scroll-position ((self om-graphic-object)) 0)
(defmethod om-scroll-position ((self om-graphic-object)) (om-make-point 0 0))

(defmethod om-set-h-scroll-position ((self om-graphic-object) pos) t)

(defmethod update-for-subviews-changes ((self om-scroller) &optional (recursive nil))
  (if (initialized-p self)
      (capi::apply-in-pane-process self (lambda ()
					(let ((v (capi::get-vertical-scroll-parameters self :max-range))
                                              (h (capi::get-horizontal-scroll-parameters self :max-range))
                                              (x (capi::get-horizontal-scroll-parameters self :slug-position))
                                              (y (capi::get-vertical-scroll-parameters self :slug-position)))
                                          (set-layout self)
                                          (capi::set-vertical-scroll-parameters self :min-range 0 :max-range v)
                                          (capi::set-horizontal-scroll-parameters self :min-range 0 :max-range h)
                                          (capi::scroll self :pan :move (list x y))
                                          (setf (tracked-scroll-x self) x
                                                (tracked-scroll-y self) y)
                                          )))
    (apply-in-pane-process self (lambda () (set-layout self))))
    (when recursive (mapc #'(lambda (view) (if (om-view-p view) (update-for-subviews-changes view t))) (vsubviews self)))
    )

;;; pour faire appaitre les scrollbars
;;; !!! plus appellee
(defmethod om-draw-contents-callback :before ((self om-scroller) x y w h)
  (unless (displayed-p self)
    (om-window-resized (om-view-window self) (om-view-size (om-view-window self)))
    (setf (displayed-p self) t)))

;;;(om-subtract-points (om-view-size self) (om-make-point 30 30))

#+win32
(defmethod om-interior-size ((self om-scroller))
  (om-subtract-points 
   (om-view-size self)
   (om-make-point (if (and (capi::simple-pane-vertical-scroll self) 
                           (> (capi::get-vertical-scroll-parameters self :max-range) (vh self)))  17 0)
                  (if (and (capi::simple-pane-horizontal-scroll self)
                           (> (capi::get-horizontal-scroll-parameters self :max-range) (vw self))) 17 0))))

;;;======================
;;; TOOLS
;;;======================
(defmethod om-view-contains-point-p ((view om-graphic-object) point)
  (let* ((x (om-point-h point))
        (y (om-point-v point))
        (brx  (om-width view))
        (bry  (om-height view))
        (vpos (om-view-position view))
        (rx  (+ (om-point-h vpos) (om-h-scroll-position view)))
        (ry  (+ (om-point-v vpos) (om-v-scroll-position view)))
        )
    (and (> x rx) (> y ry) (< x (+ brx rx)) (< y (+ bry ry)))))

(defun om-find-view-containing-point (view point &optional (recursive t))
  (if view 
      (let ((subviews (om-subviews view)))
	(do ((i (- (length subviews) 1) (- i 1)))
            ((< i 0))
          (let ((subview (nth i subviews)))
            (when (om-view-contains-point-p subview point)
	      (return-from om-find-view-containing-point
                (progn
		  (when recursive (om-find-view-containing-point subview (om-convert-coordinates point view subview))))
                ))))
        view)))

(defun om-convert-coordinates (point view1 view2)           
  (if (and view1 view2)
       (om-add-points point
                      (om-subtract-points (om-view-origin view2)
                                          (om-view-origin view1)))
   (progn
      (print (format nil "Warning: Can not convert position with NULL views: ~A, ~A." view1 view2))
      point)))
  
;  (if (and (initialized-p view1) (initialized-p view2))
;    (multiple-value-bind (x y)
;        (capi::convert-relative-position view1 view2 (om-point-h point) (om-point-v point))
;      (om-make-point x y))
;    (om-add-points point
;                   (om-subtract-points (om-view-origin view2)
;                                       (om-view-origin view1))))

(defun om-view-origin (view)
   (let ((container (om-view-container view)))
     (if container
       (let ((position (om-view-position view))
             (container-origin (om-view-origin container)))
         (if position
	     (om-subtract-points container-origin position)
           container-origin))
       (om-make-point 0 0)
       )))


(defun om-view-absolute-position (view)
   (let ((container (om-view-container view)))
     (if container
       (let ((position (om-view-position view))
             (container-origin (om-view-origin container)))
         (if position
	     (om-add-points container-origin position)
           container-origin))
       (om-make-point 0 0)
       )))


(defun find-pane-with-focus (layout)
  (capi:map-pane-descendant-children 
   layout
   #'(lambda (p)
       (when (capi:pane-has-focus-p p)
         (return-from find-pane-with-focus p)))))


;;;=============================================

(defclass om-tab-layout (capi::tab-layout om-graphic-object) ())

(defun print-tab-name (pane)
  (format nil " ~A " (or (capi::capi-object-name pane) "Untitled Tab")))

(defun om-make-tab-layout (view-list &key (size (om-make-point 200 200)) (position (om-make-point 0 0))
                                     (selection 0))
  (let ((x (om-point-v position))
        (y (om-point-v position))
        (w (om-point-h size))
        (h (om-point-v size))
        tl)
    (setf tl (make-instance 'om-tab-layout
                   :width nil :height nil
                   :default-x x :default-y y :default-width w :default-height h
                   :visible-min-width nil :visible-min-height nil
                   :accepts-focus-p t
                   :vx x :vy y :vw w :vh h
                   :items view-list
                   :print-function #'print-tab-name
                   :visible-child-function 'identity
                   :selected-item (nth selection view-list)
                   ))
    tl))

(defmethod om-current-view ((self om-tab-layout))
  (capi::tab-layout-visible-child self))

(export '(om-tab-layout om-make-tab-layout om-current-view) :om-api)


;;;======================
;;; ZOOM
;;;======================

(defparameter +om-zoom-min+ 0.5)
(defparameter +om-zoom-max+ 4.0)

(defvar *om-zoom-default* 1.0)
(defvar *om-zoom-gesture-sensitivity* 1.0)
(defvar *om-zoom-trackpad-boost* 1.0)

(defvar *om-zoom-in-progress-p* nil)
(defvar *om-zoom-unscale-mouse-pos-p* nil)

(defvar *om-zoom-diag-events-p* nil)
(defvar *om-zoom-diag-apply-p* nil)

(defvar *om-zoom-mini-helper-scale* 1.0)

(defun om-zoom-log-error (where c)
  "Log condition C to the OM listener tagged with WHERE."
  (let ((s (or om-lisp:*om-stream* *standard-output*)))
    (format s "~&[om-zoom] ERROR in ~A: ~A~%   condition-type=~S~%"
            where (princ-to-string c) (type-of c))
    (finish-output s)))

(defun om-zoom-diag-log (fmt &rest args)
  (when *om-zoom-diag-events-p*
    (let ((s (or om-lisp:*om-stream* *standard-output*)))
      (apply #'format s
             (concatenate 'string "~&[om-zoom-diag] " fmt "~%")
             args)
      (finish-output s))))

(defun om-clamp-zoom (z)
  "Clamp Z to [+om-zoom-min+, +om-zoom-max+]."
  (min +om-zoom-max+ (max +om-zoom-min+ z)))

(defun om-zoom-scale-point (pt factor)
  "PT scaled by FACTOR, rounded to integer coords."
  (om-make-point (round (* factor (om-point-h pt)))
                 (round (* factor (om-point-v pt)))))

(defun om-zoom-unscale-point (pt factor)
  "PT divided by FACTOR, rounded to integer coords."
  (om-make-point (round (/ (om-point-h pt) factor))
                 (round (/ (om-point-v pt) factor))))

(defun om-zoom-scale-font (font zoom)
  "FONT with size scaled by ZOOM; FONT returned unchanged on error."
  (handler-case
      (let* ((desc  (if (typep font 'gp:font-description)
                        font
                        (gp:font-description font)))
             (attrs (gp:font-description-attributes desc))
             (size  (or (getf attrs :size) 12)))
        (gp:augment-font-description desc :size (max 1 (round (* size zoom)))))
    (error (c) (om-zoom-log-error :scale-font c) font)))

(defgeneric om-zoom-of (pane)
  (:documentation "Current zoom factor of PANE."))

(defmethod om-zoom-of ((pane t))
  (or (capi:capi-object-property pane :om-zoom-factor) *om-zoom-default*))

(defgeneric (setf om-zoom-of) (value pane))

(defmethod (setf om-zoom-of) (value (pane t))
  (setf (capi:capi-object-property pane :om-zoom-factor) value))

(defun om-zoom-transformed-p (pane)
  "T iff PANE has a non-identity zoom transform."
  (/= (om-zoom-of pane) 1.0))

(defun om-zoom-find-ancestor-zoom (frame)
  "Zoom factor of FRAME's nearest om-scroller ancestor, or session default."
  (loop for f = frame then (om-view-container f)
        while f
        when (typep f 'om-scroller) return (om-zoom-of f)
        finally (return *om-zoom-default*)))

(defun om-zoom-effective (self)
  "Zoom factor that applies to SELF."
  (cond
   ((typep self 'om-scroller) (om-zoom-of self))
   (t (let ((pane (om-view-container self)))
        (cond
         ((typep pane 'om-scroller) (om-zoom-of pane))
         (pane (om-zoom-find-ancestor-zoom pane))
         (t *om-zoom-default*))))))

(defun om-zoom-capture-logical-geom (frame)
  "Snapshot FRAME's current pos and size as the LOGICAL reference; idempotent."
  (unless (om-zoom-logical-pos frame)
    (setf (om-zoom-logical-pos frame) (om-view-position frame)))
  (unless (om-zoom-logical-size frame)
    (setf (om-zoom-logical-size frame) (om-view-size frame))))

(defun om-zoom-capture-logical-font (frame)
  "Snapshot FRAME's current font as the LOGICAL reference; idempotent."
  (unless (om-zoom-logical-font frame)
    (let ((current (om-get-font frame)))
      (when current
        (setf (om-zoom-logical-font frame) current)))))

(defgeneric om-zoom-applies-p (pane)
  (:documentation "T iff zoom gestures apply to PANE."))

(defmethod om-zoom-applies-p ((pane t))   t)
(defmethod om-zoom-applies-p ((pane null)) nil)

(declaim (special *om-default-font0* *om-default-font1* *om-default-font2*
                  *om-default-font3* *om-default-font4*
                  *om-default-font1b* *om-default-font2b*
                  *om-default-font3b* *om-default-font4b*
                  *ombox-font* *icon-size-factor* *miniview-font-size*))

(defun om-current-default-font0 (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font0*) (= zoom 1.0))
        *om-default-font0*
        (om-zoom-scale-font *om-default-font0* zoom))))

(defun om-current-default-font1 (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font1*) (= zoom 1.0))
        *om-default-font1*
        (om-zoom-scale-font *om-default-font1* zoom))))

(defun om-current-default-font2 (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font2*) (= zoom 1.0))
        *om-default-font2*
        (om-zoom-scale-font *om-default-font2* zoom))))

(defun om-current-default-font3 (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font3*) (= zoom 1.0))
        *om-default-font3*
        (om-zoom-scale-font *om-default-font3* zoom))))

(defun om-current-default-font4 (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font4*) (= zoom 1.0))
        *om-default-font4*
        (om-zoom-scale-font *om-default-font4* zoom))))

(defun om-current-default-font1b (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font1b*) (= zoom 1.0))
        *om-default-font1b*
        (om-zoom-scale-font *om-default-font1b* zoom))))

(defun om-current-default-font2b (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font2b*) (= zoom 1.0))
        *om-default-font2b*
        (om-zoom-scale-font *om-default-font2b* zoom))))

(defun om-current-default-font3b (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font3b*) (= zoom 1.0))
        *om-default-font3b*
        (om-zoom-scale-font *om-default-font3b* zoom))))

(defun om-current-default-font4b (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font4b*) (= zoom 1.0))
        *om-default-font4b*
        (om-zoom-scale-font *om-default-font4b* zoom))))

(defun om-current-ombox-font (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *ombox-font*) (= zoom 1.0))
        *ombox-font*
        (om-zoom-scale-font *ombox-font* zoom))))

(defun om-current-icon-size-factor (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (= zoom 1.0)
        *icon-size-factor*
        (* *icon-size-factor* zoom))))

(defun om-current-miniview-font-size (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (= zoom 1.0)
        *miniview-font-size*
        (max 1 (round (* *miniview-font-size* zoom))))))

#+win32
(defun om-zoom-touch-anchor (pane x y)
  (declare (ignore pane))
  (values x y))

#-win32
(defun om-zoom-touch-anchor (pane x y)
  (values (- x (tracked-scroll-x pane))
          (- y (tracked-scroll-y pane))))

(defgeneric om-zoom-relayout-frame (frame)
  (:documentation "Re-apply layout to FRAME after PANE's zoom changed."))

(defmethod om-zoom-relayout-frame ((frame t)) nil)

(defgeneric om-zoom-touch-update (pane scale anchor-x anchor-y)
  (:documentation "Apply a multiplicative SCALE gesture to PANE's zoom."))

(defmethod om-zoom-touch-update ((pane t) scale anchor-x anchor-y)
  (declare (ignore scale anchor-x anchor-y))
  nil)

(defmethod om-zoom-touch-update ((pane om-scroller) scale anchor-x anchor-y)
  (om-zoom-update pane (* (om-zoom-of pane) scale) anchor-x anchor-y))

(defgeneric om-zoom-sync-display (pane factor)
  (:documentation "Refresh PANE's zoom-display chrome after FACTOR changed."))

(defmethod om-zoom-sync-display ((pane t) factor)
  (declare (ignore factor))
  nil)

(defgeneric om-zoom-redraw-connections (frame)
  (:documentation "Refresh FRAME's connection points after a zoom relayout."))

(defmethod om-zoom-redraw-connections ((frame t)) nil)

(defun om-zoom-update (pane new-zoom &optional anchor-x anchor-y)
  "Set PANE's zoom to NEW-ZOOM (clamped) anchored at (ANCHOR-X, ANCHOR-Y).
Returns the clamped factor, or NIL when no change."
  (let* ((old-zoom (om-zoom-of pane))
         (clamped  (om-clamp-zoom new-zoom)))
    (unless (= clamped old-zoom)
      (let* ((ratio    (if (zerop old-zoom) 1 (/ clamped old-zoom)))
             (old-sx   (tracked-scroll-x pane))
             (old-sy   (tracked-scroll-y pane))
             (vx       (or anchor-x 0))
             (vy       (or anchor-y 0))
             (new-sx   (max 0 (round (+ (* vx (- ratio 1)) (* old-sx ratio)))))
             (new-sy   (max 0 (round (+ (* vy (- ratio 1)) (* old-sy ratio))))))
        (setf (om-zoom-of pane) clamped)
        (setf (tracked-scroll-x pane) new-sx)
        (setf (tracked-scroll-y pane) new-sy)
        (om-zoom-sync-display pane clamped)
        (let ((*om-zoom-in-progress-p* t))
          (unwind-protect
              (capi:with-atomic-redisplay (pane)
                (dolist (frame (om-subviews pane))
                  (om-zoom-relayout-frame frame))
                (dolist (frame (om-subviews pane))
                  (om-zoom-redraw-connections frame))
                (multiple-value-bind (vw vh) (capi:simple-pane-visible-size pane)
                  (let ((content-h 0)
                        (content-v 0))
                    (dolist (sub (om-subviews pane))
                      (let ((p (handler-case (om-view-position sub)
                                 (error (c) (om-zoom-log-error :zoom-update-subview-pos c) nil)))
                            (s (handler-case (om-view-size sub)
                                 (error (c) (om-zoom-log-error :zoom-update-subview-size c) nil))))
                        (when (and p s)
                          (let ((right  (+ (om-point-h p) (om-point-h s)))
                                (bottom (+ (om-point-v p) (om-point-v s))))
                            (when (> right  content-h) (setf content-h right))
                            (when (> bottom content-v) (setf content-v bottom))))))
                    (let ((target-h (max (or vw 0) content-h (+ new-sx (or vw 0))))
                          (target-v (max (or vh 0) content-v (+ new-sy (or vh 0)))))
                      (capi::set-horizontal-scroll-parameters pane :min-range 0 :max-range target-h)
                      (capi::set-vertical-scroll-parameters   pane :min-range 0 :max-range target-v))))
                (om-set-h-scroll-position pane new-sx)
                (om-set-v-scroll-position pane new-sy)
                (capi::scroll pane :pan :move (list new-sx new-sy)))
            (om-invalidate-view pane t)))
        clamped))))

(defun om-set-zoom   (pane factor) (om-zoom-update pane factor))
(defun om-reset-zoom (pane)        (om-zoom-update pane 1.0))
(defun om-get-zoom   (pane)        (om-zoom-of pane))

(defun om-zoom-touch-handler (pane x y scale)
  (let* ((sensitivity (or *om-zoom-gesture-sensitivity* 1.0))
         (boost       *om-zoom-trackpad-boost*)
         (eff-scale   (+ 1.0 (* (- scale 1.0) sensitivity boost))))
    (when (and (typep pane 'om-scroller) (om-zoom-applies-p pane))
      (multiple-value-bind (vx vy) (om-zoom-touch-anchor pane x y)
        (om-zoom-touch-update pane eff-scale vx vy)))))

#+win32
(defun om-zoom-touch-pan-handler (pane x y dx dy)
  (declare (ignore x y))
  (when (and (typep pane 'om-scroller)
             (om-zoom-applies-p pane)
             (or (not (zerop dx)) (not (zerop dy))))
    (let* ((pos   (om-scroll-position pane))
           (hpos  (om-point-h pos))
           (vpos  (om-point-v pos))
           (new-x (max 0 (+ hpos (round dx))))
           (new-y (max 0 (+ vpos (round dy)))))
      (om-move-scroll-position pane (om-make-point new-x new-y))
      (om-set-h-scroll-position pane new-x)
      (om-set-v-scroll-position pane new-y)
      (om-invalidate-view pane t))))

;; The :om package is created by build-om.lisp AFTER api/om-LW loads;
;; defer kernel symbol lookup to runtime.
(defun om-zoom-scroll-pane (pane direction)
  (let* ((pkg (find-package :om))
         (sym (and pkg (find-symbol "SCROLL-PANE" pkg))))
    (when (and sym (fboundp sym))
      (funcall sym pane direction))))

(defun om-zoom-shift-wheel-handler (pane x y angle)
  (declare (ignore x y))
  (when (and (typep pane 'om-scroller) (om-zoom-applies-p pane))
    (cond ((plusp  angle) (om-zoom-scroll-pane pane :om-key-right))
          ((minusp angle) (om-zoom-scroll-pane pane :om-key-left)))))

(defun om-zoom-touch-swipe-handler (pane x y direction)
  (declare (ignore x y))
  (when (and (typep pane 'om-scroller) (om-zoom-applies-p pane))
    (case direction
      (:left  (om-zoom-scroll-pane pane :om-key-left))
      (:right (om-zoom-scroll-pane pane :om-key-right))
      (:up    (om-zoom-scroll-pane pane :om-key-up))
      (:down  (om-zoom-scroll-pane pane :om-key-down)))))

(defun om-zoom-ctrl-arrow-handler (pane x y gspec)
  (declare (ignore x y))
  (when (and (typep pane 'om-scroller) (om-zoom-applies-p pane))
    (let ((data (sys:gesture-spec-data gspec)))
      (case data
        (:left  (om-zoom-scroll-pane pane :om-key-left))
        (:right (om-zoom-scroll-pane pane :om-key-right))
        (:up    (om-zoom-scroll-pane pane :om-key-up))
        (:down  (om-zoom-scroll-pane pane :om-key-down))))))

(export '(+om-zoom-min+ +om-zoom-max+
          *om-zoom-default* *om-zoom-gesture-sensitivity*
          *om-zoom-trackpad-boost*
          *om-zoom-in-progress-p* *om-zoom-unscale-mouse-pos-p*
          *om-zoom-diag-events-p* *om-zoom-diag-apply-p*
          *om-zoom-mini-helper-scale*
          om-zoom-log-error om-zoom-diag-log
          om-clamp-zoom om-zoom-scale-point om-zoom-unscale-point
          om-zoom-scale-font
          om-zoom-of om-zoom-transformed-p om-zoom-find-ancestor-zoom
          om-zoom-effective
          om-zoom-logical-pos om-zoom-logical-size om-zoom-logical-font
          om-zoom-capture-logical-geom om-zoom-capture-logical-font
          om-zoom-applies-p
          om-zoom-relayout-frame om-zoom-touch-update
          om-zoom-sync-display om-zoom-redraw-connections
          om-current-default-font0 om-current-default-font1
          om-current-default-font2 om-current-default-font3
          om-current-default-font4
          om-current-default-font1b om-current-default-font2b
          om-current-default-font3b om-current-default-font4b
          om-current-ombox-font
          om-current-icon-size-factor om-current-miniview-font-size
          om-zoom-update om-set-zoom om-reset-zoom om-get-zoom
          om-zoom-touch-handler om-zoom-shift-wheel-handler
          om-zoom-touch-swipe-handler om-zoom-ctrl-arrow-handler
          om-zoom-scroll-pane
          #+win32 om-zoom-touch-pan-handler
          om-zoom-touch-anchor
          tracked-scroll-x tracked-scroll-y
          om-move-scroll-position)
        :om-api)



