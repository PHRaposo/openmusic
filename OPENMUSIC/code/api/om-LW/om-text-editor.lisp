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
; OM TEXT EDIT WINDOW
; & exported functions
;DocFile
;;===========================================================================
;;; uses text editor from lw-lisp-tools


(in-package :oa)

(export '(
          om-text-edit-window
          om-set-text
          om-set-text-buffer
          om-get-text
          om-kill-window-buffer 
          om-lisp-edit-window
          om-get-lisp-expression
          om-texteditor-modified
          ) :om-api)



;;; !!! created with om-make-window :
(defclass om-text-edit-window (om-text-editor om-abstract-window)
  ((save-callback :accessor save-callback :initarg :save-callback :initform nil)
   (echoarea :accessor echoarea :initarg :echoarea :initform nil)))


(defgeneric editor-window-font (window)
  (:documentation "Font for the editor-pane of WINDOW; specialize per subclass for distinct fonts."))

(defmethod editor-window-font ((self om-text-edit-window))
  om-lisp::*def-text-edit-font*)


(defgeneric editor-window-input-model (window)
  (:documentation "Extra input-model entries APPENDED to the editor-pane default; nil if none."))

(defmethod editor-window-input-model ((self om-text-edit-window))
  nil)


;;; ===== Zoom support: text-editor pane font scaling =====
;;; Categories: ZOOM-SCALE, ZOOM-INPUT

(defun ensure-font-description (font-or-desc)
  "Return a gp:font-description from either a gp:font-description (identity) or a gp:font (extract via accessor)."
  (etypecase font-or-desc
    (gp:font-description font-or-desc)
    (gp:font             (gp:font-description font-or-desc))))


(defmethod update-for-subviews-changes ((self om-text-edit-window) &optional (recursive nil)) nil)

(setf *line-color-numbers* (om-make-color 0.772 0.855 0.788))

#|
(defmethod make-window-layout ((self om-text-edit-window) &optional color)
  (make-instance 'simple-layout :description
                 (list (setf (om-lisp::ep self)
                             (make-instance 'capi::editor-pane
                                            :font om-lisp::*def-text-edit-font*
                                            :line-numbers-p *line-numbers*
                                            :line-numbers-background (om-color-to-capi *line-color-numbers*)
                                            :echo-area (echoarea self)
                                            :change-callback 'texteditor-change-callback)))))
|#

(defmethod make-window-layout ((self om-text-edit-window) &optional color)
  (let ((ep (make-instance 'capi::editor-pane
                           :font (editor-window-font self)
                           :line-numbers-p *line-numbers*
                           :line-numbers-background (om-color-to-capi *line-color-numbers*)
                           :echo-area (echoarea self)
                           :change-callback 'texteditor-change-callback)))
    (setf (om-lisp::ep self) ep)
    (setf (capi:capi-object-property ep :om-zoom-default-font)
          (ensure-font-description (capi::simple-pane-font ep)))
    (let ((extra (editor-window-input-model self)))
      (when extra
        (setf (capi:output-pane-input-model ep)
              (append extra (capi:output-pane-input-model ep)))))
    (make-instance 'simple-layout :description (list ep))))


(defun rebuild-font-with-size (font new-size)
  (let ((attrs (copy-list (gp::font-description-attributes font))))
    (setf (getf attrs :size) new-size)
    (apply #'gp::make-font-description attrs)))


(defun apply-pane-font-step (pane delta-or-scale resetp)
  "Modify PANE's font size only. DELTA-OR-SCALE is an integer step (shortcut) or float scale (touch). RESETP non-nil restores the pane's original font (stashed at construction)."
  (let* ((current-font (capi::simple-pane-font pane))
         (current-desc (gp::font-description current-font))
         (current-size (gp::font-description-attribute-value current-desc :size))
         (new-size (cond (resetp
                          (gp::font-description-attribute-value
                           (capi:capi-object-property pane :om-zoom-default-font)
                           :size))
                         ((integerp delta-or-scale)
                          (max 8 (min 72 (+ current-size delta-or-scale))))
                         (t
                          (max 8 (min 72 (round (* current-size delta-or-scale))))))))
    (unless (= new-size current-size)
      (setf (capi::simple-pane-font pane) (rebuild-font-with-size current-desc new-size)))))


;; ZOOM-INPUT: pinch handler for the editor-pane.
(defun om-zoom-pane-touch-handler (pane x y scale)
  (declare (ignore x y))
  (apply-pane-font-step pane scale nil))


(defun pane-current-zoom-ratio (pane)
  (let* ((default-desc (capi:capi-object-property pane :om-zoom-default-font))
         (current-desc (gp::font-description (capi::simple-pane-font pane)))
         (default-size (gp::font-description-attribute-value default-desc :size))
         (current-size (gp::font-description-attribute-value current-desc :size)))
    (float (/ current-size default-size))))


(defun set-pane-zoom-ratio (pane ratio)
  (let* ((default-desc (capi:capi-object-property pane :om-zoom-default-font))
         (default-size (gp::font-description-attribute-value default-desc :size))
         (new-size (max 8 (min 72 (round (* default-size ratio))))))
    (setf (capi::simple-pane-font pane)
          (rebuild-font-with-size default-desc new-size))))


;; ZOOM-INPUT: keyboard shortcut handlers.
(defun om-zoom-shortcut-in (pane x y spec)
  (declare (ignore x y spec))
  (apply-pane-font-step pane 1 nil))

(defun om-zoom-shortcut-out (pane x y spec)
  (declare (ignore x y spec))
  (apply-pane-font-step pane -1 nil))

(defun om-zoom-shortcut-reset (pane x y spec)
  (declare (ignore x y spec))
  (apply-pane-font-step pane 0 t))


;; ZOOM-INPUT: input-model entries prepended by customize-text-editor-pane.
(defun om-zoom-input-entries ()
  "Input-model entries for font zoom: 4 shortcut gesture-specs + 1 touch. Prepend to default input-model so shortcuts fire before the editor catch-all."
  `(((:gesture-spec #\= #+macosx :hyper #-macosx :control) om-zoom-shortcut-in)
    ((:gesture-spec #\+ #+macosx :hyper #-macosx :control) om-zoom-shortcut-in)
    ((:gesture-spec #\- #+macosx :hyper #-macosx :control) om-zoom-shortcut-out)
    ((:gesture-spec #\0 #+macosx :hyper #-macosx :control) om-zoom-shortcut-reset)
    ((:touch :zoom) om-zoom-pane-touch-handler)))

;;; ===== End zoom support =====


(defmethod om-window-class-menubar ((self om-text-edit-window))
  (append (om-lisp::internal-window-class-menubar self)
          (call-next-method)))

(defun texteditor-change-callback (pane point old-length new-length)
  (om-texteditor-modified (capi::top-level-interface pane)))


;;; celui-la soit il a un fichier soit non mais c'est fixe
(defmethod om-lisp::file-operations-enabled ((self om-text-edit-window)) nil)

(defmethod om-lisp::save-operation-enabled ((self om-text-edit-window))
  (or (save-callback self) 
      (and (om-lisp::editor-file self)
           (om-lisp::buffer-modified-p self))))

(defmethod om-lisp::save-text-file ((self om-text-edit-window))
  (if (save-callback self) (funcall (save-callback self) self)
    (call-next-method)))

;;; DOES NOT KILL THE BUFFER 
(defmethod om-destroy-callback ((self om-text-edit-window))
  (setf (om-lisp::window (om-lisp::editor-buffer self)) nil)
  (setf om-lisp::*editor-files-open* (remove self om-lisp::*editor-files-open*))
  (om-window-close-event self))

(defmethod om-lisp::check-close-buffer ((self om-text-edit-window))
  (cond ((and (om-lisp::buffer-modified-p self) (om-lisp::editor-file self))
         (if (call-next-method)
             (om-window-check-before-close self)
         nil))
        (t (om-window-check-before-close self))))

(defmethod om-select-window ((self om-text-edit-window)) 
  (capi::find-interface (type-of self) :name (capi::capi-object-name self)))

(defmethod om-view-window ((self om-text-edit-window)) self)

(defmethod om-set-window-title ((self om-text-edit-window) (title string))
  (setf (capi::interface-title self) title))

;;; met un nouveau buffer
(defmethod om-set-text-buffer ((self om-text-edit-window) newbuffer &optional kill) 
  (let ((rec (om-lisp::buffer (om-lisp::editor-buffer self))))
    (setf (om-lisp::editor-buffer self) newbuffer)
    (om-lisp::init-text-editor self)
     ;;; detruit le buffer precedent
    (when kill (editor::kill-buffer-no-confirm rec))))

;;; garde le m�me buffer mais change le contenu
(defmethod om-set-text ((self om-text-edit-window) (text string)) 
  (om-buffer-delete (om-lisp::editor-buffer self))
  (om-buffer-insert (om-lisp::editor-buffer self) text))

(defmethod om-get-text ((self om-text-edit-window)) 
 (om-buffer-text (om-lisp::editor-buffer self)))

(defmethod om-kill-window-buffer ((self om-text-edit-window)) 
 (om-kill-buffer (om-lisp::editor-buffer self)))

(defmethod om-texteditor-modified ((self om-text-edit-window)) nil)


(defclass om-lisp-edit-window (om-text-edit-window) ()
  (:default-initargs :lisp-editor? t))

(defmethod editor-window-font ((self om-lisp-edit-window))
  om-lisp::*om-lisp-edit-font*)

(defmethod editor-window-input-model ((self om-lisp-edit-window))
  (om-zoom-input-entries))

(defmethod om-lisp::lisp-operations-enabled ((self om-lisp-edit-window)) t)

;had to import this from lispmode.lisp version 8.1/src/editor
(defun current-form-as-read (start-point)
  (editor::with-point ((spoint start-point))
    (when (editor:form-offset spoint 1)
      (editor::read-form-from-region start-point spoint))))

(defmethod om-get-lisp-expression ((self om-lisp-edit-window))
  (let ((textbuffer (om-lisp::buffer (om-lisp::editor-buffer self))))
    (editor::use-buffer textbuffer
      ;; (editor::SKIP-LISP-READER-WHITESPACE (editor:buffers-start textbuffer) textbuffer)
      (current-form-as-read (editor:buffers-start textbuffer))
      )))

;(defmethod test-lisp-form ((self om-text-edit-window))
;  (let ((textbuffer (buffer (editor-buffer self))))
;    (editor::use-buffer textbuffer
 ;     ;(editor::SKIP-LISP-READER-WHITESPACE (editor:buffers-start textbuffer) textbuffer)
 ;;     (editor::current-form-as-read (editor:buffers-start textbuffer))
;      )))


;  (om-buffer-text (editor-buffer self)))
; SKIP-LISP-READER-WHITESPACE
; CURRENT-FORM-AS-READ
; READ-FORM
; READ-FUNCTION


#|
(defun om-lisp::open-new-text-editor (&optional path)
  (let ((win (when path (om-lisp::find-open-file path om-lisp::*editor-files-open*))))
    (if win (capi::find-interface (type-of win) :name (capi::capi-object-name win))
      (let* ((buffer (if path (editor:find-file-buffer path)
                       (editor::make-buffer (string (gensym))))))
        (setf win (make-instance om-lisp::*editor-class*
                                 :name (string (gensym))
                                 :x 200 :y 200 :width 800 :height 800
                                 :parent (capi:convert-to-screen)
                                 :internal-border 5 :external-border 0
                                 :display-state :normal
                                 :internal-min-height 200 :internal-min-width 300
                                 :title (if path (namestring path) "New Buffer")
                                 :editor-buffer (make-instance 'om-lisp::ombuffer :buffer buffer)
                                 :editor-file path
                                 :activate-callback #'(lambda (win activate-p)
                                                        (when activate-p
                                                          (setf (capi::interface-menu-bar-items win)
                                                                (append (om-lisp::internal-window-class-menubar win)
                                                                        (om-lisp::om-window-class-menubar win)))))
                                 :lisp-editor? t))
        (let ((ep (make-instance 'capi::editor-pane
                                 :echo-area t
                                 :font om-lisp::*om-lisp-edit-font*
                                 :line-numbers-p om-lisp::*line-numbers*)))
          (setf (om-lisp::ep win) ep)
          (setf (capi::layout-description (capi::pane-layout win)) (list ep))
          (setf (capi:capi-object-property ep :om-zoom-default-font)
                (ensure-font-description (capi::simple-pane-font ep)))
          (setf (capi:output-pane-input-model ep)
                (append (om-zoom-input-entries)
                        (capi:output-pane-input-model ep))))
        (push win om-lisp::*editor-files-open*)
        (setf (capi::simple-pane-background (om-lisp::ep win)) om-lisp::*text-bg-color*)
        (capi::display win)
        win))))
|#

(defmethod om-lisp::customize-text-editor-pane ((win om-lisp::om-text-editor))
  (let ((ep (om-lisp::ep win)))
    (when ep
      (setf (capi::simple-pane-font ep) om-lisp::*om-lisp-edit-font*)
      (setf (capi:capi-object-property ep :om-zoom-default-font)
            (ensure-font-description (capi::simple-pane-font ep)))
      (setf (capi:output-pane-input-model ep)
            (append (om-zoom-input-entries)
                    (capi:output-pane-input-model ep))))))

