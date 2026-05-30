(in-package :oa)

(defgeneric om-pane-property (pane key))

(defmethod om-pane-property ((pane capi:simple-pane) key)
  (capi:capi-object-property pane key))

(defgeneric (setf om-pane-property) (value pane key))

(defmethod (setf om-pane-property) (value (pane capi:simple-pane) key)
  (setf (capi:capi-object-property pane key) value))

(defgeneric om-pane-visible-size (pane))

(defmethod om-pane-visible-size ((pane capi:simple-pane))
  (capi:simple-pane-visible-size pane))

(defgeneric om-redraw-pinboard-object (obj))

(defmethod om-redraw-pinboard-object ((obj capi:pinboard-object))
  #+win32 (capi:redraw-pinboard-object obj))

(defgeneric om-redisplay-element (elem))

(defmethod om-redisplay-element ((elem capi:simple-pane))
  #+linux (capi:redisplay-element elem))

(defparameter om-text-input-tab-complete 'capi:text-input-pane-complete-text)

(export '(om-pane-property
          om-pane-visible-size
          om-redraw-pinboard-object
          om-redisplay-element
          om-text-input-tab-complete)
        :oa)
