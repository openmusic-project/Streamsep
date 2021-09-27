;;; Stream separation for OM, Anders Vinjar
;;;
;;; Copyright (C) 2017 Anders Vinjar, <anders (dot) vinjar (at) bek (dot) no>
;;;
;;; Time-stamp: <2017-04-25 10:41:38 andersvi>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License.  See
;;; http://www.cliki.net/LLGPL for the text of this agreement.

(in-package :om)

;;;==================================
;;; ANALYSIS CLASS STREAM SEPARATION
;;;==================================
;;;
;;; "SINGLE LINK AGGLOMERATIVE CLUSTERING BASED STREAM-SEGREGATION"
;;; 
;;; Segments = chord marker
;;; 

(defclass! stream-seg (abstract-analysis) ())

(defmethod compatible-analysis-p ((analyse stream-seg) (object chord-seq)) t)
(defmethod compatible-analysis-p ((analyse stream-seg) (object t)) nil)

(defclass! stream-seg-data ()
  ((time-weight :accessor time-weight :initarg :time-weight :initform 1)
   (pitch-weight :accessor pitch-weight :initarg :pitch-weight :initform 1)
   (overlap-tolerance :accessor overlap-tolerance :initarg :overlap-tolerance :initform 0.0)
   (distance-fun :accessor distance-fun :initarg :distance-fun :initform #'euclidian-distance)
   (seg-infinity :accessor seg-infinity :initarg :seg-infinity :initform most-positive-long-float)
   (streams :accessor streams :initarg :streams :initform nil)
   (updateflag :accessor updateflag :initform nil)))

(defmethod default-segment-class ((self stream-seg)) 'chord-marker)

(defmethod compute-segments-p ((self stream-seg)) nil)
(defmethod analyse-segments-p ((self stream-seg)) t)
(defmethod compute+analyse-segments-p ((self stream-seg)) nil)

(defmethod analysis-init ((self stream-seg) object)
  (unless (analysis-segments self)
    (setf (analysis-segments self)
          (list (make-instance 'chord-marker :chord-id 0))))
  (call-next-method))

(defparameter *def-time-weight* 1.0)
(defparameter *def-pitch-weight* 1.0)
(defparameter *def-overlap-tolerance* 0.0)

(defmethod analysis-init-segment ((analysis stream-seg) segment)
  (unless (segment-data segment) 
    (setf (segment-data segment)
	  (make-instance 'stream-seg-data
			 :time-weight *def-time-weight*
			 :pitch-weight *def-pitch-weight*
			 :overlap-tolerance *def-overlap-tolerance*
			 :distance-fun #'euclidian-distance)))
  (when (previous-segment segment)
    (setf (updateflag (segment-data (previous-segment segment))) nil)))
  
(defmethod delete-from-analysis ((self stream-seg) segment)
  (when (previous-segment segment)
    (setf (updateflag (segment-data (previous-segment segment))) nil))
  (call-next-method))


(defmethod analyse-one-segment ((self stream-seg) seg (object t))
  (let* ((begin (segment-begin seg))
	 (end (min (segment-end seg) (get-obj-dur object)))
	 (tmpcseq (select object begin end))
	 (stream-seg-data (or (segment-data seg) 
			      (setf (segment-data seg) (make-instance 'stream-seg-data))))
	 (distfun (distance-fun stream-seg-data))
	 (tw (time-weight stream-seg-data))
	 (pw (pitch-weight stream-seg-data))
	 (tolerance (overlap-tolerance stream-seg-data)))
    (flet ((cs-segregate-streams (cs)
	     (let ((*overlap-tolerance* tolerance)
		   (*distance-fun* (let ((cache (make-hash-table :test #'equal)))
				     #'(lambda (e1 e2)
					 (let ((idxs (sort (list (evt-idx e1) (evt-idx e2)) #'<)))
					   (multiple-value-bind (val isval) (gethash idxs cache)
					     (if isval
						 val
						 (setf (gethash idxs cache)
						       (funcall distfun e1 e2 tw pw)))))))))
	       (cs-segregate-to-streams cs))))
      (setf (streams stream-seg-data) (cs-segregate-streams tmpcseq))
      (setf (updateflag stream-seg-data) t))))


(defmethod handle-segment-doubleclick ((self stream-seg) segment panel pos) 
  (stream-seg-data-window (or (segment-data segment) 
			      (setf (segment-data segment) (make-instance 'stream-seg-data))))
  (update-panel panel))

(defmethod draw-segment-data ((self stream-seg) segment view) 
  (let ((x1 (+ 10 (time-to-pixels view (segment-begin segment)))))
    (when (segment-data segment)
      (om-with-font *om-default-font1*
                    (om-draw-string x1 (- (h view) 115) (format nil "STREAM SEP PARAMS:"))
                    (om-draw-string x1 (- (h view) 100) (format nil "time weight:~25T~A" (time-weight (segment-data segment))))
                    (om-draw-string x1 (- (h view) 85) (format nil "pitch weight:~25T~A" (pitch-weight (segment-data segment))))
                    (om-draw-string x1 (- (h view) 70) (format nil "overlap tolerance:~25T~A" (overlap-tolerance (segment-data segment))))
		    (om-draw-string x1 (- (h view) 55) (format nil "offset (ms) :~25T~6@A" (segment-begin segment))))
      (om-with-fg-color view (if (updateflag (segment-data segment)) *om-black-color* *om-gray-color*)
	(om-with-font *om-default-font1b*
		      (om-draw-string x1 (- (h view) 40)
				      (format nil "streams:~{ ~A~}" (arithm-ser 1 (length (streams (segment-data segment))) 1))))))))

(defmethod stream-seg-data-window ((data stream-seg-data))
  (let ((win (om-make-window 'om-dialog :position :centered
			     :window-title "Streamsep: parameters for segment:"
                             :size (om-make-point 430 200)))
        (pane (om-make-view 'om-view
                            :size (om-make-point 400 180)
                            :position (om-make-point 10 10)
                            :bg-color *om-white-color*))
        (i 0)
        time-w-txt
	pitch-w-txt
	overlap-tolerance-txt)
    (om-add-subviews pane
		     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 16))
					  (om-make-point 380 40)
					  "Set distance measure weights for selected segment:"
					  :font *om-default-font2b*)
		     (om-make-dialog-item 'om-static-text
					  (om-make-point 50 (incf i 30))
					  (om-make-point 150 20)
					  "Time weight"
					  :font *om-default-font1*)
		     (setf time-w-txt (om-make-dialog-item 'om-editable-text
							   (om-make-point 210 i)
							   (om-make-point 37 13)
							   (format nil "~D" (time-weight data)) 
							   :font *om-default-font1*))
		     (om-make-dialog-item 'om-static-text
					  (om-make-point 50 (incf i 30))
					  (om-make-point 150 20)
					  "Pitch weight"
					  :font *om-default-font1*)
		     (setf pitch-w-txt (om-make-dialog-item 'om-editable-text
							    (om-make-point 210 i)
							    (om-make-point 37 13)
							    (format nil "~D" (pitch-weight data)) 
							    :font *om-default-font1*))
		     (om-make-dialog-item 'om-static-text
					  (om-make-point 50 (incf i 30))
					  (om-make-point 150 20)
					  "Overlap tolerance (0.0-1.0)"
					  :font *om-default-font1*)
		     (setf overlap-tolerance-txt (om-make-dialog-item 'om-editable-text
								      (om-make-point 210 i)
								      (om-make-point 37 13)
								      (format nil "~D" (overlap-tolerance data))
								      :font *om-default-font1*))
		     (om-make-dialog-item  'om-button
					   (om-make-point 300 (- i 35))
					   (om-make-point 80 20)
					   "OK"
					   :di-action (om-dialog-item-act item 
							(let ((tw (ignore-errors (read-from-string (om-dialog-item-text time-w-txt))))
							      (pw (ignore-errors (read-from-string (om-dialog-item-text pitch-w-txt))))
							      (ot (ignore-errors (read-from-string (om-dialog-item-text overlap-tolerance-txt)))))
							  (setf (time-weight data) tw
								(pitch-weight data) pw
								(overlap-tolerance data) ot)
							  (setf (updateflag data) nil)
							  (om-return-from-modal-dialog win t)))))
    (om-add-subviews win pane)
    (om-modal-dialog win)))


;;; 
;;; access to segments with segregated streams in
;;; 

(defmethod get-stream-seg-streams-w-onset ((self stream-seg))
  (loop
     for seg in (analysis-segments self)
     collect (mapcar #'(lambda (cs)
			 (setf (offset cs) (segment-begin seg))
			 cs)
		     (streams (segment-data seg)))))

(defmethod! stream-seg-markers ((self chord-seq) &optional (n 0))
  :indoc '("chord-seq" "nth analysis")
  :doc "returns a list of offsets (ms.) for segments in analysis 'n'"
  :icon 700
  (let* ((seqs (stream-seg-segments self n))
	 (offsets (mapcar #'(lambda (css) (offset (first css))) seqs)))
    (if (zerop (car offsets))
	offsets
	(cons 0 offsets))))

(defmethod! stream-seg-segments ((self chord-seq) &optional n)
  :indoc '("chord-seq" "nth analysis")
  :doc "Get stream-seg-segments from analysis inside chord-seq.

Returns a list of lists, one per segment, each holding a list of
chord-seqs representing the voices found in that particular
segment.

If layering several stream-seg analysis in the cs, the optional
input 'n' can be used to retreive the 'n'th analysis.

Check http://repmus.ircam.fr/openmusic/dev-resources/analysis for more info on OMs analysis class."

  :icon 700
  (let* ((stream-seg-analyses (remove-if-not 
			       #'(lambda (a) (and a (equal (type-of a) 'stream-seg)))
			       (analysis self)))
	 (l (length stream-seg-analyses)))
    (cond ((and (> l 1) (not n))
	   (om-beep-msg (format nil "More than 1 stream-seg-analyses found, using 1st.~%Use optional input 'n' to select another one.")))
	  ((and (numberp n) (> n (1- l)))
	   (om-beep-msg (format nil "only ~R stream-seg-analyses found, using 1st" l)))
	  (t t))
    ;; returns a list of cs'es:
    (get-stream-seg-streams-w-onset (nth (min (1- l) (or n 0)) stream-seg-analyses))))

(defmethod! stream-seg-to-multi-seq ((self chord-seq) &optional n)
  :icon 700
  :indoc '("chord-seq with segments from stream-seg analysis inside" "nth analysis")
  :doc "Takes a chord-seq with segments from stream-seg analysis inside, and builds and returns
a multi-seq from the sequence of segments with their voices separated.

If layering several stream-seg analysis in the cs, the optional input 'n' can be used to retreive the nth analysis.

Check http://repmus.ircam.fr/openmusic/dev-resources/analysis for more info on OMs analysis class.
"
  (let* ((seqs (stream-seg-segments self n))
	 (offsets (mapcar #'(lambda (css) (offset (first css))) seqs)))
    (reduce #'(lambda (s1 s2) (concat s1 s2 (pop offsets)))
	    (mapcar #'(lambda (cs)
			(when cs (mki 'multi-seq :chord-seqs cs)))
		    ;; make sure to include everything up to first segment-marker
		    (cons nil seqs)))))

(defmethod! stream-seg-to-maquette ((self chord-seq) &optional n)
  :icon 700
  :indoc '("chord-seq with segments from stream-seg analysis inside" "nth analysis")
  :numouts 2
  :doc "Takes a chord-seq with segments from stream-seg analysis inside, and returns two values suitable to pass to a
maquette's two inputs : 1) a list of offsets, 2) a list of chord-seqs

If layering several stream-seg analysis in the cs, the optional input 'n' can be used to retreive the nth analysis.

Check http://repmus.ircam.fr/openmusic/dev-resources/analysis for more info on OMs analysis class.
"
  (let ((all-chord-seqs (flat (stream-seg-segments self n))))
    (values-list (list (mapcar #'offset all-chord-seqs)
		       all-chord-seqs))))
