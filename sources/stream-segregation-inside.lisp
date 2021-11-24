;;; 
;;; Time-stamp: <2016-12-25 15:14:32 andersvi>
;;; 
;;; ML BASED STREAM SEGREGATION, VOICE SEPARATION
;;;
;;; uses a single link clustering algorithm to combine nearby events (time, pitch)
;;; into clusters (aka voices), given a user-supplied function to measure proximity
;;; of feature vectors
;;;
;;; Inspired by the work of Szeto, W.M. & Wong, M.H., 2006: "Stream segregation
;;; algorithm for pattern matching in polyphonic music databases". Multimedia Tools
;;; and Applications, 30(1), pp.109–127.
;;;
;;; 
;;;
;;; WORKFLOW: data in chord-seq; call #'cs-segregate-euclid, or
;;; #'cs-segregate-to-streams with a user-supplied function for distance
;;; measurement; output is a list of chord-seqs, one per voice found.
;;;
;;; TUNEABLES: weighting in the distance measurement, e.g. for the supplied
;;; #'edist (euclidian distance) working on iot and relative pitch
;;; (mel-frequency): time-weight: 1000, pitch-weight: 1
;;;
;;; TODO: work per segment/window in input chord-seq, search for 'stable'
;;; sections (ie. monophonic, rests)
;;;
;;; TODO: optimization: substitute static structure (indexing) for
;;; consing, fancier interaction in analysis class
;;;

(in-package :om)

(defvar *seg-evt-idx* 0)
(defstruct evt onset end dur data (idx (incf *seg-evt-idx*)))

;; seems to make sensible collections

;; TODO: consider sub/super-classing eventmidi-seq instead

(defmethod cs->seq ((cs chord-seq))
  (let ((events (inside cs))
	(*seg-evt-idx* 0))
    (loop
       for onset in (butlast (lonset cs))
       for dur in (ldur cs)
       for evt in events
       ;; TODO: collect trills, ornaments etc. into same event
       if (> (length (inside evt)) 1)
       ;;explode chord for now:
       append (loop
		 for sub in (inside evt)
		 for subdur in dur
		 collect (make-evt :onset onset :end (+ onset subdur)
				   :dur subdur
				   :data (make-instance 'chord
							:inside (list (clone sub))
							:offset (offset->ms evt)
							;; :qvalue (qvalue evt)
							)))
       else
       collect (make-evt :onset onset
			 :dur (car dur)
			 :end (+ onset (car dur))
			 :data (clone evt)))))



(defstruct cluster onset end events)

(defun events-to-clusters (events)
  "events-to-clusters (events), transforms every event into its own cluster,
   each cluster grabbing onset and end from its event, and containing the event"
  (loop
     for event in events
     collect (make-cluster :onset (evt-onset event)
			   :end (evt-end event)
			   :events (list event))))



;;;
;;; SLAC - SINGLE LINK AGGLOMERATIVE CLUSTERING: 
;;;

;; classify qua w/wo overlapping events:

(defparameter *overlap-tolerance* 0.00
  "fraction of duration allowed to overlap next note")

(defun events-are-sequentialp (e1 e2)
  (let ((ons1 (evt-onset e1))
	(ons2 (evt-onset e2))
	(end1 (evt-end e1))
	(end2 (evt-end e2))
	(dur1 (evt-dur e1))
	(dur2 (evt-dur e2)))
    (cond  ((or (<= end1 ons2) (<= end2 ons1)) t)
	   ((>= ons2 (- end1 (* dur1 *overlap-tolerance*))) t)
	   ((>= ons1 (- end2 (* dur2 *overlap-tolerance*))) t)
	   (t nil))))

;; (let ((*overlap-tolerance* 0.03)
;;       (a (make-evt :onset 0 :end 3.4 :dur 3.4))
;;       (b (make-evt :onset 3.0 :end 8 :dur 5)))
;;   (events-are-sequentialp a b))

(defun events-are-simultaneousp (e1 e2)
  (not (events-are-sequentialp e1 e2)))

(defun clusters-are-sequentialp (c1 c2)
  (cond ((<= (cluster-end c1) (cluster-onset c2)) (list c1 c2))
	((<= (cluster-end c2) (cluster-onset c1)) (list c2 c1))
	(t nil)))

(defun clusters-are-simultaneousp (c1 c2)
  (not (clusters-are-sequentialp c1 c2)))



;; Find distances using Single Link Clustering, ie: minimize distance between the
;; two closest elements in two separate clusters

(defvar seg-infinity most-positive-long-float)

(defvar *distance-fun* #'(lambda (a b) (declare (ignore a b)) 1)
  "variable holding a binary similarity measure function")

(defun cluster-distance (c1 c2)
  (if (clusters-are-simultaneousp c1 c2)
      seg-infinity
      (loop
	 for ev1 in (cluster-events c1)
	 minimize (loop
		     for ev2 in (cluster-events c2)
		     minimize (funcall *distance-fun* ev1 ev2)))))



;; find two closest clusters in set, closeness determined by distance between
;; the closest event from each cluster

(defun find-two-closest-clusters (clusters)
  (let ((min-distance seg-infinity)			    ;current closest match
	closest-1 closest-2)
    (loop
       for (c1 . clusters-to-check) on clusters		    
       do (loop
	     ;; right of diagonal only
	     for c2 in clusters-to-check
	     for this-distance = (cluster-distance c1 c2)

	     ;; update with closer match
	     when (< this-distance min-distance)
	     do (setf closest-1 c1 closest-2 c2 min-distance this-distance)))
    
    (list closest-1 closest-2 min-distance)))



;;;
;;; merge-clusters, merges all sequential clusters together recursively, returns
;;; list of simultaneous clusters, ie. with overlapping events
;;;

;; combine two clusters into one new encompassing both in time, collecting all
;; events from both:

(defun merge-two-clusters (c1 c2)
  (make-cluster :onset (min (cluster-onset c1) (cluster-onset c2))
		:end (max (cluster-end c1) (cluster-end c2))
		:events (append (cluster-events c1) (cluster-events c2))))


;; TODO: change from consing to fixed storage, store indexes in already existing
;; structure or something....  

;; TODO: adapt "Priority queue algorithm for HAC" (Manning et al., p. 411) for
;; N²logN complexity (instead of ϴ=N³ here), and using a NBM - Next Best Merge -
;; array (p. 412)

(defun merge-clusters (clusters)
  (let ((closest (find-two-closest-clusters clusters)))
    (if (null (car closest))
	clusters
	(merge-clusters (cons (merge-two-clusters (car closest) (cadr closest))
			      (delete-if #'(lambda (c) (member c closest))
					 clusters))))))

;; (merge-clusters (events-to-clusters (cs->seq fig5))
;; 	       #'edist)

;; get our music back

(defun clusters-to-cs (streams)
  (loop
    for stream in streams
    collect (let* ((new-cs (make-instance 'chord-seq))
		   (time-sorted-evts (sort
				      (mapcar #'(lambda (evt)
						  (cons (evt-onset evt) (evt-data evt)))
					      (cluster-events stream))
				      #'< :key #'car)))
	      ;; (setf (inside new-cs) (mapcar #'cdr time-sorted-evts))
	      (set-chords new-cs (mapcar #'cdr time-sorted-evts))
	      (setf (lonset new-cs) (mapcar #'car time-sorted-evts))
	      ;; (adjust-extent new-cs)
	      ;; (QNormalize new-cs)
	      (print (lonset new-cs) t)
	      new-cs)))


;;; MAIN INTERFACE: set up *distance-fun* in a scope and call this function in the
;;; same scope:

(defun cs-segregate-to-streams (cs)
  "input chord-seq, segregate into clusters based on function
stored in dynamic variable *distance-fun*, return list of chord-seqs"
  (clusters-to-cs 
   (merge-clusters
    (events-to-clusters
     (cs->seq cs)))))



;;
;; DISTANCE FUNCTION
;;
;; measuring distance between events/clusters, any 2 closest events gets merged
;; into common cluster
;;
;;
;; Frequency distance:
;; 
;; using mel scale here to measure closer to perceptually significant
;; differences between pitches:

(defmethod! mc->mel ((mc number))
  :numouts 1 
  :initvals '(6600) 
  :indoc '("mc")
  :icon 141
  :doc "Converts a mc value or list of such to values on a mel scale"
  (* 2595 (log (+ 1 (/ (mc->f mc) 700)) 10)))

(defmethod mc->mel ((mc list))
  (mapcar #'mc->mel mc))

(defmethod! f->mel ((hz number))
  :numouts 1 
  :initvals '(440) 
  :indoc '("f (Hz)")
  :icon 141
  :doc "Converts a Hz value or list of such to values on a mel scale"
  (* 2595 (log (+ 1 (/ hz 700)) 10)))

(defmethod f->mel ((hz list))
  (mapcar #'f->mel hz))

(defun evt-mel-frequency (evt)
  (mc->mel (first (lmidic (evt-data evt)))))




;;
;; Temporal distance:
;; 
;; using (linear) milliseconds in weighted time factor for now.  Consider more
;; clever measure of closeness based on temporal situation.
;;
;; TODO: check other metrics to measure vectors
;; 

(defun inter-evt-ms (e1 e2)
  (- (apply #'max (mapcar #'evt-onset (list e1 e2)))
     (apply #'min (mapcar #'evt-end (list e1 e2)))))

(defun euclidian-distance (e1 e2 &optional (time-weight 1) (pitch-weight 1))
  "euclidian distance between events, time (ms) and pitch (mel-f) weighted"
  (if (events-are-simultaneousp e1 e2)
      seg-infinity
      (sqrt (+
	     (expt (* time-weight (inter-evt-ms e1 e2)) 2)
	     (expt (* pitch-weight (abs (- (evt-mel-frequency e1) (evt-mel-frequency e2)))) 2)))))


(defmethod! cs-segregate-streams (cs &optional (time-weight 1) (pitch-weight 1) (overlap-tolerance *overlap-tolerance*))
  :numouts 1
  :initvals '(nil 1 1) 
  :indoc '("chord-seq" "time-weight (1)" "pitch-weight (1)")
  :icon 700
  :doc "Takes as input a chord-seq, and splits this into separate musical streams.
  The output is a list of chord-seqs, one for each such stream found.

  The optional inputs time-weight and pitch-weight are used to tune the
  distance-measurement function, attributing more or less weight to the related
  dimension

  The clustering algorithm used is a single link agglomerative clustering algorithm,
  allowing each cluster to contain non-uniform distributions, typical of musical
  voices.

  The cs-segregate-streams function works on the whole input cs.  Time and memory
  consumption will grow exponentially with larger input data.
  
  An analysis class is provided - 'stream-seg' - to work separately on segmented
  sections of musical data.  Functions are provided to access the resulting
  segregated data from running the analysis on sequences of segments."
  
  (let ((*overlap-tolerance* overlap-tolerance)
	(*distance-fun*
	 (let ((cache (make-hash-table :test #'equal)))
	   #'(lambda (e1 e2)
	       (let ((idxs (sort (list (evt-idx e1) (evt-idx e2)) #'<)))
		 (multiple-value-bind (val isval) (gethash idxs cache)
		   (if isval
		       val
		       (setf (gethash idxs cache)
			     (euclidian-distance e1 e2 time-weight pitch-weight)))))))))
    (cs-segregate-to-streams cs)))

