(in-package :stream-seg)

(defun fig5 ()
  (mki 'chord-seq
       :lonset '(0 0 1000 2000 3000)
       :ldur '(2000 500 2000 2000 1000)
       :lmidic (mapcar #'n->mc '("C3" "G3" "Ab3" "E3" "F3"))))

(setf fig5
      (mki 'chord-seq
	   :lonset '(0 0 1000 2000 3000)
	   :ldur '(2000 500 2000 2000 1000)
	   :lmidic (mapcar #'n->mc '("C3" "G3" "Ab3" "E3" "F3"))))

(edist (first (cs->seq fig5) ) (fifth (cs->seq fig5)) 1 10)


(setq fig6
      (mki 'chord-seq
	   :lonset '(0 0 1000 2000 3000)
	   :ldur '(2000 500 2000 2000 1000)
	   :lmidic (mapcar #'n->mc '("C3" ("G3" "C4") "Ab3" "E3" "F3"))))

(cs->seq fig6)
;; 

(lonset (first (cs-segregate-euclid fig5)))

(lonset (first (cs-segregate-euclid fig6 1000)))

(inside fig6)

(lonset (first (cs-segregate-to-streams fig5)))
(lmidic (first (cs-segregate-to-streams fig5)))
(ldur (first (cs-segregate-to-streams fig5)))
(ldur fig5)

(defun test () (cs-segregate-to-streams fig5))

(defun all-clusters-are-simultaneousp (clusters)
  (loop
     for c1 in clusters
     always (loop
	       for c2 in clusters
	       always (clusters-are-simultaneousp c1 c2))))

(let ((C (events-to-clusters (cs->seq fig5))))
  (all-clusters-are-simultaneousp C))

(defun same-clusterp (clusters)
  (null (cdr clusters)))

(defun single-link-cluster (clusters)
  (cond ((null clusters) clusters)
	((all-clusters-are-simultaneousp clusters) clusters)
	((same-clusterp clusters) clusters)
	(t (single-link-cluster (merge-closest-clusters clusters)))))

(cs->seq fig5)
(single-link-cluster (cs->seq fig5))


;; ;; (1 2 1 3 4 2 3 1) -> ((1 1 1) (2 2) (3 3) (4))

;; (defun test-rek (liste ut)
;;   (cond ((null liste) (reverse ut))
;; 	((find (car liste) ut :key #'car)
;; 	 (test-rek (cdr liste)
;; 		   (progn (push (car liste)
;; 				(nth (position (car liste) ut :key #'car) ut))
;; 			  ut)))
	
;; 	(t (test-rek (cdr liste)
;; 		     (push (list (car liste)) ut)))))



;; (test-rek '(1 2 1 3 4 2 3 1) '())
