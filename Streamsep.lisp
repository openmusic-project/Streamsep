(in-package :om)

;;;
;;; Time-stamp: <2017-04-27 14:42:05 andersvi>
;;;
;;; Stream separation library for OM, Anders Vinjar
;;;
;;; Copyright (C) 2017 Anders Vinjar, <anders (dot) vinjar (at) bek (dot) no>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License.  See
;;; http://www.cliki.net/LLGPL for the text of this agreement.

(let* ((srcdir (append (pathname-directory *load-pathname*) '("sources")))
       (stream-sep-files '("stream-seg-lib" "stream-segregation.inside" "stream-seg")))
  (mapc #'(lambda (f)
	    (print f)
	    (compile&load (make-pathname :directory srcdir :name f :type "lisp")))
	stream-sep-files))

(set-lib-release *streamsep-version*)

(let ((*subpackages-list*
       '(("Stream-seg analysis class" nil nil 
	  (stream-seg-segments
	   stream-seg-to-multi-seq
	   stream-seg-to-maquette
	   stream-seg-markers)
	  nil)
	 ("Main interface" nil nil (cs-segregate-streams) nil)
	 ("Utils" nil nil (f->mel mc->mel) nil))))
  (fill-library *subpackages-list*))
 
(print
 (format nil "
;; ============================================
;;  Stream-Seg - Stream separation Library for OM
;;  Version:	 ~A
;;  Date:	 ~A
;;  Author:	 Anders Vinjar
;; ============================================
"
	 *streamsep-version*
	 *streamsep-date*))

;; generate html doc:
;; (gen-lib-reference (exist-lib-p "STREAMSEP"))
