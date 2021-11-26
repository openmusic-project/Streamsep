(in-package :om)

;;;
;;; Time-stamp: <2021-11-26 10:06:16 andersvi>
;;;
;;; Stream separation library for OM, Anders Vinjar
;;;
;;; Copyright (C) 2021 Anders Vinjar, <anders (at) avinjar (dot) no>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License.  See
;;; http://www.cliki.net/LLGPL for the text of this agreement.

(let* ((srcdir (append (pathname-directory *load-pathname*) '("sources")))
       (stream-sep-files '("stream-seg-lib" "stream-segregation-inside" "stream-seg")))
  (mapc #'(lambda (f)
	    (compile&load (make-pathname :directory srcdir :name f)))
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
;;  URL:	 https://github.com/openmusic-project/Streamsep
;;  Author:	 Anders Vinjar
;; ============================================
"
	 *streamsep-version*
	 *streamsep-date*))

;; generate html doc:
;; (gen-lib-reference (exist-lib-p "STREAMSEP"))
