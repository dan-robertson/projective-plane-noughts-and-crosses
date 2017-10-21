(ql:quickload 'cl-cairo2)


(in-package #:ppox)
(use-package 'cl-cairo2)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

;global for now
(defparameter *padd-factor* 0.9)

(defmacro with-saved-context ((&optional (context '*context*)) &body body)
  (let ((c (gensym "CONTEXT")))
    `(let ((,c ,context))
       (prog2
	   (save ,c)
	   (progn ,@body)
	 (restore ,c)))))

(defun cross ()
  (with-saved-context ()
    (translate (/ (- 1 *padd-factor*) 2) (/ (- 1 *padd-factor*) 2))
    (scale *padd-factor* *padd-factor*)
    (move-to 0 0)
    (line-to 1 1)
    (move-to 0 1)
    (line-to 1 0)))

(defun naught ()
  (with-saved-context ()
    (translate (/ (- 1 *padd-factor*) 2) (/ (- 1 *padd-factor*) 2))
    (scale *padd-factor* *padd-factor*)
    (move-to 1 0.5)
    (arc 0.5 0.5 0.5 0 (* 2 pi))))

(defun grid ()
  (loop for p in '(0.25 0.5 0.75)
     do (move-to 0 p)
       (line-to 1 p)
       (move-to p 0)
       (line-to p 1)))

(defmacro in-grid-square ((row col &optional (context '*context*)) &body body)
  (let ((c (gensym "CONTEXT")))
    `(let ((,c ,context))
       (with-saved-context (,c)
	 (scale 0.25 0.25)
	 (translate ,col ,row)
	 (translate 0.025 0.025)
	 (scale 0.95 0.95)
	 ,@body))))

(defun red-cross (row col)
  (in-grid-square (row col)
    (set-source-rgb 1 0 0)
    (set-line-width 0.05)
    (cross)
    (stroke)
    (set-line-width 0.01)
    (set-source-rgb 0 0 0)))

(defun draw-strategy (strat &optional (board (clean-board)))
  ;; draw the grid first
  (flet ((rw (n) (truncate (/ n 4)))
	 (cl (n) (mod n 4)))
    (if (and (consp strat)
	     (loop for i from 0 below 16
		  thereis (row-major-aref (cadr strat) i)))
	(progn
	  ;; draw the cross
	  (red-cross (rw (car strat)) (cl (car strat)))
	  ;; draw the rest
	  (let ((st (cadr strat)))
	    (setf (row-major-aref board (car strat)) 1)
	    (loop for row from 0 below 4
	       do (loop for col from 0 below 4
		     for rc = (+ col (* row 4))
		     unless (= rc (car strat))
		     do (in-grid-square (row col)
			    (case (aref board row col)
			      (1 (cross) (stroke)) ;fill in the board
			      (-1 (naught) (stroke))
			      (0 (when strat ;recurse in blanks
				   (setf (aref board row col) -1)
				   (draw-strategy (aref st row col) board)
				   (setf (aref board row col) 0)))))))
	    (setf (row-major-aref board (car strat)) 0)))
	;; Or just draw the board
	(progn
	  (when (consp strat)
	    (stroke)
	    (red-cross (rw (car strat)) (cl (car strat))))
	  (loop for row from 0 below 4
	     do (loop for col from 0 below 4
		   do (in-grid-square (row col)
			(case (aref board row col)
			  (1 (cross))
			  (-1 (naught)))))
	     finally (stroke)))))
  (grid)
  (stroke))

(defun draw (size)
  (let*  ((surface (create-ps-surface "strat.ps" size size))
	 ;(surface (create-image-surface :rgb24 size size))
	 (*context* (create-context surface)))
    (destroy surface)
    (scale size size)
    (set-source-rgb 1 1 1)
    (paint)
    (set-line-width 0.01)
    (set-source-rgb 0 0 0)
    (draw-strategy *strat*)
    (destroy *context*)
    (surface-write-to-png surface "strat.png")
    (destroy surface)))

(defun draw-tile (scale-factor x y imsize path)
  (let* ((surface (create-image-surface :rgb24 imsize imsize))
	 (*context* (create-context surface)))
    (scale imsize imsize)
    (translate (- x) (- y))
    (scale (expt 2d0 scale-factor) (expt 2d0 scale-factor))
    (set-source-rgb 1 1 1)
    (paint)
    (set-line-width 0.01)
    (set-source-rgb 0 0 0)
    (draw-strategy *strat*)
    (destroy *context*)
    (surface-write-to-png surface path)
    (destroy surface)))

(defun draw-tiles (max-zoom size path-prefix &optional (min-zoom 0) (final-zoom max-zoom))
  (loop for sf from min-zoom to final-zoom
     do (loop for x from 0 below (expt 2 sf)
	   do (loop for y from 0 below (expt 2 sf)
		 do (draw-tile sf x y size (format nil "~a~3,'0d_~3,'0d_~3,'0d.png"
						   path-prefix
						   (- max-zoom sf)
						   x y))
		   (write-char #\.))
	     (write-char #\*))
       (fresh-line))
  (* size (expt 2 max-zoom)))

(defun draw-tiles2 (sf size path-prefix)
  (loop for x from (1- (expt 2 sf)) downto 0
     do (loop for y from (1- (expt 2 sf)) downto 0
	   do (draw-tile sf x y size (format nil "~a~3,'0d_~3,'0d_~3,'0d.png"
					     path-prefix
					     0
					     x y))
	     (write-char #\.))
       (write-char #\*))
  (* size (expt 2 sf)))
