(deftype piece () '(integer -1 1))

(deftype board ()
  '(array piece (4 4)))

(defun clean-board ()
  (make-array '(4 4) :element-type 'piece :initial-element 0))

(defun make-line (str)
  (let ((b (clean-board))
	(i 0))
    (loop for x across str
       unless (char= x #\Newline)
       do (setf (row-major-aref b i) (if (char= x #\Space) 0 1))
	 (incf i))
    b))

(defun lines-reflections (line)
  (let ((l1 line)
	(l2 (clean-board))
	(l3 (clean-board))
	(l4 (clean-board)))
    (loop for row from 0 below 4
       do (loop for col from 0 below 4
	     for x = (aref l1 row col)
	     for -ro = (- 3 row)
	     for -co = (- 3 col)
	     do (setf (aref l2 row -co) x
		      (aref l3 -ro col) x
		      (aref l4 -ro -co) x)))
    (list l1 l2 l3 l4)))

(defun rot-line (line)
  (let ((l (clean-board)))
    (loop for row from 0 below 4
       do (loop for col from 0 below 4
	     do (setf (aref l (- 3 col) row) (aref line row col))))
    l))

(defun lines (line)
  (let ((lines (append (lines-reflections line) (lines-reflections (rot-line line)))))
    (remove-duplicates lines :test #'equalp)))

(let ((line-strings
       '(
	 "
xxxx
    
    
    " "
    
xxxx
    
    " "
x   
 x  
  x 
   x" "
xx  
  x 
   x
    " "
x   
  x 
   x
  x " "
x   
    
 x x
  x " "
 41 
3  2
    
    " "
 4  
3  2
    
 5  ")))
  (let ((lines (remove-duplicates (mapcan #'lines (mapcar #'make-line line-strings))
				  :test #'equalp)))
    (defparameter *lines* (make-array (list (length lines)) :element-type 'board :initial-contents lines))))

(defun determine-game (board)
  (let* ((lc (first (array-dimensions *lines*)))
	 (c (make-array (list lc) :element-type '(integer -4 4) :initial-element 0)))
    (loop for p from 0 below 16
       for v = (row-major-aref board p)
       do (loop for l from 0 below lc
	     do (incf (aref c l) (* (row-major-aref (aref *lines* l) p) v))))
    (format t "~a~%" c)
    (loop for count across c
       count (= count 4) into +
       count (= count -4) into -
       finally (return (if (zerop +)
			   (if (zerop -)
			       0	;tie
			       -1	;- wins
			       )
			   (if (zerop -)
			       +1	;+ wins
			       -2	;invalid game
			       ))))))

(defconstant +threes+ (make-array '(16) :element-type '(integer 0 #.(expt 3 15))
				  :initial-contents (mapcar (lambda (x) (expt 3 x))
							    '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))))
(defconstant +board-num-shift+ (/ (1- (expt 3 16)) 2))

(defun board-num (board)
  (+
   (loop for i from 0 below 16
      summing (* (row-major-aref board i) (aref +threes+ i)))
   +board-num-shift+
   ))

(defparameter *game-memoize* (make-array '(43046721) :element-type '(integer -2 2) :initial-element 2)
  "-2 <=> invalid
-1 <=> - wins
 0 <=> tie
 1 <=> + wins
 2 <=> undefined")

(defun board-from-num (bn)
  (decf bn +board-num-shift+)
  (let ((board (clean-board)))
    (loop for i from 15 downto 0
       for a = (abs bn)
       for p across #(7174453 2391484 797161 265720 88573 29524 9841 3280 1093 364 121 40 13 4 1 0)
       do (setf (row-major-aref board i)
		(if (<= a p)
		    0
		    (let ((s (signum bn)))
		      (decf bn (* s (aref +threes+ i)))
		      s))))
    board))

(defun game-result (board-num)
  (let ((m (aref *game-memoize* board-num)))
    (when (= m 2)
      (let ((m* (determine-game (board-from-num board-num))))
	(setf (aref *game-memoize* board-num) m*
	      m m*)))
    (case m
      (1 :player-1)
      (0 :tie)
      (-1 :player-2)
      (-2 :invalid))))

