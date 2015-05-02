(deftype piece () '(integer -1 1))

(deftype board ()
  '(simple-array piece (4 4)))

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
    
 xxx
x   
    " "
    
  xx
xx  
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
  (declare (type board board)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((lc (first (array-dimensions *lines*)))
	 (c (make-array (list lc) :element-type '(integer -4 4) :initial-element 0)))
    (loop for p from 0 below 16
       for v = (row-major-aref board p)
       do (loop for l from 0 below lc
	     do (incf (aref c l) (* (row-major-aref (aref *lines* l) p) v))))
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

(defconstant +game-memoize+ (make-array '(43046721) :element-type '(integer -2 2) :initial-element 2)
  "-2 <=> invalid
-1 <=> - wins
 0 <=> tie
 1 <=> + wins
 2 <=> undefined")

(defun board-from-num (bn)
  (declare (type (integer 0 43046721) bn)
	   (optimize (speed 3) (safety 0) (debug 0)))
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

(defun game-result (board-num &optional board)
  (declare (type (or null board) board)
	   (type (integer 0 43046721) board-num)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let ((m (aref +game-memoize+ board-num)))
    (when (= m 2)
      (let ((m* (determine-game (or board (board-from-num board-num)))))
	(setf (aref +game-memoize+ board-num) m*
	      m m*)))
    (case m
      (1 :player-1)
      (0 :tie)
      (-1 :player-2)
      (-2 :invalid))))

(defparameter *pc* 0)
(defparameter *pcn* 5)
(defparameter *pcl* 120)

(defun pc (x)
  (when (>= (incf *pc*) *pcl*)
    (setf *pcl* (* (incf *pcn*) *pcl*))
    (format t "Tried ~a =~a! combinations~%" *pc* (1- *pcn*))))

(defun find-strategies* (board bn n turn btp &aux (we1 (= turn 1)))
  "`board' is a board, `bn' is the corresponding number. This returns the results:
win1, win2, tie1, tie2,
where they are booleans corresponding to whether there exist winning/non-losing
strategies for players 1 or 2. This considers a game starting with the passed
board with `n' turns left where turn = -1 if it is player 2's turn or +1 if it is
player 1's turn."
  (declare (type board board)
	   (type (integer 0 43046721) bn)
	   (type (member -1 1) turn)
	   (type (integer 0 16) n btp)
	   (type (boolean) we1)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (pc btp)
  ;; First check if anyone has won:
  (let ((r (game-result bn board)))
    (case r
      (:player-1 (return-from find-strategies* (values t nil t nil)))
      (:player-2 (return-from find-strategies* (values nil t nil t)))
      (:tie (when (= n 0)
	      (return-from find-strategies* (values nil nil t t))))
      (:invalid (error "invalid board reached: ~a" board))))
  ;; Now try the moves we might make
  (let ((win1 (if we1 nil t))
	(win2 (if we1 t nil))
	(noloss1 (if we1 nil t))
	(noloss2 (if we1 t nil)))
    (loop for i from 0 below 16
       for p = (row-major-aref board i)
       when (= p 0)
       do (prog2 (setf (row-major-aref board i) turn)
	      (multiple-value-bind
		    (win+ win- nlo+ nlo-)
		  (find-strategies*
		   board
		   (+ bn (* turn (aref +threes+ i)))
		   (1- n)
		   (- turn)
		   i)
		;; We want to collate this: is there some move such that the current player wins/doesn't lose
		;; does the other player win/not lose no matter what we do?
		(if we1
		    (setf win1 (or win1 win+)
			  win2 (and win2 win-)
			  noloss1 (or noloss1 nlo+ (not win-))
			  noloss2 (and noloss2 (or nlo- win- (not win+))))
		    (setf win2 (or win2 win-)
			  win1 (and win1 win+)
			  noloss2 (or noloss2 nlo- (not win+))
			  noloss1 (and noloss1 (or nlo+ win+ (not win-)))))
		)
	    (setf (row-major-aref board i) 0))
       until (if we1
		  (and win1 (not win2) noloss1 (not noloss2))
		  (and win2 (not win1) noloss2 (not noloss1))))
    (values win1 win2 noloss1 noloss2)))

(defun find-strategies ()
  (find-strategies* (clean-board) (board-num (clean-board)) 16 1 0))

(defun find-strategy* (board bn n)
  "looks for a strategy for player 1 to win.
returns a list (place arr) where place is where player 1 should play.
NB place is row-major index, from 0 below 16.
arr is a 4x4 array. each element is either:
  nil - if the move is invalid
  t - if player 1 wins
  (place . arr) as before
each representing the outcome if player 2 plays in that place.
If there isn't a winning strategy, nil is returned.
If the game has been won, t is returned."
  (declare (type board board)
	   (type (integer 0 43046721) bn)
	   (type (integer 0 16) n)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (pc 0)
  ;; See if we've won:
  (let ((r (game-result bn board)))
    (case r
      (:player-1 (return-from find-strategy* t))
      (:player-2 (return-from find-strategy* nil))
      (:tie (when (<= n 1) (return-from find-strategy* nil)))
      (:invalid (error "invalid board reached: ~a" board))))
  (loop for p1 from 0 below 16 ;for anywhere player 1 can play,
     for strat = (make-array '(4 4) :initial-element nil)
     for alwayswin =
       (and
	(zerop (row-major-aref board p1)) ;we ignore games where p1 can't place his peice
	(prog2
	    (setf (row-major-aref board p1) +1)
	    (or
	     (eq :player-1 (game-result (+ bn (expt 3 p1)) board)) ;does p1 play a winning move?
	     (loop for p2 from 0 below 16 ;Can we always win if we play there
		always (or
			(not (zerop (row-major-aref board p2)))
			(= p1 p2)
			(prog2
			    (setf (row-major-aref board p2) -1)
			    (setf (row-major-aref strat p2)
				  (find-strategy* board (+ bn (expt 3 p1) (- (expt 3 p2))) (- n 2)))
			  (setf (row-major-aref board p2) 0)))))
	  (setf (row-major-aref board p1) 0)))
     until alwayswin
     finally (return (and alwayswin (list p1 strat)))))

(defun find-strategy ()
  "serches for a strategy for player 1 to win"
  (find-strategy* (clean-board) (board-num (clean-board)) 16))

(defun write-board (board &optional (pre ""))
	   (loop for i from 0 below 4
	      do (write-char #\Newline)
		(write-string pre)
		(loop for j from 0 below 4 
		   do (write-char (ecase (aref board i j)
				    (1 #\x) (0 #\.) (-1 #\o))))))

(defun write-strategy (strat &optional (pre "") (board (clean-board)))
  (if (consp strat)
      (progn
	(format t "~acrosses should play ~a~%" pre
		(aref #((0 0) (0 1) (0 2) (0 3)
			(1 0) (1 1) (1 2) (1 3)
			(2 0) (2 1) (2 2) (2 3)
			(3 0) (3 1) (3 2) (3 3))
		      (car strat)))
	(setf (row-major-aref board (car strat)) +1)
	(let ((pre (concatenate 'string pre " "))
	      (st (cadr strat)))
	  (loop for i from 0 below 16
	     for s = (row-major-aref st i)
	     when s
	     do (format t "~aif naughts plays like:" pre)
	       (setf (row-major-aref board i) -1)
	       (write-board board pre) (write-line "")
	       (write-strategy s pre board)
	       (setf (row-major-aref board i) 0))
	  (setf (row-major-aref board (car strat)) 0)))))
