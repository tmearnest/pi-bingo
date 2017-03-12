(ql:quickload '(parenscript css-lite cl-who cl-utilities hunchentoot random-state)) 

(defpackage :pi-bingo
  (:use :cl :cl-who :parenscript :cl-utilities :hunchentoot :random-state :css-lite)
  (:shadow :%)
  (:export write-game-template load-game start-game main-loop))

(in-package :pi-bingo)

(defconstant +card-cols+ 5)
(defconstant +card-rows+ 5)
(defconstant +card-title+ "BINGO")

(defconstant +stylesheet+
  (concatenate 'string
	       "@import url('https://fonts.googleapis.com/css?family=Bubblegum+Sans|Diplomata+SC');"
	       (let* ((viewport 80.0)
		      (cell-edge (/ viewport (max (+ 1 +card-rows+) +card-cols+)))
		      (card-width (* cell-edge +card-cols+))
		      (card-height (* cell-edge (+ 1 +card-rows+)))
		      (cell-dim-str (concatenate 'string (write-to-string cell-edge) "vmin"))
		      (card-height-str (concatenate 'string (write-to-string card-height) "vmin"))
		      (card-width-str (concatenate 'string (write-to-string card-width) "vmin")))
		 (css (("body")
		       (:background-color "#ffa"))
		      (("table")
		       (:border-collapse "collapse"
					 :table-layout "fixed"
					 :height card-height-str
					 :width card-width-str
					 :position "absolute"
					 :top "50%"
					 :left "50%"
					 :transform "translate(-50%, -50%)"))
		      ((".bingo:hover")
		       (:background-color "#000"))
		      ((".marked:hover")
		       (:background-color "#fbb"))
		      ((".free:hover")
		       (:background-color "#bfb"))
		      ((".free")
		       (:background-color "#bfb"))
		      ((".marked")
		       (:background-color "#fbb"))
		      ((".unmarked")
		       (:background-color "#fff"))
		      ((".bingo")
		       (:font-family "'Diplomata SC', cursive"
				     :background-color "#000"
				     :color "#fff"
				     :font-size "3em"))
		      (("td")
		       (:border "1px solid black"
				:padding "4px"
				:font-family "'Bubblegum Sans', cursive"
				:font-size "1.5em"
				:height cell-dim-str
				:width cell-dim-str
				:word-wrap "break-word"
				:text-align "center"
				:vertical-align "middle"))
		      (("td:hover")
		       (:background-color "#f5f5f5"))
		      (("#title")
		       (:width "100%"
			       :font-family "'Diplomata SC', cursive"
			       :font-size "3em"
			       :text-align "center"
			       :padding "4px 0px"))
		      (("#config")
		       (:padding "12px"
				 :font-family "'Bubblegum Sans', cursive"
				 :width "50em"
				 :position "absolute"
				 :top "50%"
				 :left "50%"
				 :transform "translate(-50%, -50%)"
				 :background-color "#fff"
				 :border "1px solid #000"))
		      (("label")
		       (:float "left"
			       :width "7em"
			       :margin-right "1em"
			       :color "#000"
			       :font-size "1.5em"))
		      (("#save")
		       (:width "16em"
			       :font-size "1.5em"
			       :border "1px solid #000"
			       :background-color "#000"
			       :color "#fff"
			       :margin "2px"
			       :font-family "'Bubblegum Sans', cursive"))
		      (("input[type=text]")
		       (:width "16em"
			       :border "1px solid #000"
			       :padding "6px 12px"
			       :box-sizing "border-box"
			       :font-family "'Bubblegum Sans', cursive"
			       :font-size "1.5em"
			       :background-color "#f5f5f5"
			       :margin "2px"))
		      (("textarea")
		       (:width "16em"
			       :height "60vmin"
			       :font-family "'Bubblegum Sans', cursive"
			       :font-size "1.5em"
			       :padding "12px 12px"
			       :margin "2px"
			       :box-sizing "border-box"
			       :border "1px solid #000"
			       :background-color "#f5f5f5"
			       :resize "none"))))))

(defvar *game-file-prefix* "/Users/earnest3/bingo/state")
(defvar *word-list* nil)
(defvar *free-word* nil)

(defun get-current-state-filename ()
  "Return latest state file using *game-file-prefix*"
  (car
   (sort
    (mapcar #'namestring
	    (directory (concatenate 'string *game-file-prefix* "*")))
    #'string-greaterp)))

(defun load-game ()
  "Load current state file and start www"
  (with-open-file (in (get-current-state-filename))
    (with-standard-io-syntax
      (let ((db (read in)))
	(setf *word-list* (getf db :words))
	(setf *free-word* (getf db :free))))))

(defun gen-save-filename ()
  "Generates a state filename using *game-file-prefix*-YYYYMMDDHHMMSS"
  (multiple-value-bind
      (second minute hour day month year)
      (get-decoded-time)
    (format nil "~a-~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d"
	    *game-file-prefix*
	    year
	    month
	    day
	    hour
	    minute
	    second)))

(defun write-game-state (words free)
  "Write a state file using a word list and the free word"
  (with-open-file (out (gen-save-filename)
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print `(:words ,words
		      :free ,free)
	     out))))

(defun save-game ()
  "Write out a new state file"
  (write-game-state *word-list* *free-word*))

(defun write-game-template ()
  "Write an empty save game file"
  (let ((word-list (loop for x from 1 to (* +card-cols+ +card-rows+) collect (format nil "word~a" x)))
	(free "free"))
    (write-game-state word-list free)))


(defun make-card (seed)
  "Construct a 2d list of words and set the center cell to the free word"
  (let* ((used-words nil)
	 (rng (make-generator :mersenne-twister-32 seed))
	 (card
	  (labels ((get-unique-word (gen used)
		     (let ((word (elt *word-list* (random-int gen 0 (length *word-list*)))))
		       (if (member word used :test #'equal)
			   (get-unique-word gen used)
			   word)))
		   (collect-n (n fn)
		     (do ((x 0 (1+ x))
			  (list nil (cons (funcall fn) list)))
			 ((= x n) list)))
		   (get-word ()
		     (push (get-unique-word rng used-words) used-words)
		     (car used-words))
		   (get-row ()
		     (collect-n +card-cols+ #'get-word)))
	    (collect-n +card-rows+ #'get-row))))
    (setf (elt
	   (elt card
		(floor +card-cols+ 2))
	   (floor +card-rows+ 2))
	  *free-word*)
    card))

(defun stringify-word-list ()
  "Convert *word-list* into string of words separated by a newline"
  (format nil "~{~A~^~%~}"
	  (sort (copy-list *word-list*) #'string-lessp)))
  
(defun parse-word-list (str)
  "Convert string of words separeted by newline back into *word-list*"
  (remove-if (lambda (s) (= (length s) 0))
    (mapcar (lambda (x) (string-trim '(#\linefeed #\return #\space #\tab) x) )
	    (split-sequence  #\newline str))))

(defun launch-server ()
  "Start www"
  (start (make-instance 'easy-acceptor :port 8080)))

(defun start-game ()
  "Load game and launch server"
  (load-game)
  (launch-server))

(defun main-loop ()
 "Command line entry point. Game state should be loaded prior to calling"
 (launch-server)
 (sb-thread:join-thread
  (find-if
   (lambda (th)
     (string= (sb-thread:thread-name th) "hunchentoot-listener-*:8080"))
   (sb-thread:list-all-threads))))


(defmacro html-page ((&key title) (&key script) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
	    (:head
	     (:meta :charset "utf-8")
	     (:style (str +stylesheet+))
	     ,(when script
	       `(:script :type "text/javascript" (str ,script)))
	     (:title ,title))
	    (:body ,@body))))


(define-easy-handler (config-page :uri "/words" :default-request-type :post) (free words)
  (when free
    (setf *free-word* free))
  (when words
    (setf *word-list* (parse-word-list words)))
  (when (or free words)
    (save-game))
  (html-page
      (:title "BINGO!")
      (:script nil)
      (:div :id "config"
	    (:div :id "title" "WORDS")
	    (:form :action "/words" :method "post"
		   (:label :for "free" "Free space" )
		   (:input :id "free" :type "text" :name "free" :value *free-word*)
		   (:br)
		   (:label :for "words" "Word list")
		   (:textarea :cols "80" :rows "25" :name "words" :id "words"
			      (str (stringify-word-list)))
		   (:br)
		   (:label :for "save" (str #\NARROW_NO-BREAK_SPACE))
		   (:input :id "save" :type "submit" :value "Submit")))))

(define-easy-handler (landing-page :uri "/bingo") ()
  (redirect (format nil "/bingo-card?id=~d" (random (expt 2 31)))))

(define-easy-handler (bingo-card-page :uri "/bingo-card" :default-request-type :get) (id)
  (let ((card (make-card (parse-integer (string id)))))
    (html-page
	(:title "BINGO!")
	(:script (ps
		   (defun toggle-mark-callback (elem)
		     (cond ((string= (chain elem class-name) "unmarked")
			    (setf (chain elem class-name) "marked"))
			   ((string= (chain elem class-name) "marked")
			    (setf (chain elem class-name) "unmarked"))))))
      (:table
       (:tr
	(dolist (c (coerce +card-title+ 'list))
	  (htm (:td :class "bingo" (str (string c))))))
       (loop for row in card and row-id from 0 collect
	    (htm (:tr (loop for word in row and col-id from 0 collect
			   (let ((cls (if (and (= (floor +card-rows+ 2) row-id)
					       (= (floor +card-cols+ 2) col-id))
					  "free"
					  "unmarked")))
			     (htm (:td :class cls
				       :id (format nil "b~ax~a" row-id col-id)
				       :onclick (ps (toggle-mark-callback this))
				       (str word))))))))))))
