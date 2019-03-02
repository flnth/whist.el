;;; whist.el --- Window based history management. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Florian Thevißen

;; Author: Florian Thevißen <mail@florian-thevissen.de>
;; Maintainer: James Nguyen <mail@florian-thevissen.de>
;; URL: https://github.com/flnth/whist.el
;; Version: 0.6
;; Package-Requires:
;; Keywords: tools

(require 'cl-lib)

;; TODO: use window-parameters
;; TODO: https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/ewoc.el

(defcustom whist--messages-p nil
  "If non-nil, `whist--message' will produce a message. Toggling
this on will cause whist to produce debug messages."
  :group 'whist
  :type 'boolean)

(defun whist--message (format-string &rest args)
  "Produce a message if `whist-message-on-p' is non-nil.
The message is produced with the `message' function. In any case,
return the formatted string. FORMAT-STRING and ARGS are passed to
`message' or `format' as is."
  (if whist--messages-p
      (apply #'message format-string args)
    (apply #'format format-string args)))


;;; data structures
;;;; whist--node

(defstruct (whist--node
			(:type vector)
			(:constructor whist--node-create_)
			:named
			(:copier nil))
  "A node in history."
  prev
  next
  buffer-name
  point)

(defvar whist--null-node (whist--node-create_))
(setf (whist--node-prev whist--null-node) whist--null-node)
(setf (whist--node-next whist--null-node) whist--null-node)

(setf (whist--node-point whist--null-node) 0)
(setf (whist--node-buffer-name whist--null-node) "NULL")

(cl-defun whist--node-create (buffer &key point next prev)
  "Creates new node from buffer. If no point is given, set point to 1.
   TODO: if no point is given, set point to what it's at in some
   win."
  (let* ((buffer-name (cond ((bufferp buffer) (buffer-name buffer))
							((get-buffer buffer) buffer)
							(t nil)))
		 (point (if (integerp point) point 1))
		 (next (if (not (null next)) next whist--null-node))
		 (prev (if (not (null prev)) prev whist--null-node)))
	(when buffer-name
	  (whist--node-create_ :buffer-name buffer-name :point point :next next :prev prev))))

(defun whist--node-valid? (node)
  "Returns true if node has valid buffer and point."
  (and (not (null node))
	   (not (equal node whist--null-node))
	   (get-buffer (whist--node-buffer-name node))
	   (not (null (whist--node-point node)))))

(defun whist--node-chain-count-forward (node)
  "Count total number of total nodes in chain, including itself,
including invalid nodes."
  (let ((count 1)
		(cur node))
	(while (not (equal whist--null-node (whist--node-next cur)))
	  (setf cur (whist--node-next cur))
	  (incf count))
	count)
  )

;;;; whist--dll
(defstruct (whist--dll
			(:type vector)
			:named
			(:constructor whist--dll-create))
  "The navigation-history of a window."
  window
  (rear-node  whist--null-node)
  (cursor     whist--null-node)
  (front-node whist--null-node)
  (size 0))

(defun whist--dll-get-cursor (whist &optional prefer-next)
  "Returns cursor taking into account that the node cursor points
  could have been invalidated. If PREFER-NEXT is t, the nodes
  after the now-invalid cursor is preferred, otherwise the nodes
  before are. If no valid node could be found, return
  WHIST--NULL-NODE."
  (let ((cursor (whist--dll-cursor whist)))
	(if (whist--node-valid? cursor)
		cursor
	  (let* ((next-fun (if prefer-next #'whist--node-next #'whist--node-prev))
			 (prev-fun (if prefer-next #'whist--node-prev #'whist--node-next))
			 (next-best-node (whist--dll-find-next-valid next-fun cursor)))
		(if (whist--node-valid? next-best-node)
			next-best-node
		  (let ((prev-best-node (whist--dll-find-next-valid prev-fun cursor)))
			(if (whist--node-valid? prev-best-node)
				prev-best-node
			  whist--null-node)))))))

(defun whist--dll-find-next-valid (fun-next node)
  "Helper to find next valid node among nodes returned by
fun-next."
  (whist--message "called find-next-valid with %s" node)
  (let ((next-node (funcall fun-next node)))
	(cond ((whist--node-valid? next-node) next-node)
		  ((equal next-node whist--null-node) node)
		  (t (funcall 'whist--dll-find-next-valid fun-next next-node)))))

(cl-defun whist--dll-get (whist arg &optional (cyclic nil))
  "Get next valid node in WHIST in direction specified via arg.
-1 is the previous node, +1 the next one."
  (let* ((forward (> arg 0))
		 (cursor (whist--dll-cursor whist))
		 (cursor-null (equal cursor whist--null-node)))
	(cond
	 ((and cursor-null forward)			; history front:  no next
	  whist--null-node)
	 ((and cursor-null (not forward)) ; history front:  backward to front node or first valid one afterthat
	  (let ((search-result (whist--dll-find-next-valid #'whist--node-prev (whist--dll-front-node whist)))))
	  (whist--dll-front-node whist))
	 ((and (not cursor-null) forward)	; history middle: backward to next valid
	  (let ((search-result (whist--dll-find-next-valid #'whist--node-next cursor)))
		(if (whist--node-valid? search-result)
			search-result cursor)))
	 ((and (not cursor-null) (not forward))	; history middle: forward to next valid
	  (let ((search-result (whist--dll-find-next-valid #'whist--node-prev cursor)))
		(if (whist--node-valid? search-result)
			search-result cursor))))
	)
  )

(cl-defun whist--dll-push! (whist new-node &key (to-front t))
  "Push NEW-NODE TO-FRONT of whist, or after CURSOR,
dropping everything after CURSOR first."
  (whist--message "whist--dll-push!")
  (if to-front
	  (let* ((front-node (whist--dll-front-node whist)))
		(if (equal 0 (whist--dll-size whist)) ; empty?
			(progn							  ; add front/rear node
			  (setf (whist--dll-front-node whist) new-node)
			  (setf (whist--dll-rear-node whist) new-node))
		  (progn						; else -> add-links
			(setf (whist--node-next front-node) new-node)
			(setf (whist--node-prev new-node) front-node)
			(setf (whist--dll-front-node whist) new-node)))
		(setf (whist--dll-cursor whist) whist--null-node))
	;; push-in-place:  drop nodes after CURSOR, push to front
	(progn
	  (whist--dll-drop-future! whist)
	  (whist--dll-push! whist new-node :to-front t)))
  (incf (whist--dll-size whist) 1))

(defun whist--dll-drop-future! (whist)
  "Removes nodes after CURSOR."
  (whist--message "  dropping future")
  (let ((cur (whist--dll-cursor whist)))
	(when (not (null cur))
	  (setf (whist--dll-size whist) (- (whist--dll-size whist)
									   (whist--node-chain-count-forward (whist--node-next cur))))
	  (setf (whist--node-next cur) whist--null-node)
	  (setf (whist--dll-front-node whist) cur))))

(defun whist--dll-drop-first! (whist)
  "Remove first dll node."
  (whist--message "  dropping first node")
  (let ((size (whist--dll-size whist)))
	(cond ((equal size 0) nil) ; no nodes in whist -> return
		  ((equal size 1)      ; one node -> remove
		   (setf (whist--dll-rear-node whist) nil)
		   (setf (whist--dll-front-node whist) nil)
		   (setf (whist--dll-cursor whist) whist--null-node)
		   (setf (whist--dll-size whist) 0))
		  (t (let* ((first (whist--dll-rear-node whist)) ; >=2 nodes -> unlink first,
					(second (whist--node-next first)))	  ; make second first
			   (setf (whist--node-next first) nil )
			   (setf (whist--node-prev second) nil)
			   (setf (whist--dll-rear-node whist) second)
			   (when (equal (whist--dll-cursor whist) first)
				 (setf (whist--dll-cursor whist) second))
			   (decf (whist--dll-size whist)))))))

(defun whist--dll-print (whist &optional front-to-rear)
  (if (or (null whist)
		  (not  (> (whist--dll-size whist) 0)))
	  (message "No history for window or empty history.")
	(let* ((fun-start (if front-to-rear #'whist--dll-front-node #'whist--dll-rear-node))
		   (fun-next-el (if front-to-rear #'whist--node-prev #'whist--node-next))
		   (cur (funcall fun-start whist))
		   (dll-cur (whist--dll-cursor whist))
		   (str ""))
	  (while (not (equal cur whist--null-node))
	  	(let ((bufname (whist--node-buffer-name cur))
			  (valid (whist--node-valid? cur)))
	  	  (when (equal cur dll-cur)
	  	  	(setf bufname (concat "(" bufname ")" )))
		  (when (not valid)
			(setf bufname (concat "{" bufname "}")))
	  	  (setf str (concat str "," bufname))
	  	  (setf cur (funcall fun-next-el cur))
		  )
	  	)
	  (message str))))

;;; introspection

(defun whist-print-status ()
  "Print status in message buffer."
  (interactive)
  (message "-- whist status ------------------")
  (message "  %s  window histories" (cl-loop for w in whist--alist count w))
  ;; (message "  %s/%s  windows total/live" (cl-loop for ))
  (message "----------------------------------")
  nil
  )


;;; public interface
;;;; state

(defvar whist--alist '() "An association list containing (window
. whist) pairs.")
(defvar whist--last-used nil "The last-used whist.")
(defvar whist--max-size 10)

;;;; dll creation/access/deletion
(cl-defun whist--new (&key window force-add)
  "Creates new dll for WINDOW and adds to whist--alist.
Does not check for duplicates if FORCE-ADD is true."
  (let* ((win (if (null window) (selected-window) window))
		 (buf (window-buffer win))
		 (point (window-point win))
		 (duplicate (if force-add nil (cdr (assoc win whist--alist)))))
	(if duplicate
		(progn
		  (setf whist--last-used duplicate)
		  duplicate)
	  (let ((new-whist (whist--dll-create :window win)))
		(setf whist--alist (cons (cons win new-whist) whist--alist))
		(setf whist--last-used new-whist)
		(car whist--alist)))))

(cl-defstruct (copy-tree*
               (:constructor copy-tree*-mem (&optional stack stack-new (hash (make-hash-table)))))
  stack stack-new hash)

(defmacro copy-tree*--push (el el-new mem &optional hash)
"Put EL onto the stack and EL-NEW onto stack-new in the `copy-tree*'
structure MEM. Add a key-value pair mapping EL to EL-NEW in the hash map
of mem."
  (let ((my-el (make-symbol "my-el"))
        (my-el-new (make-symbol "my-el-new"))) ; makes sure `el' is only evaluated once
    (append `(let ((,my-el ,el)
                   (,my-el-new ,el-new))
               (push ,my-el (copy-tree*-stack ,mem))
               (push ,my-el-new (copy-tree*-stack-new ,mem)))
            (and hash
                 `((puthash ,my-el ,my-el-new (copy-tree*-hash ,mem))))
            (list my-el-new))))

(defmacro copy-tree*--pop (el el-new mem)
  `(setq ,el (pop (copy-tree*-stack ,mem))
         ,el-new (pop (copy-tree*-stack-new mem))))

(defun copy-tree*--copy-node (node mem vecp)
  "If NODE is not a `cons' just return it.
Create a new copy of NODE if NODE is a `cons' not already contained in the hash map of mem (a `copy-tree*' structure). Register NODE and its copy as key-value pair in the hash table.
If NODE is already a key of the hash map return its copy.
With non-nil VECP vectors are treated analogously to conses."
  (if (or (consp node)
      (and vecp (vectorp node)))
      (let ((existing-node (gethash node (copy-tree*-hash mem))))
    (if existing-node
        existing-node
      (copy-tree*--push node (if (consp node)
                     (cons nil nil)
                   (make-vector (length node) nil))
                mem t)))
    node))

(defun copy-tree* (tree &optional vecp)
  "Structure preserving version of `cl-copy-tree'."
  (if (or (consp tree)
      (and vecp (vectorp tree)))
      (let* ((tree-new (if (consp tree) (cons nil nil)
             (make-vector (length tree) nil)))
             (mem (copy-tree*-mem))
             next
             next-new)
        (copy-tree*--push tree tree-new mem t)
        (while (copy-tree*--pop next next-new mem)
      (cond
       ((consp next)
        (setcar next-new (copy-tree*--copy-node (car next) mem vecp))
        (setcdr next-new (copy-tree*--copy-node (cdr next) mem vecp)))
       ((and vecp (vectorp next))
        (cl-loop for i from 0 below (length next) do
             (aset next-new i (copy-tree*--copy-node (aref next i) mem vecp))))))
    tree-new)
    tree))

;; TODO: implement proper deep-copy of a whist--dll. Prolly need to copy over
;; the dll by hand. Below doesn't work.

;; (defun whist--copy ()
;;   "Copies the dll from window last active to current-window."
;;   (let* ((last-window (get-mru-window nil nil t))
;; 		 (whist-copy (copy-whist--dll (whist--get :window last-window :create nil))))
;; 	(message "last-window: %s, selected-window: %s" last-window (selected-window))
;; 	(setf (whist--dll-window whist-copy) (selected-window))
;; 	(setf whist--alist (cons `(,(selected-window) . ,whist-copy) whist--alist))
;; 	(setf whist--last-used whist-copy)))

;; (advice-add 'split-window-left-gui :after 'whist--copy)
;; (advice-add 'split-window-below-and-focus :after 'whist--copy)
;; (advice-add 'split-window-above-gui :after 'whist--copy)
;; (advice-add 'split-window-right-and-focus :after 'whist--copy)

(cl-defun whist--get (&key window create)
  "Get the window history for WINDOW. Creates it if it doesn't
exist and CREATE is t."
  (let ((win (if (null window) (selected-window) window)))
	;; whist for win in whist--last-used? return that
	(if (and (not (null whist--last-used))
			 (equal window (whist--dll-window whist--last-used)))
		(progn
		  whist--last-used)
	  ;; whist in global list? return that
	  (let ((match (cdr (assoc win whist--alist))))
		(if match (progn
					(setf whist--last-used match)
					match)
		  ;; no, create new if create t and window exists (!
		  (if (and create (window-live-p win))
			  (let ((new-whist (cdr (whist--new :window win))))
				(setf whist--last-used new-whist)
				new-whist)
			nil))))))

(cl-defun whist--delete (window)
  "Deletes whist for WINDOW if it exists."
  (setf whist--alist (delq (assoc window whist--alist) whist--alist))
  (when (and
		 (not (null whist--last-used))
		 (equal (whist--dll-window whist--last-used) window))
	(setf whist--last-used nil))
  )

;;;; creation/access
(cl-defun whist-add (win buf point &optional new-buf)
  "Add or update location (buf ,point) to whist history of WIN
  depending on what CURSOR currently points to. Removes old
  entries if whist size > whist--max-size."
  (whist--message "-- whist-add for win %s, buf %s, point %s, new-buf %s: " win (buffer-name buf) point new-buf)
  (let* ((whist (whist--get :window win :create t))
		 (cursor (whist--dll-cursor whist)))
	;; cursor behind end?
	(if (equal cursor whist--null-node)
		;; yes -> push to front
		(progn
		  (whist--message "  pushing %s to front, is live-buffer: %s" (buffer-name buf) (buffer-live-p buf))
		  (whist--dll-push! whist (whist--node-create buf :point point) :to-front t)
		  )
	  ;; no -> update node if we're in sync
	  (when (string= (whist--node-buffer-name cursor)
					 (buffer-name buf))
		(whist--message "  updating node")
		(setf (whist--node-point cursor) (if (integerp point) point
										   (progn (message "point nil!") 1))) ; TODO: check here for point validity?? really?
		(when (and (not (null new-buf))
				   (not (equal (get-buffer buf)
							   (get-buffer new-buf))))
		  (whist--dll-drop-future! whist)
		  (setf (whist--dll-cursor whist) whist--null-node)
		  )))))

(defun whist-go (arg &optional window)
  "Go to next node in WINDOW history in the direction specified
  by arg, which can be -1 or 1."
  (let* ((win (if (null window) (selected-window) window))
		 (whist (whist--get :window win :create t))
		 (forward (> arg 0))
		 (target-node (whist--dll-get whist arg)))
	(whist--message "-- whist-go for win %s to target-node buffer %s" win (whist--node-buffer-name target-node) (whist--node-valid? target-node))
	(when (whist--node-valid? target-node)
	  (whist--message "  node valid: %s" target-node)
	  ;; target exists and valid
	  (let ((buf (window-buffer win))
			(point (window-point win)))
		;; add or update location in whist
		(whist-add win buf point)
		;; update cursor
		(setf (whist--dll-cursor whist) target-node)
		;; switch window
		(without-purpose
		  (switch-to-buffer (whist--node-buffer-name target-node) t t)
		  (goto-char (whist--node-point target-node))))))
  nil)

(defun whist-go-back (&optional window)
  (interactive)
  (whist-go -1 window))

(defun whist-go-forward (&optional window)
  (interactive)
  (whist-go 1 window))

(defun whist-print (&optional back-to-front)
  (interactive)
  (whist--dll-print (whist--get) back-to-front)
  )

;;;; hooks

;; ... exclude minibuffer?

;; changing buffer in ... some window, somewhere:
;; (defun whist--on-buffer-list-update ()  ;; ... called very very often :(
;;   (when (not (active-minibuffer-window)))
;;   ;;)
;;   )

;; TODO:  need to advice pop-to-buffer, too?  maybe advise more...?

;;;;; Navigation

(defvar whist--pre-windows (list) "Windows selected before navigation.")
(defvar whist--pre-buffers (list) "Buffers shown in windows before navigation.")
(defvar whist--pre-buffers-points (list) "Points pre-windows were at in pre-buffers.")
(defvar whist--post-windows (list) "Windows selected after navigation.")

;; (defvar whist--pre-buf-point '() "Point of whist--pre-win before switch-to-buffer call.") ; ... no :(, not correct when window changed.

;; (defvar whist--nav-time 0.25 "Time in seconds after which navigation is considered finished, and whist begins to act.")
(defvar whist--nav-timer-started nil "Flag to indicate if timer already started.")

(defun whist--before-navigation ()
  "Called before a navigating call. Collects information."
  (when (not whist--nav-timer-started)
	(whist--message "starting timer")
	(setf whist--pre-windows (cons (selected-window) whist--pre-windows))
	(setf whist--pre-buffers (cons (window-buffer (selected-window)) whist--pre-buffers))
	(setf whist--pre-buffers-points (cons (window-point (selected-window)) whist--pre-buffers-points))
	;; (run-with-timer whist--nav-time nil 'whist--after-navigation-eval)
	(if (null whist--pre-buffers)
		(error "---------------- whist--pre-buffers nil!! ")
	  (progn
		(setf whist--nav-timer-started t)
		(run-with-idle-timer 0 nil 'whist--after-navigation-eval)))))

(defun whist--after-navigation ()
  "Called after a navigating call. Collects information."
  (setf whist--post-windows (cons (selected-window) whist--post-windows)))

(defun whist--after-navigation-eval ()
  "Called `whist--nav-time' after the first navigating call.
Looks at information to determine windows and buffers for window
history. Resets information before end."
  (ignore-errors
	(when whist--nav-timer-started
	  (setf whist--nav-timer-started nil)
	  (let* ((pre-win (car (last whist--pre-windows)))
			 (pre-buf (car (last whist--pre-buffers)))
			 (pre-buf-point (car (last whist--pre-buffers-points)))
			 (post-win (car whist--post-windows))
			 (post-buf (window-buffer post-win)))

		(whist--message "-- whist--after-navigation-eval: ")
		(whist--message "   whist--pre-windows: %s" whist--pre-windows)
		(whist--message "   whist--pre-buffers: %s" whist--pre-buffers)
		(whist--message "   whist-post-windows: %s" whist--post-windows)
		(whist--message "   pre-win: %s " pre-win)

		(if (s-match "Minibuf" (buffer-name post-buf))
			(whist--message "-- skipping minibuffer")
		  (if (equal pre-win post-win)
			  (when (not (equal pre-buf post-buf))
				(setq p pre-buf-point)
				(whist--message "  same window")
				(whist--message "  pre-buf:pre-win | post-buf:post-win:  %s:%s | %s:%s, pre-buf-point: %s" (buffer-name pre-buf) pre-win (buffer-name post-buf) post-win pre-buf-point)
				(whist-add pre-win pre-buf pre-buf-point post-buf) ; NOTE:  pre-buf was sometimes nil here, execution of this now prohibited
				)
			(let* ((pre-buf-lst (car (window-prev-buffers post-win)))
				   (pre-buf (car pre-buf-lst))
				   (pre-buf-marker (caddr pre-buf-lst))
				   (pre-buf-point (if (null pre-buf-marker) 1 (marker-position pre-buf-marker))))
			  (when (and (not (equal pre-buf post-buf))
						 (not (null pre-buf)))
				;; new window selected, buffer there changed
				(whist--message "  new window")
				(whist--message "  post-win | pre-buf:post-buf:  %s | %s:%s live:live %s:%s" post-win (buffer-name pre-buf) (buffer-name post-buf) (buffer-live-p pre-buf) (buffer-live-p post-buf))
				(whist-add post-win pre-buf pre-buf-point post-buf))))))

	  ;; reset state
	  (run-with-idle-timer 0 nil #'(lambda ()
									 (setf whist--pre-windows '())
									 (setf whist--pre-buffers '())
									 (setf whist--pre-buffers-points '())
									 (setf whist--post-windows '())))
	  )))

;; TEMP -------
;; periodically re-set whist--nav-timer-started
(defun whist--reset-nav-timer-started ()
  (setq whist--nav-timer-started nil))
(run-with-idle-timer 60 t #'whist--reset-nav-timer-started)
;; TEMP -------

;; (defun whist--before-switch-to-buffer (buffer-or-name &optional norecord force-same-window)
;;   (when (not norecord)
;; 	(whist--before-navigation)))

;; (defun whist--after-switch-to-buffer (buffer-or-name &optional norecord force-same-window)
;;   (when (not norecord)
;; 	(whist--after-navigation)))
;; (run-with-idle-timer 5 nil 'whist--after-navigation)   doesnt work, somehow.

;; (advice-add 'switch-to-buffer :before 'whist--before-switch-to-buffer)
;; (advice-add 'switch-to-buffer :after  'whist--after-switch-to-buffer)

;; (advice-remove 'switch-to-buffer 'whist--before-switch-to-buffer)
;; (advice-remove 'switch-to-buffer 'whist--after-switch-to-buffer)

;;;;;; pop-to-buffer
(defun whist--before-pop-to-buffer (buffer-or-name &optional action norecord)
  (whist--message "whist--before-pop-to-buffer, norecord: %s" norecord)
  (when (not norecord)
	(whist--message "whist--before-pop-to-buffer:  %s"  (selected-window))
	(whist--before-navigation)))

(defun whist--after-pop-to-buffer (buffer-or-name &optional action norecord)
  (if (not norecord)
	(whist--message "whist--after-pop-to-buffer:  %s"  (selected-window))
	(whist--after-navigation)
	))

(advice-add 'pop-to-buffer :before #'whist--before-pop-to-buffer)
(advice-add 'pop-to-buffer :after #'whist--after-pop-to-buffer)

;; (advice-remove 'pop-to-buffer #'whist--before-pop-to-buffer)
;; (advice-remove 'pop-to-buffer #'whist--after-pop-to-buffer)

;;;;;; pop-to-buffer-same-window
(defun whist--before-pop-to-buffer-same-window (buffer-or-name &optional norecord)
  (whist--message "shit--before-pop-to-buffer, norecord: %s" norecord)
  (when (not norecord)
	(whist--message "whist--before-pop-to-buffer-same-window:  %s"  (selected-window))
	(whist--before-navigation)))

(defun whist--after-pop-to-buffer-same-window (buffer-or-name &optional norecord)
  (if (not norecord)
	(whist--message "whist--after-pop-to-buffer-same-window:  %s"  (selected-window))
	(whist--after-navigation)))

;; (advice-add 'pop-to-buffer-same-window :before #'whist--before-pop-to-buffer-same-window)
;; (advice-add 'pop-to-buffer-same-window :after #'whist--after-pop-to-buffer-same-window)

;;;;;; display-buffer
(defun whist--before-display-buffer (buffer-or-name &optional action frame)
  (whist--message "whist--before-display-buffer:  %s"  (selected-window))
  (whist--before-navigation))

;; maybe also advise select-window, and set-window-buffer?

(defun whist--after-display-buffer (buffer-or-name &optional action frame)
  (whist--message "whist--after-display-buffer:  %s" (selected-window))
  (whist--after-navigation))

(advice-add 'display-buffer :before #'whist--before-display-buffer)
(advice-add 'display-buffer :after #'whist--after-display-buffer)

;;;;;; find-file

;; (advice-remove 'display-buffer #'whist--before-display-buffer)
;; (advice-remove 'display-buffer #'whist--after-display-buffer)

;; !! --------------
;; before:  #<window 47 on *helpful function: equal*>
;; after:  #<window 47 on *helpful function: equal*>
;; before:  #<window 3 on fns.c>
;; after:  #<window 3 on fns.c>
;; !! --------------

(defun whist--before-select-window (window &optional norecord)
  (when whist--nav-timer-started
	;; (whist--message "before-select-window: %s" (selected-window))
	(whist--before-navigation)))

(defun whist--after-select-window (window &optional norecord)
  (when whist--nav-timer-started
	;; (whist--message "after-select-window: %s" (selected-window))
	(whist--after-navigation)))

(advice-add 'select-window :before #'whist--before-select-window)
(advice-add 'select-window :after #'whist--after-select-window)


;;;;; Window deletion

(defun whist--on-delete-window (&optional window)
  (let ((window (if (null window) (selected-window) window)))
	(whist--delete window)))

(advice-add 'delete-window :before #'whist--on-delete-window)

;; TODO:  make sure that everything is deleted properly, nothing dangling remains (!)


(provide 'whist)
