;;; whist-test.el  tests for whist.el         -*- lexical-binding: t -*-
;;
;; Filename: whist-test.el
;;

(eval-and-compile
  (require 'ert nil t)
  (require 'ert "lib/ert"))

(require 'whist)

(ert-deftest whist--dll-test ()
  (let* ((whist (whist--dll-create)))
	;; '() -----------------------------------------------------------

	(should (equal 1
				   (whist--node-point (whist--dll-front-node whist) )))
	(should (equal 0
				   (whist--node-point (whist--dll-rear-node whist) )))
	(should (equal 0
				   (whist--dll-size whist)))
	(should (equal whist--null-node
				   (whist--dll-cursor whist)))

	(should (equal whist--null-node
				   (whist--dll-get whist 1)))
	(should (equal whist--null-node
				   (whist--dll-get whist -1)))

	;; '(1) ----------------------------------------------------------
	(whist--dll-push! whist (whist--node-create "*scratch*" :point 1))

	(should (equal 1
				   (whist--node-point (whist--dll-front-node whist) )))
	(should (equal 1
				   (whist--node-point (whist--dll-rear-node whist) )))
	(should (equal 1
				   (whist--dll-size whist)))
	(should (equal whist--null-node
				   (whist--dll-cursor whist)))

	(should (equal whist--null-node
				   (whist--dll-get whist 1)))
	(should (equal 1
				   (whist--node-point (whist--dll-get whist -1))))

	;; '(1 2 3) ---------------------------------------------------------
	(whist--dll-push! whist (whist--node-create "*scratch*" :point 2))
	(whist--dll-push! whist (whist--node-create "*scratch*" :point 3))

	(should (equal 3
				   (whist--node-point (whist--dll-front-node whist) )))
	(should (equal 1
				   (whist--node-point (whist--dll-rear-node whist) )))
	(should (equal 3
				   (whist--dll-size whist)))
	(should (equal whist--null-node
				   (whist--dll-cursor whist)))

	(should (equal whist--null-node
				   (whist--dll-get whist 1)))
	(should (equal 3
				   (whist--node-point (whist--dll-get whist -1))))

	;; --
	(should (equal 2
				   (whist--node-point (whist--node-next (whist--dll-rear-node whist)))))
	(should (equal 2
				   (whist--node-point (whist--node-prev (whist--dll-front-node whist)))))

	;; '(1 2 c3) --------------------------------------------------------
	(setf (whist--dll-cursor whist) (whist--dll-front-node whist))

	(should (equal 3
				   (whist--node-point (whist--dll-cursor whist) )))
	(should (equal 2
				   (whist--node-point (whist--dll-get whist -1))))
	(should (equal 3
				   (whist--node-point (whist--dll-get whist 1))))

	;; '(c1 2 3) --------------------------------------------------------
	(setf (whist--dll-cursor whist) (whist--dll-rear-node whist))

	(should (equal 1
				   (whist--node-point (whist--dll-cursor whist) )))
	(should (equal 1
				   (whist--node-point (whist--dll-get whist -1))))
	(should (equal 2
				   (whist--node-point (whist--dll-get whist 1))))
	(should (equal 3
				   (whist--dll-size whist)))
	(should (equal 3
				   (whist--node-chain-count-forward (whist--dll-cursor whist))))

	;; '(1 c2)
	(setf (whist--dll-cursor whist) (whist--dll-get whist 1))
	(whist--dll-drop-future! whist)

	(should (equal 2
				   (whist--node-point (whist--dll-cursor whist) )))
	(should (equal 2
				   (whist--node-point (whist--dll-front-node whist) )))
	(should (equal 1
				   (whist--node-point (whist--dll-rear-node whist) )))
	(should (equal 2
				   (whist--dll-size whist)))
	(should (equal 1
				   (whist--node-chain-count-forward (whist--dll-cursor whist))))

	;; '(c2)
	(whist--dll-drop-first! whist)

	(should (equal 2
				   (whist--node-point (whist--dll-cursor whist) )))
	(should (equal 2
				   (whist--node-point (whist--dll-front-node whist) )))
	(should (equal 2
				   (whist--node-point (whist--dll-rear-node whist) )))
	(should (equal 1
				   (whist--dll-size whist)))
	(should (equal 1
				   (whist--node-chain-count-forward (whist--dll-cursor whist))))

	;; TODO: make some node invalid, check that next and prev jumps over it (maybe even removes it?)
	;; TODO: whist--dll-get can get stuck in an endless-loop via whist--dll-find-next-valid. fix.
	)

  ;; ;; --- invalid nodes
  ;; (let* ((h (whist--create)))
  ;; 	(whist--dll-push (whist--node-create "*scratch*" :point 1))
  ;; 	(whist--dll-push (whist--node-create_ :buffer "blabla" :point 2)) ; invalid
  ;; 	(whist--dll-push (whist--node-create "*scratch*" :point 3))
  ;; 	(should (equal ))
  ;; 	)

  )

(ert-deftest whist-dll-invalid-nodes-test2 ()

  (get-buffer-create "buf1")
  (get-buffer-create "buf2")
  (get-buffer-create "buf3")
  (get-buffer-create "buf4")

  (switch-to-buffer "buf1" nil t)
  (whist--delete (selected-window))

  (let* ((whist (whist--get :create t) )
		 (win (selected-window)))
	(whist--dll-push! whist (whist--node-create "buf1" :point 1))
	(whist--dll-push! whist (whist--node-create "buf2" :point 2))
	(whist--dll-push! whist (whist--node-create "buf3" :point 3))

	(kill-buffer "buf2")

	;; ;; '(1 i2 3)
	(should (equal 3
				   (whist--node-point (whist--dll-front-node whist))))
	(should (equal 1
				   (whist--node-point (whist--dll-rear-node whist))))
	(should (equal 3
				   (whist--node-point (whist--dll-get whist -1))))
	(should (equal 3
				   (whist--dll-size whist)))

	(setf (whist--dll-cursor whist) (whist--dll-get whist -1))
	;; '(1 i2 c3)
	(should (equal 3
				   (whist--node-point (whist--dll-cursor whist))))
	(should (equal 1
				   (whist--node-point
					(whist--dll-find-next-valid #'whist--node-prev (whist--dll-cursor whist) ))))

	(setf (whist--dll-cursor whist) (whist--node-prev (whist--dll-cursor whist)))
	;; '(1 ic2 3)
	(should (equal 3
				   (whist--node-point (whist--dll-get-cursor whist t))))
	(should (equal 1
				   (whist--node-point (whist--dll-get-cursor whist nil))))

	(setf (whist--dll-cursor whist) (whist--dll-rear-node whist))
	;; '(c1 i2 3)
	(should (equal 1
				   (whist--node-point (whist--dll-cursor whist))))
	(should (equal 3
				   (whist--node-point
					(whist--dll-find-next-valid #'whist--node-next (whist--dll-cursor whist) ))))

	(kill-buffer "buf1")
	;; '(ic1 i2 3)
	(should (equal 3
				   (whist--node-point (whist--dll-get-cursor whist))))
	)
  )

(ert-deftest whist-public-api-invalidity-test ()

  (let ((mintime (+ 0.05 whist--nav-time)))

	(setq whist--nav-timer-started nil)

	(get-buffer-create "buf1")
	(get-buffer-create "buf2")
	(get-buffer-create "buf3")
	(get-buffer-create "buf4")
	(get-buffer-create "buf5")

	(switch-to-buffer "buf1" nil t)
	(sleep-for mintime)
	;; (whist--delete (selected-window))

	(let* ((whist (whist--get :create t) )
		   (win (selected-window))
		   (mintime (+ 0.1 whist--nav-time)))

	  (switch-to-buffer "buf2")
	  (sleep-for mintime)
	  (switch-to-buffer "buf3")
	  (sleep-for mintime)
	  (switch-to-buffer "buf4")
	  (sleep-for mintime)
	  ;; '(1 2 3)
	  (should (equal whist--null-node (whist--dll-cursor whist)))
	  (should (equal 3
	  				 (whist--dll-size whist)))

	  (whist-go-back)
	  ;; ;; '(1 2 c3 4)
	  (should (equal "buf3"
	  				 (whist--node-buffer-name (whist--dll-cursor whist))))
	  (should (equal 4
	  				 (whist--dll-size whist)))

	  (kill-buffer "buf3")
	  ;; '(1 2 ci3 4)
	  (whist-go-back)
	  ;; ;; '(1 c2 i3 4)
	  (should (equal "buf2"
	  				 (whist--node-buffer-name (whist--dll-cursor whist))))
	  (should (equal 4
	  				 (whist--dll-size whist)))

	  (whist-go-forward)
	  ;; '(1 2 i3 c4)
	  (should (equal "buf4"
	  				 (whist--node-buffer-name (whist--dll-cursor whist))))
	  (should (equal 4
	  				 (whist--dll-size whist)))

	  (whist-go-back)
	  ;; '(1 c2 i3 4)
	  (should (equal "buf2"
	  				 (whist--node-buffer-name (whist--dll-cursor whist))))
	  (should (equal 1
					 (whist--node-point (whist--dll-cursor whist))))

	  (switch-to-buffer "buf5")
	  (sleep-for mintime)
	  ;; '(1 2)
	  (should (equal whist--null-node (whist--dll-cursor whist)))

	  ;; (setq w whist)

	  (whist-go-back)
	  ;; '(1 c2 5)
	  (should (equal t (bufferp (get-buffer "buf2"))))
	  (should (equal t (buffer-live-p (get-buffer "buf2"))))
	  (should (equal "buf2"
	  				 (whist--node-buffer-name (whist--dll-cursor whist))))

	  (kill-buffer "buf2")
	  ;; '(1 ic2 5)
	  (whist-go-forward)
	  ;; '(1 i2 c5)
	  (should (equal "buf5"
					 (whist--node-buffer-name (whist--dll-cursor whist))))
	  )
	)
  )

(ert-deftest whist-public-api-test ()
  (let ((mintime (+ 0.05 whist--nav-time)))

	(message "----whist-public-api-test ----------------------------------------------------")

	(get-buffer-create "buf1")
	(get-buffer-create "buf2")
	(get-buffer-create "buf3")
	(get-buffer-create "buf4")

	(switch-to-buffer "buf1" nil t)
	(whist--delete (selected-window))

	(let* ((whist (whist--get :create t) )
		   (win (selected-window)))

	  (switch-to-buffer "buf2")
	  (sleep-for mintime)
	  ;; '(1)
	  (should (equal 1
					 (whist--dll-size whist)))

	  (pop-to-buffer "buf3")
	  (sleep-for mintime)
	  ;; '(1 2)
	  (should (equal 2
	  				 (whist--dll-size whist)))

	  (whist-go-back)
	  ;; '(1 c2 3)
	  (should (equal 3
	  				 (whist--dll-size whist)))
	  (should (equal "buf2"
	  				 (whist--node-buffer-name (whist--dll-cursor whist))))

	  (whist-go-back)
	  (whist-go-back)
	  (whist-go-back)
	  ;; '(c1 2 3)
	  (should (string= "buf1"
	  				   (whist--node-buffer-name (whist--dll-cursor whist))))

	  (whist-go-forward)
	  ;; '(1 2 c3)
	  (should (string= "buf2"
	  				   (whist--node-buffer-name (whist--dll-cursor whist))))

	  (whist-go-forward)
	  (whist-go-forward)
	  ;; '(1 2 c3)
	  (should (string= "buf3"
	  				   (whist--node-buffer-name (whist--dll-cursor whist))))

	  (whist-go-back)
	  (switch-to-buffer "buf4")
	  (sleep-for mintime)
	  ;; '(1 2)
	  (should (equal whist--null-node
	  				 (whist--dll-cursor whist)))

	  (pop-to-buffer "buf3")
	  (sleep-for mintime)
	  ;; '(1 2 4)
	  (should (string= "buf4"
	  				   (whist--node-buffer-name (whist--dll-front-node whist))))

	  )
	)
  )

(ert-deftest whist-window-deletion-test ()
  (message "----whist-window-deletion-test")
  (let ((mintime (+ 0.1 whist--nav-time)))

	(get-buffer-create "buf1")
	(get-buffer-create "buf2")

	;; ---------------------------------
	(switch-to-buffer "buf1" nil t)
	(sleep-for mintime)
	(whist--delete (selected-window))

	(let* ((whist (whist--get :create t) )
		   (win (selected-window)))

	  ;; '(1)
	  (switch-to-buffer "buf2")
	  (sleep-for mintime)
	  (should (equal 1
					 (whist--dll-size whist)))

	  (delete-window win)
	  ;; ;; nil
	  (should (equal nil
					 (whist--get :window win :create nil)))

	  (whist--delete win)
	  )

	;;  -------------------------------
	(switch-to-buffer "buf1" nil t)
	(sleep-for mintime)
	(whist--delete (selected-window))

	(let* ((whist (whist--get :create t) )
		   (win (selected-window)))

	  ;; '(1)
	  (switch-to-buffer "buf2")
	  (sleep-for mintime)
	  (should (equal 1
					 (whist--dll-size whist)))

	  (kill-buffer-and-window)
	  ;; ;; nil
	  (should (equal nil
					 (whist--get :window win :create nil)))
	  )
	)
  )


;; (ert "whist--dll-test")
;; (ert "whist-dll-invalid-nodes-test2")
;; (ert "whist-public-api-test")
;; (ert "whist-public-api-invalidity-test")
;; (ert "whist-window-deletion-test")

(ert "whist")

