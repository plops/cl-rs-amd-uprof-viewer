(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator")
  (ql:quickload "cl-ppcre"))

(in-package :cl-rust-generator)

(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))

(progn
  (defparameter *source-dir* #P"/home/martin/stage/cl-rs-amd-uprof-viewer/code_zenpower/")
  (defparameter *code-file*
    (merge-pathnames #P"src/main.rs"
		     *source-dir*))
  (defun logprint (msg &optional (rest nil))
    `(progn
       (println! (string ,(format nil "{} {}:{} ~a ~{~a~^ ~}"
				  msg
				  (loop for e in rest collect
				       (format nil " ~a={}" (emit-rs :code e)))))
		 #+nil (dot (SystemTime--now)
		      (duration_since UNIX_EPOCH)
		      (expect (string "time went backwards"))
		      (as_micros))
		 (Utc--now)
		 (file!)
		 (line!)
		 ,@(loop for e in rest collect
			e ;`(dot ,e (display))
			))))


  (with-open-file (s (merge-pathnames #P"Cargo.toml"
				      *source-dir*)
		     
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (format s "~a"
	    "[package]
name = \"rs03_glium\"
version = \"0.1.0\"
authors = [\"Martin Kielhorn <kielhorn.martin@gmail.com>\"]
edition = \"2018\"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
glium = \"*\"
imgui = \"*\"
imgui-glium-renderer = \"*\"
imgui-winit-support = \"*\"
chrono = \"*\"
crossbeam-channel = \"*\"
positioned-io = \"*\"
"
	    


	    ))

  
  (let ((code
	 `(do0
	   (do0

	    (use (std io))
	    
	    (use
	     (chrono (curly DateTime Utc)))
	    
	    "use imgui::*;"
	    ,@(loop for e in `("glutin"
			       "glutin::event::{Event,WindowEvent}"
			       "glutin::event_loop::{ControlFlow,EventLoop}"
			       "glutin::window::WindowBuilder"
			       "{Display,Surface}")
		 collect
		   `(use (glium ,e))))
	   (use (imgui (curly Context FontConfig FontGlyphRanges FontSource Ui))
		(imgui_glium_renderer Renderer)
		(imgui_winit_support (curly HiDpiMode WinitPlatform)))
	   (use (std (curly thread time fs))
		(std fs File)
		(std time Instant)
		(positioned_io ReadAt)
		)
	  
	   

	   
	   
	   (defstruct0 System
	       (event_loop "EventLoop<()>")
	     (display "glium::Display")
	     (imgui Context)
	     (platform WinitPlatform)
	     (renderer Renderer)
	     (font_size f32))


	   (defun init ("title: &str")
	     (declare (values System))
	     (let ((title (case (title.rfind (string "/"))
			    ((Some idx) (dot title (split_at (+ idx 1)) 1))
			    (None title)))
		   (event_loop ("EventLoop::new"))
		   (context (dot ("glutin::ContextBuilder::new")
				 (with_vsync true)))
		   (builder (dot ("WindowBuilder::new")
				 (with_title (title.to_owned))
				 (with_inner_size
				  ("glutin::dpi::LogicalSize::new"
				   "512f64"
				   "512f64"))))
		   (display (dot ("Display::new"
				  builder
				  context
				  &event_loop)
				 (expect (string "failed to initialize display"))))
		   (imgui ("Context::create")))
	       (declare (mutable imgui))
	       (imgui.set_ini_filename None)
	       (let* ((platform ("WinitPlatform::init"
				 "&mut imgui")))
		 (progn
		   (let ((gl_window (display.gl_window))
			 (window (gl_window.window)))
		     (platform.attach_window (imgui.io_mut)
					     &window
					     "HiDpiMode::Rounded"))))
	      #+Nil(let ((hidpi_facotr (platform.hidpi_factor))
		     (font_size (* 13s0 hidpi_factor)))
		 (declare (type f32 font_size))
		 (dot imgui
		      (fonts)
		      (add_font
		       (ref (list
			     (make-instance "FontSource::DefaultFontData"
					    :config (Some
						     (make-instance
						      FontConfig
						      :size_pixels font_size
						      "" ("..FontConfig::default"))))
			     (make-instance "FontSource::TtfData"
					    ))))))
	      (let ((renderer (dot ("Renderer::init"
				    "&mut imgui"
				    &display)
				   (expect (string "failed to initialize renderer")))))
		(return (make-instance System
				       event_loop
				       display
				       imgui
				       platform
				       renderer
				       :font_size 12s0)))))


	   (impl System
		 (defun "main_loop<F: FnMut(&mut bool,&mut Ui)+'static>"
		     (self "mut run_ui: F")
		   (let (((make-instance System
					 event_loop
					 display
					 "mut imgui"
					 "mut platform"
					 "mut renderer"
					 "..") self)
			 (last_frame ("Instant::now")))
		     (declare (mutable last_frame))
		     (dot event_loop
			  (run
			   (space
			    move
			    (lambda (event _ control_flow)
			      (case event
				(("Event::NewEvents" _)
				 (setf last_frame (dot imgui
						       (io_mut)
						       (update_delta_time last_frame)))
				 )
				("Event::MainEventsCleared"
				 (let ((gl_window (display.gl_window)))
				   (dot platform
					(prepare_frame (imgui.io_mut)
						       (ref (gl_window.window)))
					(expect (string "failed to prepare frame")))
				   (dot gl_window
					(window)
					(request_redraw))))
				(("Event::RedrawRequested" _)
				 (let* ((ui (imgui.frame))
					(run true))
				   (run_ui "&mut run"
					   "&mut ui")
				   (unless run
				     (setf *control_flow
					   "ControlFlow::Exit"))
				   (let ((gl_window (display.gl_window)))
				     (let* ((target (display.draw)))
				       (target.clear_color_srgb 1s0 1s0 1s0 1s0)
				       (platform.prepare_render &ui
								(gl_window.window))
				       (let ((draw_data (ui.render)))
					 (dot renderer
					      (render "&mut target"
						      draw_data)
					      (expect (string "rendering failed")))
					 (dot target
					      (finish)
					      (expect (string "swap buffer failed"))))))))
				((make-instance "Event::WindowEvent"
						:event "WindowEvent::CloseRequested"
						"..")
				 (setf *control_flow "ControlFlow::Exit"))
				(event
				 (let ((gl_window (display.gl_window)))
				   (platform.handle_event (imgui.io_mut)
							  (gl_window.window)
							  &event)))))))))))
	   (defun parse ("data: &[u8]")
	     (declare (values u64))
	     (return 
	      (dot data
		   (iter)
		   (fold 0
			 (lambda (a b)
			   ;,(logprint "parse" `(a b (- b (byte 0))))
			   (return (+ (* 10 a)
				      (coerce (- b (byte 0))
					      u64))))))))

	   (defun read_int ("data: &[u8]")
	     (declare (values ; "Result<Vec<u64>, MyError>"
		       "io::Result<u64>"
		       ))
	     ;; https://users.rust-lang.org/t/reading-integers-from-a-file-into-vector/17517/3
	     (let* ((res 0))
	      (do0			;let* ((j 0))
	       (for ((values i byte) (dot data
					  (iter)
					  (enumerate)))
		    ;,(logprint "parse" `(i byte))
		    (case byte
		      ((byte "\\n")
		       ;,(logprint "parse newline" `(i byte))
		       (setf res (parse (ref (aref data "0..i"))))
		       (return (Ok res))
		        ;; j..i
					;(setf j (+ i 1))
		       )
		      ("b'0'..=b'9'"
		       "()")
					;(_ (return (? (Err MyError))))
		      (0
		       ,(logprint "parse zero" `(i byte))
		       (setf res (parse (ref (aref data "0..i")))))
		      (_
		       ,(logprint "parse error" `(i byte))
		       (? (Err 
					     (io--Error--new
					      io--ErrorKind--Other
					      (string "int reader fail")))))))
	       (return (Ok res))))
	     )
	   (defun main ()
	     
	     ,(let ((files (let* ((dir #P"/sys/class/hwmon/hwmon0/")
				  (paths (directory (merge-pathnames "*input" dir)))
				  )
			     (loop for path in paths collect
				  (let* ((prefix (elt (cl-ppcre:split "_"
								      (pathname-name path))
						      0))
					 (label-file (merge-pathnames (format nil "~a_label" prefix)
								      dir)))
				    (list (with-open-file (s label-file)
					    (read s))
					  path))))
		      ;(directory #P"/sys/class/hwmon/hwmon0/*input")
		      )
		    )
		`(do0
		  #+nil
		  (loop
		     ,@(loop for f in files collect
			  `(progn
			     (let ((contents (dot (fs--read_to_string (string ,f))
						  (expect (string "read error")))))
			       ,(logprint f `(contents))))))

		  (let (,@(loop for (name f) in files and i from 0 collect
			       `(,(format nil "f_~a" name) (dot (File--open (string ,f))
							    (unwrap)))))
		    (loop
		       (let* (,@(loop for (name f) in files and i from 0 collect
				     `(,(format nil "buf_~a" name) "[0; 32]")))
			 #+nil (declare (type (array u8 32) ,@(loop for f in files and i from 0 collect
							     (format nil "buf~a" i))))
			 (let (,@(loop for (name f) in files and i from 0 collect
				      `(,(format nil "_bytes_~a" name) (dot ,(format nil "f_~a" name)
									(read_at 0 ,(format nil "&mut buf_~a" name))
									(expect (string ,(format nil "read_at ~a fail" name)))))))
			   ;,(logprint "read" (loop for f in files and i from 0 collect (format nil "_bytes~a" i)))
			   (let (,@(loop for (name f) in files and i from 0 collect
					`(,(format nil "v_~a" name) (dot (read_int ,(format nil "&mut buf_~a" name))
								     (expect (string ,(format nil "read_int ~a error" name)))))))
			   
			    ,(logprint "" (loop for (name f) in files and i from 0 collect
					       (format nil "v_~a" name))))))))
		  
		  ,@(loop for f in files collect
			  `(progn
			     (let ((contents (dot (fs--read_to_string (string ,f))
						  (expect (string "read error")))))
			       ,(logprint f `(contents)))))
		  ))
	     
	     #+nil (let* ((client (request--Client--new))
		    (body (dot client
			       (get (string "https://query1.finance.yahoo.com/v7/finance/quote?lang=en-US&region=US&corsDomain=finance.yahoo.com&symbols=DBX,LITE,AMD,INTC&fields=regularMarketPrice"))
			       (? await)
			       (text)
			       (? await))))
	       
	       ,(logprint "stock" `(body)))
	     (let ((system (init (file!))))
	       (system.main_loop
		(space move
		       (lambda (_ ui)
			 (dot ("Window::new" (im_str! (string "Hello world")))
			      (size (list 300.0 100.0)
				    "Condition::FirstUseEver")
			      (build ui
				     (lambda ()
				       (ui.text (im_str! (string "Hello World")))
				       (let ((mouse_pos (dot ui
							     (io)
							     mouse_pos)))
					 (ui.text (format!
						   (string "mouse: ({:.1},{:.1})"
							   )
						   (aref mouse_pos 0)
						   (aref mouse_pos 1)))))))
			 (dot ("Window::new" (im_str! (string "recv")))
			      (size (list 200.0 100.0)
				    "Condition::FirstUseEver")
			      (build ui
				     (lambda ()
				       (ui.text (im_str! (string "recv")))
				       )))))))))))

    
    
    (write-source *code-file*
		  code)))



