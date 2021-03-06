(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-cpp-generator2")
  (ql:quickload "cl-ppcre"))

(in-package :cl-cpp-generator2)

(progn
  ;; make sure to run this code twice during the first time, so that
  ;; the functions are defined

  (defparameter *source-dir* ; #P"/home/martin/stage/cl-rs-amd-uprof-viewer/c_api/source/"
    #P"../cl-rs-amd-uprof-viewer/c_api/source/"
    )
  
  (progn
    ;; collect code that will be emitted in utils.h
    (defparameter *utils-code* nil)
    (defun emit-utils (&key code)
      (push code *utils-code*)
      " ")
    (defparameter *global-code* nil)
    (defun emit-global (&key code)
      (push code *global-code*)
      " "))
  (progn
  
    (defparameter *module-global-parameters* nil)
    (defparameter *module* nil)
    (defun logprint (msg &optional rest)
      `(do0
	" "
	#-nolog
	(do0
	 ;("std::setprecision" 3)
	 (<< "std::cout"
	     ;;"std::endl"
	     ("std::setw" 10)
	     (dot ("std::chrono::high_resolution_clock::now")
		  (time_since_epoch)
		  (count))
					;,(g `_start_time)
	     
	     (string " ")
	     ("std::this_thread::get_id")
	     (string " ")
	     __FILE__
	     (string ":")
	     __LINE__
	     (string " ")
	     __func__
	     (string " ")
	     (string ,msg)
	     (string " ")
	     ,@(loop for e in rest appending
		    `(("std::setw" 8)
					;("std::width" 8)
		      (string ,(format nil " ~a='" (emit-c :code e)))
		      ,e
		      (string "'")))
	     "std::endl"
	     "std::flush"))))
    (defun guard (code &key (debug t))
		  `(do0
		    #+lock-debug ,(if debug
		       (logprint (format nil "hold guard on ~a" (cl-cpp-generator2::emit-c :code code))
				 `())
		       "// no debug")
		    #+eou ,(if debug
		     `(if (dot ,code ("std::mutex::try_lock"))
			 (do0
			  (dot ,code (unlock)))
			 (do0
			  ,(logprint (format nil "have to wait on ~a" (cl-cpp-generator2::emit-c :code code))
				     `())))
		     "// no debug")
		    "// no debug"
		   ,(format nil
			    "std::lock_guard<std::mutex> guard(~a);"
			    (cl-cpp-generator2::emit-c :code code))))
    (defun lock (code &key (debug t))
      `(do0
	#+lock-debug ,(if debug
	     (logprint (format nil "hold lock on ~a" (cl-cpp-generator2::emit-c :code code))
		       `())
	     "// no debug")

	#+nil (if (dot ,code ("std::mutex::try_lock"))
	    (do0
	     (dot ,code (unlock)))
	    (do0
	     ,(logprint (format nil "have to wait on ~a" (cl-cpp-generator2::emit-c :code code))
			`())))
	
		    ,(format nil
			     "std::unique_lock<std::mutex> lk(~a);"
			     
				(cl-cpp-generator2::emit-c :code code))
		    ))

    
    (defun emit-globals (&key init)
      (let ((l `((_start_time ,(emit-c :code `(typeof (dot ("std::chrono::high_resolution_clock::now")
							   (time_since_epoch)
							   (count)))))
		 ,@(loop for e in *module-global-parameters* collect
			(destructuring-bind (&key name type default)
			    e
			  `(,name ,type))))))
	(if init
	    `(curly
	      ,@(remove-if
		 #'null
		 (loop for e in l collect
		      (destructuring-bind (name type &optional value) e
			(when value
			  `(= ,(format nil ".~a" (elt (cl-ppcre:split "\\[" (format nil "~a" name)) 0)) ,value))))))
	    `(do0
	      (include <chrono>)
	      (defstruct0 State
		  ,@(loop for e in l collect
 			 (destructuring-bind (name type &optional value) e
			   `(,name ,type))))))))
    (defun define-module (args)
      "each module will be written into a c file with module-name. the global-parameters the module will write to will be specified with their type in global-parameters. a file global.h will be written that contains the parameters that were defined in all modules. global parameters that are accessed read-only or have already been specified in another module need not occur in this list (but can). the prototypes of functions that are specified in a module are collected in functions.h. i think i can (ab)use gcc's warnings -Wmissing-declarations to generate this header. i split the code this way to reduce the amount of code that needs to be recompiled during iterative/interactive development. if the module-name contains vulkan, include vulkan headers. if it contains glfw, include glfw headers."
      (destructuring-bind (module-name global-parameters module-code) args
	(let ((header ()))
	  (push `(do0
		  " "
		  (include "utils.h")
		  " "
		  (include "globals.h")
		  " "
		  ;(include "proto2.h")
		  " ")
		header)
	  (unless (cl-ppcre:scan "main" (string-downcase (format nil "~a" module-name)))
	    (push `(do0 "extern State state;")
		  header))
	  (push `(:name ,module-name :code (do0 ,@(reverse header) ,module-code))
		*module*))
	(loop for par in global-parameters do
	     (destructuring-bind (parameter-name
				  &key (direction 'in)
				  (type 'int)
				  (default nil)) par
	       (push `(:name ,parameter-name :type ,type :default ,default)
		     *module-global-parameters*))))))
  (defun g (arg)
    `(dot state ,arg))
  
  (define-module
      `(lib ()
	     (do0
	      (include <AMDTPowerProfileApi.h>
		       <chrono>
		       <cstdio>
		       <cassert>
					;<unordered_map>
		       <string>
		       <fstream>)

	      "using namespace std::chrono_literals;"
	      #+nil (let ((state ,(emit-globals :init t)))
		(declare (type "State" state)))

	      (space "extern \"C\" "
		     
		     (progn
		       (defstruct0 samples_pair_t
			 (result int)
		       (handle void*))
		       ,@(loop for e in `((int ProfileInitialize_online (;(mode int)
								  )
					       (do0
						(let ((mode_ AMDT_PWR_MODE_TIMELINE_ONLINE)
						      (res (AMDTPwrProfileInitialize mode_)))
						   ,(logprint (format nil "~a" 'init) `(res))
						 (return res))))
					  (int EnableCounter ((counter int)))
					  (int SetTimerSamplingPeriod ((interval_ms int)))
					  (int StartProfiling ())
					  (int StopProfiling ())
					  (int ProfileClose ())
					  (samples_pair_t ReadAllEnabledCounters ()
					       (do0
						(let ((n 0)
						      (samples nullptr)
						      (pair (curly -1 nullptr))
						      (res (AMDTPwrReadAllEnabledCounters &n &samples)))
						  (declare (type AMDTUInt32 n)
							   (type AMDTPwrSample* samples)
							   (type samples_pair_t pair))
						  (do0
						   (unless (== AMDT_STATUS_OK res)
						     (case res
						       ,@(loop for e in `(AMDT_STATUS_OK
									  AMDT_ERROR_INVALIDARG
									  AMDT_ERROR_DRIVER_UNINITIALIZED
									  AMDT_ERROR_PROFILE_NOT_STARTED
									  AMDT_ERROR_PROFILE_DATA_NOT_AVAILABLE
									  AMDT_ERROR_OUTOFMEMORY
									  AMDT_ERROR_SMU_ACCESS_FAILED
									  AMDT_ERROR_FAIL) collect
							      `(,e (progn
								     ,(logprint e `())
								     break)
								   ))
						       )
						     (return pair)))
						  (setf pair.result n
							pair.handle (reinterpret_cast<void*> samples))
						  ,(logprint "" `(pair.result pair.handle))
						  (return pair))))
					  

					  ,@(loop for var in `((elapsedTimeMs int -1) ;; fixme this is uint64
							       (recordId uint 1000000)
							       (numOfCounter int -1))
					       collect
						 (destructuring-bind (name type &optional err-value) var
						   (declare (ignorable err-value))
						   `(,type ,(format nil "ReadAllEnabledCounters_PwrSample_~a" name) ((handle void*)
												 (idx int))
							   (do0
							    
							    (let ((samples (reinterpret_cast<AMDTPwrSample*> handle))
								  (res (dot (aref samples idx)
									   ,(format nil "m_~a" name))))
							      
							      ,(logprint "" `(idx res))
							     
							      (return res))))))

					  ,@(loop for var in `((counterID int -1)
							       (valueCnt int -1)
							       )
					       collect
						 (destructuring-bind (name type &optional err-value) var
						   (declare (ignorable err-value))
						   `(,type ,(format nil "ReadAllEnabledCounters_counterValues_~a" name) ((handle void*)
												 (idx int) (counter_idx int))
							   (do0
							    
							    (let ((samples (reinterpret_cast<AMDTPwrSample*> handle)))

							      (when (== nullptr
									  (dot (aref samples idx)
									       m_counterValues))
								(return ,err-value))
							      (unless (< counter_idx (dot (aref samples idx)
											  m_numOfCounter))
								(return ,err-value))
							      
							      (return (dot (aref samples idx)
									   (aref m_counterValues counter_idx)
									   ,(format nil "m_~a" name))))))))
					  ,@(loop for var in `((data float NAN))
					       collect
						 (destructuring-bind (name type &optional err-value) var
						   (declare (ignorable err-value))
						   `(,type ,(format nil "ReadAllEnabledCounters_counterValues_~a" name) ((handle void*)
												 (idx int) (counter_idx int) )
							   (do0
							    
							    (let ((samples (reinterpret_cast<AMDTPwrSample*> handle)))

							      (when (== nullptr
									(dot (aref samples idx)
									     m_counterValues))
								,(logprint "fail nullptr" `((dot (aref samples idx)
									     m_counterValues)))

								(return ,err-value))
							      (unless (< counter_idx (dot (aref samples idx)
											  m_numOfCounter))
								,(logprint "out of bounds" `(counter_idx (dot (aref samples idx)
											  m_numOfCounter)))

								(return ,err-value))
							      
							      
							      (return (dot (aref samples idx)
									   (aref m_counterValues counter_idx)
									  ,(format nil "m_~a" name))))))))
					  
					  (int GetSupportedCounters_num ()
					       (do0
						(let ((n 0)
						      (desc nullptr)
						      (res (AMDTPwrGetSupportedCounters &n &desc)))
						  (declare (type AMDTUInt32 n)
							   (type AMDTPwrCounterDesc* desc))
						  (do0
						   (unless (== AMDT_STATUS_OK res)
						     ,(logprint "fail" `(res))
						     (return -1)))
						  ,(logprint "" `(n desc))
						  (let ((p desc))
						    (dotimes (i n)
						      (unless (== nullptr p)
							,(let ((l `((counterID int -1)
								    (deviceId int -1)
								    (devType int -1)
								    (devInstanceId int -1)
							     (name char* nullptr)
							     (description char* nullptr)
							     (category int -1)
							     (aggregation int -1)
							     (minValue double NAN)
							     (maxValue double NAN)
							     (units int -1)
								    (isParentCounter int -1))))
							   `(do0
							     (let (,@(loop for e in l collect
									  (destructuring-bind (name &rest rest) e
									    (declare (ignorable rest))
									    `(,name (-> p ,(format nil "m_~a" name)))))

								   )
							       ,(logprint "" `(,@(loop for e in l collect
										      (destructuring-bind (name &rest rest) e
											 (declare (ignorable rest))
											name))))))))
						     (incf p)))
						  (return n))))
					  (int EnableAllCounters ()
					       (do0
						(let ((n 0)
						      (desc nullptr)
						      (res (AMDTPwrGetSupportedCounters &n &desc)))
						  (declare (type AMDTUInt32 n)
							   (type AMDTPwrCounterDesc* desc))
						  (do0
						   (unless (== AMDT_STATUS_OK res)
						     ,(logprint "fail" `(res))
						     (return -1)))
						  ,(logprint "" `(n desc))
						  (let ((p desc))
						    (dotimes (i n)
						      (unless (== nullptr p)
							(let ((res (AMDTPwrEnableCounter p->m_counterID)))
							  (unless (== AMDT_STATUS_OK res)
							    ,(logprint "fail enable" `(res i p->m_counterID n desc))))
							)
						     (incf p)))
						  (return n))))
					  ,@(loop for var in `((counterID int -1)
							     (deviceId int -1)
							     (devType int -1)
							     (devInstanceId int -1)
							     (name char* nullptr)
							     (description char* nullptr)
							     (category int -1)
							     (aggregation int -1)
							     (minValue double NAN)
							     (maxValue double NAN)
							     (units int -1)
							     (isParentCounter int -1))
					       collect
						 (destructuring-bind (name type &optional err-value) var
						   (declare (ignorable err-value))
						   `(,type ,(format nil "GetCounterDesc_~a" name) ((idx int))
							   (do0
							    
							    (let ((n 0)
								  (desc nullptr)
								  (res (AMDTPwrGetSupportedCounters &n &desc)))
							      (declare (type AMDTUInt32 n)
								       (type AMDTPwrCounterDesc* desc))
							      (unless (== AMDT_STATUS_OK res)
								,(logprint "fail" `(res))
								(return ,err-value))
							      (unless (< idx n)
								,(logprint "out of bounds" `(idx n))
								(return ,err-value))
							      (return (dot (aref desc idx)
									  ,(format nil "m_~a" name)))))))))
			    collect
			      (destructuring-bind (ret-type base params &optional code) e
			       (let ((name (format nil "AMDTPwr~a" base)))
				 `(defun ,base ,(loop for (name type) in params collect name)
				    (declare ,@(loop for (name type) in params collect
						    `(type ,type ,name))
					     (values ,ret-type))
				    
				    ,(if code
					 `(do0
					  ,(logprint (format nil "~a" name) (loop for (name type) in params collect name))
					  ,code)
					 `(let ((res (,name ,@(loop for (name type) in params collect name))))
					    ,(logprint (format nil "~a" name) `(res ,@(loop for (name type) in params collect name)))
					    (return res)))))))))
	      
	      #+nil(defun main ()
		(declare (values int))
		))))

    (progn
    (with-open-file (s (asdf:system-relative-pathname 'cl-cpp-generator2
						      (merge-pathnames #P"proto2.h"
								       *source-dir*))
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (loop for e in (reverse *module*) and i from 0 do
	   (destructuring-bind (&key name code) e
	     (let ((cuda (cl-ppcre:scan "cuda" (string-downcase (format nil "~a" name)))))
	       (unless cuda
		 (emit-c :code code :hook-defun 
			 #'(lambda (str)
			     (format s "~a~%" str))))
	       
	       (write-source (asdf:system-relative-pathname
			      'cl-cpp-generator2
			      (format nil
				      "~a/vis_~2,'0d_~a.~a"
				      *source-dir* i name
				      (if cuda
					  "cu"
					  "cpp")))
			     code)))))
    (write-source (asdf:system-relative-pathname
		   'cl-cpp-generator2
		   (merge-pathnames #P"utils.h"
				    *source-dir*))
		  `(do0
		    "#ifndef UTILS_H"
		    " "
		    "#define UTILS_H"
		    " "
		    (include <vector>
			     <array>
			     <iostream>
			     <iomanip>)
		    
		    " "
		    (do0
		     
		     " "
		     ,@(loop for e in (reverse *utils-code*) collect
			  e)
					;"#define length(a) (sizeof((a))/sizeof(*(a)))"
					;"#define max(a,b)  ({ __typeof__ (a) _a = (a);  __typeof__ (b) _b = (b);  _a > _b ? _a : _b; })"
					;"#define min(a,b)  ({ __typeof__ (a) _a = (a);  __typeof__ (b) _b = (b);  _a < _b ? _a : _b; })"
					;"#define max(a,b) ({ __auto_type _a = (a);  __auto_type _b = (b); _a > _b ? _a : _b; })"
					;"#define min(a,b) ({ __auto_type _a = (a);  __auto_type _b = (b); _a < _b ? _a : _b; })"
		     
		     " "
		     
		     )
		    " "
		    "#endif"
		    " "))
    (write-source (asdf:system-relative-pathname 'cl-cpp-generator2 (merge-pathnames
								     #P"globals.h"
								     *source-dir*))
		  `(do0
		    "#ifndef GLOBALS_H"
		    " "
		    "#define GLOBALS_H"
		    " "
		    (include <thread>
			     <mutex>
			     <queue>
			     <deque>
			     <map>
			     <string>
			     <condition_variable>
			     <complex>)
		    
		    " "

		    ,@(loop for e in (reverse *global-code*) collect
			  e)

		    " "
		    ,(emit-globals)
		    " "
		    "#endif"
		    " "))))
