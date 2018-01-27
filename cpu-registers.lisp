(setq CPU-TYPE-GPU 'jaguar-gpu)
(setq CPU-TYPE-DSP 'jaguar-dsp)

(defclass risc-cpu ()
  ((register-file
    :accessor risc-cpu-register-file
    :initform (make-list 2 :initial-element (make-list 32 :initial-element 0)))
   (cpu-type
    :accessor risc-cpu-cpu-type
    :initform nil
    :initarg :cpu-type)))

(setq JAGRISC-GPU (make-instance 'risc-cpu :cpu-type CPU-TYPE-GPU))
(setq JAGRISC-DSP (make-instance 'risc-cpu :cpu-type CPU-TYPE-DSP))

(print (risc-cpu-register-file JAGRISC-GPU))
      
