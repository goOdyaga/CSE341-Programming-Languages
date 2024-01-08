;; Define a struct for fractions
(defstruct fraction
  (intval 0)
  (frac 0))

;; Define enum-like constants for node types
(defparameter *node-fraction* 0)
(defparameter *node-add* 1)
(defparameter *node-min* 2)
(defparameter *node-mult* 3)
(defparameter *node-div* 4)
(defparameter *node-iden* 5)
(defparameter *node-if* 6)
(defparameter *node-less* 7)

;; Define a struct for AST nodes
(defstruct ast-node
  (type nil)
  (data nil))

;; Define a struct for functions
(defstruct my-function
  (name nil)
  (param1 nil)
  (param2 nil)
  (body nil))

;; Define a global function table
(defparameter *function-table* '())


;; Define a function to add a function to the table
(defun add-function (name param1 param2 body)
  (if (< (length *function-table*) 100)
      (push (make-my-function :name name :param1 param1 :param2 param2 :body body) *function-table*)
      (format t "Function table is full~%")))


;; Define functions to create different types of AST nodes
(defun create-fraction-node (value frac)
  (make-ast-node :type *node-fraction* :data (make-fraction :intval value :frac frac)))

(defun create-identifier-node (identifier)
  (make-ast-node :type *node-iden* :data identifier))

(defun create-if-node (condition then-branch else-branch )
  (make-ast-node :type *node-if*
                 :data (list  condition then-branch else-branch)))

(defun create-binary-op-node (type left right)
  (make-ast-node :type type :data (list left right)))
  

(defun evaluate-ast (node)
  (let ((fraction (make-fraction :intval -1 :frac -1)))
    (let ((left-val (make-fraction))
          (right-val (make-fraction))
          (condition-value (make-fraction)))
      (if (null node)
          fraction
          (cond 
            ( (= (ast-node-type node) *node-fraction*)
             (let ((fraction-data (ast-node-data node)))
               (setf (fraction-intval fraction) (fraction-intval fraction-data))
               (setf (fraction-frac fraction) (fraction-frac fraction-data))))
            ( (= (ast-node-type node) *node-add*)
             (setf left-val (evaluate-ast  (first (ast-node-data node))))
             (setf right-val (evaluate-ast (second (ast-node-data node))))
             (setf (fraction-intval fraction)
                   (+ (* (fraction-intval left-val) (fraction-frac right-val))
                      (* (fraction-intval right-val) (fraction-frac left-val))))
             (setf (fraction-frac fraction)
                   (* (fraction-frac left-val) (fraction-frac right-val))))
            ((= (ast-node-type node) *node-min*)
             (setf left-val (evaluate-ast  (first (ast-node-data node))))
             (setf right-val (evaluate-ast (second (ast-node-data node))))
             (setf (fraction-intval fraction)
                   (- (* (fraction-intval left-val)  (fraction-frac right-val))
                      (* (fraction-intval right-val) (fraction-frac left-val))))
             (setf (fraction-frac fraction)
                   (* (fraction-frac left-val) (fraction-frac right-val))))
            ((= (ast-node-type node) *node-mult*)
             (setf left-val (evaluate-ast  (first (ast-node-data node))))
             (setf right-val (evaluate-ast (second (ast-node-data node))))
             (setf (fraction-intval fraction)
                   (* (fraction-intval left-val) (fraction-intval right-val)))
             (setf (fraction-frac fraction)
                   (* (fraction-frac left-val) (fraction-frac right-val))))
            ((= (ast-node-type node) *node-div*)
             (setf left-val (evaluate-ast  (first (ast-node-data node))))
             (setf right-val (evaluate-ast (second (ast-node-data node))))
             (setf (fraction-intval fraction)
                   (/ (* (fraction-intval left-val) (fraction-intval right-val))
                      (fraction-intval right-val)))
             (setf (fraction-frac fraction)
                   (* (fraction-frac left-val) (fraction-frac right-val))))
            ((= (ast-node-type node) *node-if*)
                 (setf condition-value (evaluate-ast (first (ast-node-data node))))

                 (cond ((= 1 (fraction-intval condition-value))

                     (setf fraction (evaluate-ast (second(ast-node-data node)))))
                     ( (not (null (third(ast-node-data node))))
                         (setf fraction (evaluate-ast (third(ast-node-data node))))
                         nil)))
            ((= (ast-node-type node) *node-less*)
             (setf left-val (evaluate-ast  (first (ast-node-data node))))
             (setf right-val (evaluate-ast (second (ast-node-data node))))
             (let ((left-number (* (fraction-intval left-val) (fraction-frac right-val)))
                   (right-number (* (fraction-intval right-val) (fraction-frac left-val))))
               (if (< left-number right-number)
                   (setf (fraction-intval fraction) 1)
                   (setf (fraction-intval fraction) 0)))
             (setf (fraction-frac fraction) -1)))
             ))

      (let ((common-divisor (gcd (fraction-intval fraction) (fraction-frac fraction))))
        (if (/= common-divisor 0)
            (progn
              (setf (fraction-intval fraction) (/ (fraction-intval fraction) common-divisor))
              (setf (fraction-frac fraction) (/ (fraction-frac fraction) common-divisor)))))
      fraction))

;; Define a function to copy an AST node
(defun copy-ast (node)
  (if (null node)
      nil
      (let ((new-node (make-ast-node :type (ast-node-type node) :data (ast-node-data node))))
        (if (or (eql (ast-node-type node) *node-add*)
                (eql (ast-node-type node) *node-min*)
                (eql (ast-node-type node) *node-mult*)
                (eql (ast-node-type node) *node-div*))
            (progn
              (setf (ast-node-data new-node) (list (copy-ast (second (ast-node-data node))) (copy-ast (first (ast-node-data node)))))
              ))
        new-node)))


(defun substitute-arguments (body param1 arg1 param2 arg2)
  (cond
    ((null body) nil)
    ((and (equal (ast-node-type body) *node-iden*)
          (string= (ast-node-data body) param1))
     (copy-ast arg1))
    ((and (equal (ast-node-type body) *node-iden*)

          (string= (ast-node-data body) param2))
     (copy-ast arg2))
    ((or (eql (ast-node-type body) *node-add*)(eql (ast-node-type body) *node-min*) (eql (ast-node-type body) *node-mult*) (eql (ast-node-type body) *node-div*))
     (create-binary-op-node  (ast-node-type body)
                    
                           (substitute-arguments (first (ast-node-data body)) param1 arg1 param2 arg2)
                           (substitute-arguments (second (ast-node-data body))  param1 arg1 param2 arg2)))
    (t 
    (copy-ast body))))


;; Define a function to find a function body by name
(defun find-function-body (name)
  (if (null *function-table*)
      (progn
        (format t "Function table is empty~%")
        nil)
      (loop for function in *function-table*
            when (and (typep function 'my-function)
                      (typep (my-function-name function) 'ast-node)
                      (string= (ast-node-data (my-function-name function)) name))
            return function)
      ))

