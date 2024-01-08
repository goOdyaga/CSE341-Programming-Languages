

(load "gpp_lexer.lisp")
(load "ast.lisp")

(defstruct special-number
  int-part   ; Integer part of the number
  fract-part ; Fractional part of the number
)

(defun parse-special-number (input)
  "Parse a special number from a string in the format '4f1'."
  (let* ((f-position (position #\f input))
         (int-part-str (subseq input 0 f-position))
         (fract-part-str (subseq input (1+ f-position)))
         (int-part (parse-integer int-part-str))
         (fract-part (parse-integer fract-part-str)))
    (make-special-number :int-part int-part :fract-part fract-part)))


(defun shift (stack input)
  "Shift the next token from the input to the stack."
  (when input
    (push (pop input) stack))
  (values stack input))  ; Return both modified stack and input




(defun reduce-stack (stack)
  ;;(format t "~a  ~%" (nth 0 stack) ) 
  (cond
    ;; Reduction rule for an expression like (OP_OP OP_PLUS EXP EXP OP_CP)



    ((and (>= (length stack) 7)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "exp")
        (string= (car (nth 4 stack)) "exp")
        (string= (car (nth 5 stack)) "KW_DEF")
        (string= (car (nth 6 stack)) "OP_OP"))
        (let ((param1 (second (nth 3 stack)))
        (param2 (second (nth 2 stack)))
        (name (second (nth 4 stack)))
        (body (second (nth 1 stack)))
        ) 
        (add-function name param1 param2 body)
        (pop stack) ; These `pop` calls will remove elements from 'stack'
        (pop stack)
        (pop stack)
        (pop stack)
        (pop stack)
        (pop stack)
        (pop stack)
        (push (list "FUNCTION" 0 ) stack) 
        (return-from reduce-stack stack)
        ))

    ((and (>= (length stack) 3)
      (string= (car (nth 4 stack)) "OP_OP")
      (string= (car (nth 3 stack)) "exp")
      (string= (car (nth 2 stack)) "exp")
      (string= (car (nth 1 stack)) "exp")
      (string= (car (nth 0 stack)) "OP_CP"))
 (let* ((func-name ( ast-node-data (second(nth 3 stack))))
        (arg1 (second (nth 2 stack)))
        (arg2 (second (nth 1 stack)))
        (func-body (find-function-body func-name)))
   (if func-body
       (progn
         (setf body-of (substitute-arguments (my-function-body func-body)
                                        (ast-node-data (my-function-param1 func-body))
                                        arg1
                                        (ast-node-data (my-function-param2 func-body))
                                        arg2))
        
        
        (pop stack)
        (pop stack)
        (pop stack)
        (pop stack)
        (pop stack)
        (push (list "exp" body-of) stack)
        (return-from reduce-stack stack)
         )
       (format t "Function not found: ~a~%" func-name))))


    ((and (>= (length stack) 5)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "OP_PLUS")
        (string= (car (nth 4 stack)) "OP_OP"))
     (let ((result-sr (list "exp" (create-binary-op-node *node-add* (second (nth 2 stack)) (second (nth 1 stack)) )) ))
       (pop stack) (pop stack) (pop stack) (pop stack) (pop stack)
       (push  result-sr stack))
       
       )

       ((and (>= (length stack) 6)
        (string= (car (nth 5 stack)) "OP_OP")
         (string= (car (nth 4 stack)) "KW_IF")
         (string= (car (nth 3 stack)) "exp")
        
        (string= (car (nth 2 stack)) "exp")
         (string= (car (nth 1 stack)) "exp")
         (string= (car (nth 0 stack)) "OP_CP")
        )
     (let ((result-sr  (create-if-node 
     ( second (nth 3 stack)) 
     (second (nth 2 stack)) 
     (second (nth 1 stack)) 
     
     ) ))
       (pop stack) (pop stack) (pop stack) (pop stack) (pop stack) (pop stack )
       (push  (list "exp" result-sr) stack))
       
       )
      ((and (>= (length stack) 5)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "OP_MINUS")
        (string= (car (nth 4 stack)) "OP_OP"))
    (let ((result-sr (list "exp" (create-binary-op-node *node-min* (second (nth 2 stack)) (second (nth 1 stack)) )) ))
       (pop stack) (pop stack) (pop stack) (pop stack) (pop stack)
       (push  result-sr stack))
       
       )
       
       ((and (>= (length stack) 5)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "KW_LESS")
        (string= (car (nth 4 stack)) "OP_OP"))
    (let ((result-sr (list "exp" (create-binary-op-node *node-less* (second (nth 2 stack)) (second (nth 1 stack)) )) ))
       (pop stack) (pop stack) (pop stack) (pop stack) (pop stack)
       (push  result-sr stack))
       
       )

       ((and (>= (length stack) 5)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "OP_MULT")
        (string= (car (nth 4 stack)) "OP_OP"))
     (let ((result-sr (list "exp" (create-binary-op-node *node-mult* (second (nth 2 stack)) (second (nth 1 stack)) )) ))
       (pop stack) (pop stack) (pop stack) (pop stack) (pop stack)
       (push  result-sr stack))
       
       )((and (>= (length stack) 5)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "OP_DIV")
        (string= (car (nth 4 stack)) "OP_OP"))
     (let ((result-sr (list "exp" (create-binary-op-node *node-div* (second (nth 2 stack)) (second (nth 1 stack)) )) ))
       (pop stack) (pop stack) (pop stack) (pop stack) (pop stack)
       (push  result-sr stack))
       
       )
       ((and (>= (length stack) 1)
        (string= (car (nth 0 stack)) "VALUEF"))
        
       (let ((token (pop stack)))
         (let* ((parts (parse-special-number (second token)))
                )
             (let ((fraction-node (create-fraction-node (special-number-int-part parts) (special-number-fract-part parts))))
               (push (list "exp" fraction-node) stack)
                (return-from reduce-stack stack) 
               ))))

((and (>= (length stack) 1)
       (string= (car (nth 0 stack) )"IDENTIFIER"))
       (push (list "exp"  (create-identifier-node (second (pop stack)))) stack)
       (return-from reduce-stack stack)
       )

    ((and (>= (length stack) 3)
       (string= (car (nth 0 stack)) "OP_CP")

        (string= (car (nth 1 stack)) "KW_EXIT")

        (string= (car (nth 2 stack)) "OP_OP"))
        (pop stack)
        (pop stack)
        (pop stack)
        
       (push (list "EXIT" -1) stack)
       (return-from reduce-stack stack)
       )
  
  
    ;; Add more reduction rules here
    )
  stack)
  

(defun can-reduce-stack (stack)
  (or
   ;; Rule: OP_OP OP_PLUS $EXP SEXP OP_CP
   (and (>= (length stack) 1)
        (string= (car (nth 0 stack)) "OP_CP")

        (string= (car (nth 1 stack)) "KW_EXIT")

        (string= (car (nth 2 stack)) "OP_OP")
      )
   (and (>= (length stack) 5)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "OP_PLUS")
        (string= (car (nth 4 stack)) "OP_OP"))
     (and (>= (length stack) 5)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "KW_LESS")
        (string= (car (nth 4 stack)) "OP_OP"))

   ;; Rule: OP_OP OP_MINUS $EXP SEXP OP_CP
   (and (>= (length stack) 5)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "OP_MINUS")
        (string= (car (nth 4 stack)) "OP_OP"))
      (and (>= (length stack) 5)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "OP_MULT")
        (string= (car (nth 4 stack)) "OP_OP"))

        (and (>= (length stack) 5)
        (string= (car (nth 0 stack)) "OP_CP")
        (string= (car (nth 1 stack)) "exp")
        (string= (car (nth 2 stack)) "exp")
        (string= (car (nth 3 stack)) "OP_DIV")
        (string= (car (nth 4 stack)) "OP_OP"))

  
   ;; Rule: OP_OP IDENTIFIER SEXP
   (and (>= (length stack) 3)
        (string= (car (nth 3 stack)) "OP_OP")
         (string= (car (nth 2 stack)) "exp")
        ;; Check for SEXP as needed
        (string= (car (nth 1 stack)) "IDENTIFIER")
         (string= (car (nth 0 stack)) "OP_CP")
        )

   ;; Rule: OP_OP IDENTIFIER $EXP $EXP
   (and (>= (length stack) 3)
        (string= (car (nth 4 stack)) "OP_OP")
         (string= (car (nth 3 stack)) "exp")
         (string= (car (nth 2 stack)) "exp")
        ;; Check for SEXP as needed
        (string= (car (nth 1 stack)) "IDENTIFIER")
         (string= (car (nth 0 stack)) "OP_CP")
        )

    (and (>= (length stack) 6)
        (string= (car (nth 5 stack)) "OP_OP")
         (string= (car (nth 4 stack)) "KW_IF")
         (string= (car (nth 3 stack)) "exp")
        ;; Check for SEXP as needed
        (string= (car (nth 2 stack)) "exp")
         (string= (car (nth 1 stack)) "exp")
         (string= (car (nth 0 stack)) "OP_CP")
        )

))

 

;; The rest of your parser code remains the same.


    
(defun parse-cfg (input)
  
  (let ((stack '()))
    (loop while (or input stack)  ; Continue if there is input or potential reductions
      do (multiple-value-bind (new-stack new-input) (shift stack input)
           (setq firstinput (first input)) 
           (setq stack new-stack)
           (setq input new-input)
           )
      do(
            let ((new-stack (copy-list stack)))
            (cond
              ((and (can-reduce-stack (push firstinput new-stack)) (can-reduce-stack  stack))
                (return-from parse-cfg (format nil "UNRECOGNIZED TOKEN"))
              )
              (t
                (setq stack (reduce-stack stack))
                ( cond
                  ((and (< (length input) 1) (< (length stack) 2) (string= (first (nth 0 stack)) "FUNCTION" ))
                        (return-from parse-cfg (format nil "FUNCTION")))
                 ((and (< (length input) 1) (< (length stack) 2) (string= (first (nth 0 stack)) "EXIT" ))
                        (format t "(exit)")
                        (quit))
                ((and (< (length input) 1) (< (length stack) 2) (string= (first (nth 0 stack)) "exp" ))
                        (let ((arg (nth 0 stack)))
                         ; Get the argument from the second element of the stack
                        (let((result (evaluate-ast (second arg) )))
                        (pop stack)  
                        (cond 
                            ((eql 0 (fraction-frac result))
                                (return-from parse-cfg (format nil "Divide By 0 is illegal!!"))
                            )
                            ((and (eql -1 (fraction-frac result))(eql 1 (fraction-intval result)))
                                (return-from parse-cfg (format nil "True")))
                            ((and (eql -1 (fraction-frac result))(eql 0 (fraction-intval result)))
                                (return-from parse-cfg (format nil "False")))
                            ((and (eql -1 (fraction-frac result))(eql -1 (fraction-intval result)))
                                (return-from parse-cfg (format nil "Syntax Error!!")))
                            (t
                                (return-from parse-cfg (format nil "~Af~A" (fraction-intval result) (fraction-frac result)))
                            )


                        )
                        ))) ; Call your function with the argument

                    
                    ((and ( < (length input) 1) (> (length stack) 1))

                    (return-from parse-cfg (format nil "SYNTAX ERROR")))
                    
                )

              )
            )
        ))
     )
    stack) 
    




(defun gppinterpreter-file (file-path)
  (with-open-file (file file-path :direction :input)
    (loop for line = (read-line file nil)
         while line
         do (let ((position (find-position-of-first-double-semicolons line)))
              (if position
                  (progn
                    (let ((tokens (tokenize-input (subseq line 0 position))))
                      (loop for token in tokens do
                        (match-token token)))
                    (format t "COMMENT: a%" (subseq line  position))
                    nil)
                  (let ((tokens (tokenize-input line)))
                    (format t "> ~a~%" line)
                    (format t "> ~a~%" (parse-cfg (collect-match-tokens tokens)))
                    ))))))

(defun collect-match-tokens (tokens)
  (let ((results '()))  ; Initialize the 'results' list within the 'let' scope
    (loop for token in tokens do
          (let ((result (match-token token)))  ; Call 'match-token' for each token
            (push result results)))  ; Append the result to the 'results' list
    (nreverse results)))


(defun interpreter (line)
   (let ((position (find-position-of-first-double-semicolons line)))
              (if position
                  (progn
                    (let ((tokens (tokenize-input (subseq line 0 position))))
                      (loop for token in tokens do
                        (match-token token)))
                    (format t "COMMENT: a%" (subseq line  position))
                    nil)
                  (let ((tokens (tokenize-input line)))
                    (format t "> ~a~%" (parse-cfg (collect-match-tokens tokens)))
                      )))
                      
)
(defun interpret-shell()
	(loop (format t "~%> ") (interpreter (read-line))))

(defun gppinterpreter (&optional input)
  (if input
      (gppinterpreter-file input)
      (interpret-shell)))




(setq filename (car *args*))
(gppinterpreter filename)

;(loop
;  :with exit-flag = nil
;  :until exit-flag
;  :do
;  (let ((tokens (interpreter (read-line))))
;    (cond
;     
;      (t
;       (format t "> ~a~%" (parse-cfg tokens))))))
;
;(format t "Exiting the loop.~%")
