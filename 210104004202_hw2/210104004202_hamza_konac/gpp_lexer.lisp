
;;;;this function search charcters which are create commnet line ,if exist function return 
;;;; start posiiton of the comment else retun NIL
(defun find-position-of-first-double-semicolons (input)
  (search ";;" input :test 'string=))


;;;; check the token is match the identifieror not
(defun is-identifier (token)
  (if (> (length token) 0)
      (let ((first-char (char token 0)))
        (if (or (alpha-char-p first-char)  )
            (loop for i from 1 below (length token)
                  always (or (alpha-char-p (char token i)) (digit-char-p (char token i)) ))
            nil))
      nil))



;;;;Helper function for is-syntax-error.it checks the all stirng except first index. if first index is digit and rest of the substring contains 
;;;; character , it cause syntax error
(defun string-contains-char-p (string)
  (loop for c across string
        return t ; Return true as soon as any character is found
        finally (return nil))) ; Return false if the loop completes without finding a character

;;;;this function misson is find syntax error during the decleare identifier
(defun is-syntax-error (token)
  (and (>= (length token) 2)
       (digit-char-p (char token 0) 10)
       (string-contains-char-p token)))


;;;;this function misson is check the token is match the VALUEF or not 
(defun is-valuef (token)
  (let ((b-position (position #\b token)))
    (and b-position
         (>= (length token) (1+ b-position))
         (>= (length token) (+ 2 b-position))
         (digit-char-p (char token (- b-position 1)))
         (digit-char-p (char token (+ b-position 1)))))
)

;;this function misson is match the token with correct step for DFA
(defun match-token (token)
  (cond
    ((string= token "and")
     (format t "KW_AND:~a~%" token))
    ((string= token "or")
     (format t "KW_OR:~a~%" token))
    ((string= token "not")
     (format t "KW_NOT:~a~%" token))
    ((string= token "equal")
     (format t "KW_EQUAL:~a~%" token))
    ((string= token "less")
     (format t "KW_LESS:~a~%" token))
    ((string= token "nil")
     (format t "KW_NIL:~a~%" token))
    ((string= token "list")
     (format t "KW_LIST:~a~%" token))
    ((string= token "append")
      (format t "KW_APPEND:~a" token))
    ((string= token "concat")
      (format t "KW_CONCAT:~a" token))
    ((string= token "set")
     (format t "KW_SET:~a~%" token))
    ((string= token "def")
     (format t "KW_DEF:~a~%" token))
    ((string= token "for")
     (format t "KW_FOR:~a~%" token))
    ((string= token "if")
     (format t "KW_IF:~a~%" token))
    ((string= token "exit")
     (format t "KW_EXIT:~a~%" token))
    ((string= token "load")
     (format t "KW_LOAD:~a~%" token))
    ((string= token "display")
     (format t "KW_DISP:~a~%" token))
    ((string= token "true")
     (format t "KW_TRUE:~a~%" token))
    ((string= token "false")
     (format t "KW_FALSE:~a~%" token))
    ((string= token "+")
     (format t "OP_PLUS:~a~%" token))
    ((string= token "-")
     (format t "OP_MINUS:~a~%" token))
    ((string= token "/")
     (format t "OP_DIV:~a~%" token))
    ((string= token "*")
     (format t "OP_MULT:~a~%" token))
    ((string= token "(")
     (format t "OP_OP:~a~%" token))
    ((string= token ")")
     (format t "OP_CP:~a~%" token))
    ((string= token ",")
     (format t "OP_COM:~a~%" token))
    
    ((is-identifier token)
     (format t "IDENTIFIER:~a ~%" token ))
     ((is-valuef token)
     (format t "VALUEF:~a ~%" token ))
    ((is-syntax-error token)
       (format t "SYNTAX ERROR:~a~%" token))
    
    
    (t
     (format t "SYNTAX ERROR:~a~%" token))))



;;this function misson is find special charactesr such as operators
(defun is-special-char (chars)
  (or (char= chars #\+) (char= chars #\-) (char= chars #\*)
      (char= chars #\/)   (char= chars #\,) (char= chars #\() (char= chars #\))
    )
)

;;this function misson is find whitespace and Space
(defun is-whitespace-or-control-char (chars)
  (or (and (<= 9 (char-code chars)) (>= 13 (char-code chars))))
      (char= chars #\Space))



;;this function misson is split line of the file or string tht user enter respect whitespace and operators
;; during spling operation function ignore whispaces only
(defun tokenize-input (input)
    (let ((tokens '())
        (temp ""))
       
        (dotimes (i (length input))
            (let ((chars (char input i)))
            
                (cond
                
                    ;; If the character is not a whitespace or operator, add it to the current token.
                    (
                        (and (not(is-whitespace-or-control-char chars))
                            (not(is-special-char chars)))
                    
                        (setf temp (concatenate 'string temp (string chars))) 
                   
                    )
                    

                    ;; If the character is an operator or a whitespace character, and not a control character,
                    ;; add the current token to the list of tokens, then add the operator.
                    (
                            (if (not (is-whitespace-or-control-char chars))
                                (progn
                                    (if (> (length temp) 0)
                                        (progn
                                            (push temp tokens)
                                            (setf temp nil)))
                                (push (string (char input i)) tokens)))    
                           ; (push (string chars) tokens)
                    )

                    ;; If the character is none of the above, it's a part of the current token.
                    (t
                        (if (> (length temp) 0)
                            (progn
                                (push temp tokens)
                                (setf temp "")
                            nil)
                            
                        )
                    )
                )
                
                ( if( and (= i (- (length input) 1))
                          (not (is-special-char chars))) 
                      
                      (push temp tokens))
            )
        )
        (reverse tokens)
    )
    
)
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
                    (format t "COMMENT: ~a~%" (subseq line  position))
                    nil)
                  (let ((tokens (tokenize-input line)))
                    (loop for token in tokens do
                      (match-token token))))))))

( defun interpreter (line)
   (let ((position (find-position-of-first-double-semicolons line)))
              (if position
                  (progn
                    (let ((tokens (tokenize-input (subseq line 0 position))))
                      (loop for token in tokens do
                        (match-token token)))
                    (format t "COMMENT: ~a~%" (subseq line  position))
                    nil)
                  (let ((tokens (tokenize-input line)))
                    (loop for token in tokens do
                      (match-token token)))))
)

(defun interpret-shell()
	(loop (format t "~%> ") (interpreter (read-line))))

(defun gppinterpreter (&optional input)
  (if input
      (gppinterpreter-file input)
      (interpret-shell)))

(setq filename (car *args*))
(gppinterpreter filename)
