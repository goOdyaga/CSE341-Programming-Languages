;;;this function search charcters which are create commnet line ,if exist function return 
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
  (let ((b-position (position #\f token)))
    (and b-position
         (>= (length token) (1+ b-position))
         (>= (length token) (+ 2 b-position))
         (digit-char-p (char token (- b-position 1)))
         (digit-char-p (char token (+ b-position 1)))))
)

;;this function misson is match the token with correct step for DFA
(defun match-token (token)
  (cond
    ;; Matching specific keywords and operators
    ((string= token "and") (list "KW_AND" token))
    ((string= token "or") (list "KW_OR" token))
    ((string= token "not") (list "KW_NOT" token))
    ((string= token "equal") (list "KW_EQUAL" token))
    ((string= token "less") (list "KW_LESS" token))
    ((string= token "nil") (list "KW_NIL" token))
    ((string= token "list") (list "KW_LIST" token))
    ((string= token "append") (list "KW_APPEND" token))
    ((string= token "concat") (list "KW_CONCAT" token))
    ((string= token "set") (list "KW_SET" token))
    ((string= token "def") (list "KW_DEF" token))
    ((string= token "for") (list "KW_FOR" token))
    ((string= token "if") (list "KW_IF" token))
    ((string= token "exit") (list "KW_EXIT" token))
    ((string= token "load") (list "KW_LOAD" token))
    ((string= token "display") (list "KW_DISP" token))
    ((string= token "true") (list "KW_TRUE" token))
    ((string= token "false") (list "KW_FALSE" token))
    ((string= token "+") (list "OP_PLUS" token))
    ((string= token "-") (list "OP_MINUS" token))
    ((string= token "/") (list "OP_DIV" token))
    ((string= token "*") (list "OP_MULT" token))
    ((string= token "(") (list "OP_OP" token))
    ((string= token ")") (list "OP_CP" token))
    ((string= token ",") (list "OP_COM" token))
    
    ;; Handling identifiers, VALUEFs, syntax errors, and unrecognized tokens
    ((is-identifier token) (list "IDENTIFIER" token))
    ((is-valuef token) (list "VALUEF" token))
    ((is-syntax-error token) (list "SYNTAX ERROR" token))
    (t (list "UNRECOGNIZED TOKEN" token))))




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
