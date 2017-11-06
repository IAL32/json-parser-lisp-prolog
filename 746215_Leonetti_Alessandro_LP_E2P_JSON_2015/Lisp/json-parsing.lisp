; 728590 Trevi Alessandro
; 736424 Cassataro Riccardo
; 746215 Leonetti Alessandro


; for clarity - #'(lambda is a bit weighty for eye
(defmacro as (list &rest body) (append (list 'lambda list) body))
(defmacro -> (list &rest body) (append (list 'lambda list) body))

(defmacro @ (&rest args) (cons 'funcall args))

;;;; STREAM-INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set' mark
    (-> (stream yes no)
        (@ yes stream stream)))

(defun recall (position)
    (-> (stream yes no)
        (@ yes nil position)))

(defun string->stream (string)
    "Construct stream out of string"
    (coerce string 'list))

(defun stream->string (stream)
    "Turn stream back into the string"
    (coerce stream 'string))

(defun startswith (part list)
    "Test if first stream is a prefix of second"
    (cond
        ((and (null part) (null list))
            t)

        ((null part)
            t)

        ((null list)
            nil)

        ((eq (car part) (car list))
            (startswith (cdr part) (cdr list))
        )
    )) 

(defun drop (part list)
    "Consume part of the stream"
    (cond
        ((null part)
            list)

        ((null list)
            nil)

        (t
            (drop (cdr part) (cdr list)))))

; Basic parser adapter, consume & return 1 char.
;
(set' uncons 
    (-> (stream yes no)
        (cond
            ((null stream) (@ no "not the end of stream" stream))
            (t             (@ yes (first stream) (rest stream)))
        )))

;;;; PARSER COMBINATOR LINRARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; each parser is a function, accepting list of chars, success-continuation
; and failure-continuation and calling either of the latter two, depending
; on situation

(defun just (value)
    "Basic parser, always succeeds"
    (-> (stream yes no)
        "Just call the success route with value given"
        (@ yes value stream)))

(defun expected (what)
    "Basic parser, always fails"
    (-> (stream yes no)
        "Call the failure route with message given"
        (@ no what stream)))

(defun with (parser callback)
    "Connect parser's success route with function, which consumes its result
     and generates another parser"
    (-> (stream yes no)
        (@ parser 
            stream 
            (-> (result rest)
                "on success, feed result to the callback 
                 and run what it produces"
                (@ (@ callback result) rest yes no))
            no)))

(defun after (parser another)
    "As with, but ignore result of parser (it will only consume input)"
    (with parser (as (unused)
          another)))

(defun fallback (parser fallback)
    "Same as with, but feeds error to the fallback"
    (-> (stream yes no)
        (@ parser 
            stream 
            yes
            (-> (err rest)
                (@ (@ fallback err) rest yes no)))))

(defun or-else (parser another)
    "Analogue of 'after'"
    (fallback parser (as (unused)
              another)))

(defun str (token)
    "Parse some string (converting to char list along the process).
     On success, consume parsed token & return it.
     On failure, leave input as is constitute failure."
    (-> (stream yes no)
        (let ((list (string->stream token)))
        (if (startswith list stream)
            (@ yes token (drop list stream))
            (@ no  token            stream)))))

(defun satisfying (description predicate)
    "Parse a char, satisfying the predicate"
    (with mark   (as (here)
    (with uncons (as (char)
          (if (@ predicate char)
              (just char)
              (after (recall here) 
                     (expected description))))))))

(defun zero-or-more (parser)
    "Parse zero or more successes of parser"
    (or-else (one-or-more parser)
             (just nil)))

(defun one-or-more (parser)
    "Parse one or more successes of parser"
    (with  parser               (as (x)
    (with (zero-or-more parser) (as (xs)
          (just (cons x xs)))))))

(defun sep-by (separator parser)
    "Parse zero or more successes of parser, separated by separator.
     Separator results are ignored."
    (or-else (sep-by-1 separator parser)
             (just nil)))

(defun sep-by-1 (separator parser)
    "Parse one or more successes of parser, separated by separator."
    (with parser                                  (as (first)
    (with (zero-or-more (after separator parser)) (as (rest)
          (just (cons first rest)))))))

(defun is-space (char)
    "Predicate, check is char is a space"
    (member char (list #\Space #\Tab #\NewLine)))

; parse any amount of sequental spaces
;
(set 'spaces 
    (zero-or-more 
        (satisfying "space" #'is-space)))

(defun token (token)
    "Parse token & all spaces after"
    (with  (str token) (as (result)
    (after  spaces           
           (just result)))))

(defun one-of (string)
    "Predicate, check if a char which is an element of the given string"
    (let ((list (string->stream string)))
        (-> (char)
            (member char list))))

(defun any (description &rest parsers)
    "Check if any given parser succeeds"
    (any-raw description parsers))

(defun any-raw (description parsers)
    (if (null parsers)
        (expected description)
        (or-else (first parsers)
                 (any-raw description (rest parsers)))))

(defun recursion-point (quoted-parser)
    "Defer evaluation of parser body till input stream arrives.
     Used to prevent infinite loop on recursively-built parsers."
    (-> (stream yes no)
        (@ (eval quoted-parser) stream yes no)))

(defun just-wrapped (name item)
    "Like just, but append a tag to the result before return"
    (just (cons name item)))

; parses any json value
;
(set 'json 
    (recursion-point
        '(any "json-value"
            json-number
            json-string
            json-array
            json-object)))

; parses json char
;
(set 'json-char
    (any "json-char"
        (after (str "\\\"")
               (just #\"))

        (satisfying "non-dquote-char" 
            (-> (c) (char/= c #\")))))

; parses json number
;
(set 'json-number
    (let ((json-number-text
            (one-or-more 
                (satisfying "number" 
                    (one-of "1234567890")))))
    
        (with  json-number-text (as (text)
        (after spaces
              (just (parse-integer (stream->string text))))))))

; parses json string
;
(set 'json-string
    (after (str "\"")
    (with  (zero-or-more json-char) (as (content)
    (after (str "\"")
    (after  spaces
           (just (stream->string content))))))))

; parses json array
;
(set' json-array
    (after (token "[")
    (with  (sep-by (token ",") json) (as (items)
    (after (token "]")
    (after  spaces
           (just-wrapped 'json-array items)))))))

; parses a "name : value" pair inside an object
;
(set' json-pair
    (with   json-string (as (name)
    (after (token ":")
    (with   json        (as (value)
    (after  spaces
           (just (list name value)))))))))

; parses json object
;
(set' json-object
    (after (token "{")
    (with (sep-by (token ",") json-pair) (as (fields)
    (after (token "}")
    (after  spaces
           (just-wrapped 'json-object fields)))))))

; for easier debug (set another parser to test it)
;

(defun json-parse (stream)
    (json-parse-aux json stream))

(defun json-parse-aux (parser stream)
    "Exported, runs json parser on a given string."
    (@ parser 
        (string->stream stream)
        (-> (result at) result)
        (-> (err    at) (error (format nil "~S: ~S" err (stream->string at))))))

(defun nth-item (n list)
    "Helper, gets nth elem in a list, issuing an error 
     if list isn't long enouth."
    (cond 
        ((null list)
            (error "index out of diapasone"))

        ((= 0 n)
            (car list))

        (t
            (nth-item (1- n) (cdr list)))
    ))

(defun find-key (key map)
    "Retrieves a value from (key value) pair list"
    (cond
        ((null map)
            (error "no key"))

        ((equal key (caar map))
            (cadar map))

        (t
            (find-key key (cdr map)))
    ))

(defun json-dot-aux (json path)
    (cond
        ((null path)
            json)

        ((eq (car json) 'json-array)
            (json-dot-aux 
                (nth-item (first path) (rest json))
                (rest path)))

        ((eq (car json) 'json-object)
            (json-dot-aux 
                (find-key (first path) (rest json))
                (rest path)))

        ; access into numbers or string is prohibited
        (t
            (error "error"))
    ))

(defun json-dot (json &rest path)
    "Performs access inside json by path"

    (json-dot-aux json path))
