; %% Authors:
; %% Adrian Castro,  Matricola 816015
; %% Nicola Saviano, Matricola 822436

; %% Funzione per gestire gli errori
(defun error-nil (string &optional arg)
  (error string arg)
  nil
)

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %% Inizio parte relativa al parsing
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; %% Restituisco il numero di caratteri vuoti dalla posizione attuale
; %% alla posizione del prossimo carattere non vuoto
(defun json-parse--iswhitespace (char)
  (member
   char
   '(#\space #\newline #\return #\tab)))
(defun json-parse--whitespace-aux (input pos)
  (cond 
   ; Fine della stringa
   ((= (length input) pos)
    pos)
   ; Caratteri da saltare
   ((json-parse--iswhitespace (char input pos))
    (json-parse--whitespace-aux 
     input 
     (+ pos 1)))
   ; Qualsiasi carattere
   (T 
    pos)))
(defun json-parse--whitespace (input pos)
  (json-parse--whitespace-aux input pos))

; %% Leggo una stringa, contenuta in singoli o doppi apici
; %% Restituisco la stringa letta, considerato l'escape degli apici, e la
; %% lunghezza della lettura effettuata (apici compresi)
(defun json-parse--string-aux (input out evalSpecial pos Type)
  (cond 
   ; Fine inaspettata della stringa json
   ((= (length input) pos)
    (error-nil "Unexpected end of JSON"))
   ; Qualsiasi carattere preceduto da una carattere di escape
   ((= evalSpecial 1)
    (json-parse--string-aux 
     input
     (push (char input pos) out) 
     0
     (+ pos 1)
     Type))
   ; Carattere di escape
   ((equal (char input pos) #\\)
    (json-parse--string-aux
     input
     (push (char input pos) out)
     1
     (+ pos 1)
     Type))
   ; Doppio apice, fine della stringa
   ((equal (char input pos) Type)
    (cons
     (format nil "~{~a~^~}" (reverse out))
     (+ pos 1)))
   ; Qualsiasi carattere
   (T 
    (json-parse--string-aux
     input
     (push (char input pos) out)
     0
     (+ pos 1)
     Type))))
(defun json-parse--string (input pos)
  (cond 
   ; Parse stringa con doppi o singoli apici
   ((or
     (equal (char input pos) #\")
     (equal (char input pos) #\'))
    (json-parse--string-aux input '() 0 (+ pos 1) (char input pos)))
   ; Carattere iniziale della stringa non ammesso
   (T
    (error-nil "Unexpected char at beginning of String"))))

; %% Leggo un numero nel formato digit+ | digit+ "." digit+
; %% Restituisco il numero letto e la lunghezza della lettura
(defun json-parse--number-aux (input out pos)
  (cond 
   ; Fine inaspettata della stringa
   ((= (length input) pos)
    (error-nil "Unexpected end of JSON"))
   ; Numero o punto
   ((or
     (equal (char input pos) #\.)
     (digit-char-p (char input pos) 10))
    (json-parse--number-aux 
     input
     (push (char input pos) out) 
     (+ pos 1)))
   ; Qualsiasi carattere, fine del parsing del numero
   (T 
    (cons out pos))))
(defun json-parse--number (input pos)
  (if (or
       (equal (char input pos) #\-)
       (equal (char input pos) #\+))
      (let ((x (json-parse--number-aux input '() (+ pos 1))))
        (cons
         (with-input-from-string 
             (in 
              (concatenate 
               'string
               (format nil "~a" (char input pos))
               (format nil "~{~a~^~}" (reverse (car x)))))
           (read in))
         (cdr x)))
    (let ((x (json-parse--number-aux input '() pos)))
      (cons
       (with-input-from-string 
           (in 
            (format nil "~{~a~^~}" (reverse (car x))))
         (read in))
       (cdr x)))))

; %% Leggo un array
; %% Restituisco un formato gestibile da lisp (list) e la
; %% lunghezza della lettura effettuata
(defun json-parse--array-aux (input pos &optional (out '()) (arrStage 0))
  (cond
   ; Fine inaspettata della stringa json
   ((= (length input) pos)
    (error-nil "Unexpected end of JSON"))
   ; Spazio vuoto
   ((json-parse--iswhitespace (char input pos))
    (json-parse--array-aux 
     input
     (json-parse--whitespace input pos)
     out
     arrStage))
   ; Carattere ] - Fine array
   ((and
     (equal (char input pos) #\])
     (or
      (= arrStage 0)
      (= arrStage 2)))
    (cons (reverse out) (+ pos 1)))
   ; Carattere , - Nuovo elemento nell'array
   ((and
     (equal (char input pos) #\,)
     (= arrStage 2))
    (json-parse--array-aux 
     input
     (+ pos 1)
     out
     1))
   ; Carattere { - Inizio nuovo oggetto
   ((and
     (equal (char input pos) #\{)
     (or
      (= arrStage 0)
      (= arrStage 1)))
    (let ((x (json-parse--object input pos)))
      (json-parse--array-aux 
       input
       (cdr x)
       (push (car x) out)
       2)))
   ; Carattere [ - Inizio nuovo array
   ((and
     (equal (char input pos) #\[)
     (or
      (= arrStage 0)
      (= arrStage 1)))
    (let ((x (json-parse--array input pos)))
      (json-parse--array-aux 
       input
       (cdr x)
       (push (car x) out)
       2)))
   ; Carattere " o ' - Inizio di una stringa
   ((and
     (or 
      (equal (char input pos) #\") 
      (equal (char input pos) #\'))
     (or
      (= arrStage 0)
      (= arrStage 1)))
    (let ((x (json-parse--string input pos)))
      (json-parse--array-aux 
       input
       (cdr x)
       (push (car x) out)
       2)))
   ; Qualsiasi carattere numerico - Inizio di un numero
   ((and
     (or
      (equal (char input pos) #\-)
      (equal (char input pos) #\+)
      (digit-char-p (char input pos) 10))
     (or
      (= arrStage 0)
      (= arrStage 1)))
    (let ((x (json-parse--number input pos)))
      (json-parse--array-aux 
       input
       (cdr x)
       (push (car x) out)
       2)))
   ; Errore - Carattere non ammesso dal parser
   (T 
    (error-nil "Unexpected char in array ( ~S )" (char input pos)))))
(defun json-parse--array (input pos)
  (let ((x (json-parse--array-aux input (+ pos 1))))
    (cons
     (push 'json-array (car x))
     (cdr x))))

; %% Leggo una coppia chiave - valore, ossia un elemento dell'oggetto
; %% La restituisco in un formato gestibile da lisp (cons)
(defun json-parse--pair-aux (input out pairStage pos)
  (cond
   ; Fine inaspettata della stringa json
   ((= (length input) pos)
    (error-nil "Unexpected end of JSON"))
   ; Fine parse del pair
   ((= pairStage 3)
    (cons (reverse out) pos))
   ; Spazio vuoto
   ((json-parse--iswhitespace (char input pos))
    (json-parse--pair-aux 
     input
     out
     pairStage
     (json-parse--whitespace input pos)))
   ; Carattere { - Inizio nuovo oggetto
   ((and 
     (equal (char input pos) #\{) 
     (= pairStage 2))
    (let ((x (json-parse--object input pos)))
      (json-parse--pair-aux 
       input
       (push (car x) out)
       (+ pairStage 1)
       (cdr x))))
   ; Carattere [ - Inizio nuovo array
   ((and 
     (equal (char input pos) #\[) 
     (= pairStage 2))
    (let ((x (json-parse--array input pos)))
      (json-parse--pair-aux 
       input
       (push (car x) out)
       (+ pairStage 1)
       (cdr x))))
   ; Carattere : - Inizio secondo elemento del pair
   ((and 
     (equal (char input pos) #\:) 
     (= pairStage 1))
    (json-parse--pair-aux 
     input
     out
     (+ pairStage 1)
     (+ pos 1)))
   ; Carattere " o ' - Inizio di una stringa
   ((and 
     (or 
      (equal (char input pos) #\") 
      (equal (char input pos) #\')) 
     (or 
      (= pairStage 0) 
      (= pairStage 2)))
    (let ((x (json-parse--string input pos)))
      (json-parse--pair-aux 
       input
       (push (car x) out)
       (+ pairStage 1)
       (cdr x))))
   ; Qualsiasi carattere numerico - Inizio di un numero
   ((or
     (equal (char input pos) #\-)
     (equal (char input pos) #\+)
     (digit-char-p (char input pos) 10))
    (let ((x (json-parse--number input pos)))
      (json-parse--pair-aux 
       input
       (push (car x) out)
       (+ pairStage 1)
       (cdr x))))
   ; Errore - Carattere non ammesso dal parser
   (T 
    (error-nil "Unexpected char in pair ( ~S )" (char input pos)))))
(defun json-parse--pair (input pos)
  (json-parse--pair-aux input '() 0 pos))

; %% Leggo un oggetto
; %% Restituisco un formato gestibile da lisp (list) e la
; %% lunghezza della lettura effettuata
(defun json-parse--object-aux (input pos &optional (out '()) (objStage 0))
  (cond
   ; Fine inaspettata della stringa json
   ((= (length input) pos)
    (error-nil "Unexpected end of JSON 1"))
   ; Spazio vuoto
   ((json-parse--iswhitespace (char input pos))
    (json-parse--object-aux 
     input
     (json-parse--whitespace input pos)
     out
     objStage))
   ; Carattere } - Fine oggetto
   ((and
     (equal (char input pos) #\})
     (or
      (= objStage 0)
      (= objStage 2)))
    (cons (reverse out) (+ pos 1)))
   ; Carattere , - Nuovo elemento dell'oggetto
   ((and
     (equal (char input pos) #\,)
     (= objStage 2))
    (json-parse--object-aux 
     input
     (+ pos 1)
     out
     1))
   ; Carattere " o ' - Inizio di una stringa, elemento iniziale di un pair
   ((and
     (or
      (equal (char input pos) #\")
      (equal (char input pos) #\'))
     (or
      (= objStage 0)
      (= objStage 1)))
    (let ((x (json-parse--pair input pos)))
      (json-parse--object-aux 
       input
       (cdr x)
       (push (car x) out)
       2)))
   ; Errore - Carattere non ammesso dal parser
   (T 
    (error-nil "Unexpected char in object ( ~S )" (char input pos)))))
(defun json-parse--object (input pos)
  (let ((x (json-parse--object-aux input (+ pos 1))))
    (cons
     (push 'json-obj (car x))
     (cdr x))))

; %% Leggo una stringa json e restituisco un formato gestibile da lisp
(defun json-parse-aux (input pos)
  (cond
   ; Stringa json non valida
   ((= (length input) pos)
    (error-nil "Invalid JSON string")
    nil)
   ; Spazio vuoto
   ((json-parse--iswhitespace (char input pos))
    (json-parse-aux input (json-parse--whitespace input pos)))
   ; Carattere { - Inizio nuovo oggetto
   ((equal (char input pos) #\{)
    (let ((x (json-parse--object input pos)))
      (if (= (length input) (json-parse--whitespace input (cdr x))) 
          (car x)
        (error-nil "Invalid JSON string"))))
   ; Carattere [ - Inizio nuovo array
   ((equal (char input pos) #\[)
    (let ((x (json-parse--array input pos)))
      (if (= (length input) (json-parse--whitespace input (cdr x))) 
          (car x)
        (error-nil "Invalid JSON string"))))
   ; Errore - Carattere non ammesso dal parser
   (T 
    (error-nil "Unexpected char in JSON string ( ~S )" (char input pos)))))
(defun json-parse (input)
  (json-parse-aux input 0))



; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %% Inizio parte relativa al get degli elementi
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; %% Cerco una chiave in un oggetto e restituisco il suo valore
(defun json-get--inobject (obj name)
  (if (eql nil (car obj))
      (error-nil "Name ( ~S ) not found in object" name)
    (if (equalp (car (car obj)) name)
        (car (cdr (car obj)))
      (json-get--inobject (cdr obj) name))))

; %% Restituisco l'elemento in una certa posizione di un array
(defun json-get--inarray (array pos)
  (if (= pos 0)
      (if(eql nil (car array))
          (error-nil "Array Index ( ~S ) Out Of Bound" pos)
        (car array))
    (json-get--inarray (cdr array) (- pos 1))))

; %% Effettuo la ricerca in un json
(defun json-get-aux(json name)
  (if(or (eql nil name) (eql nil (car name)))
      json
    (cond
     ((and 
       (typep (car name) 'integer) 
       (> (car name) -1)
       (eql (car json) 'json-array))
      (json-get-aux
       (json-get--inarray (cdr json) (car name)) 
       (cdr name)))
     ((and 
       (typep (car name) 'string) 
       (eql (car json) 'json-obj))
      (json-get-aux 
       (json-get--inobject (cdr json) (car name)) 
       (cdr name)))
     (T
      (error-nil "Unexpected getter type")))))
(defun json-get (json &rest name)
  (json-get-aux json name))
				
				
	
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %% Inizio parte relativa alla conversione da lista a json
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%				

; %% Aggiungo gli apici doppi a una stringa
(defun json-stringify--string (string)
  (concatenate
   'string
   "\"" string "\"")
  )

; %% Converto un numero in una stringa, in modo da poterlo gestire meglio
(defun json-stringify--number (num)
  (write-to-string num))

; %% Converto un elemento (oggetto, array, stringa, numero) in una stringa
(defun json-stringify--element (element)
  (cond
   ((or (typep element 'float) (typep element 'integer))
    (json-stringify--number element))
   ((typep element 'string)
    (json-stringify--string element))
   ((typep element 'list)
    (json-stringify element))))

; %% Converto una coppia chiave-valore in una stringa che rappresenta un elemento
; %% di un oggetto json
(defun json-stringify--pair (pair)
  (concatenate
   'string
   (json-stringify--string (car pair)) ":" (json-stringify--element (cadr pair))))

; %% Converto un array in una stringa
(defun json-stringify--array-aux (arr pos out)
  (if (= pos (list-length arr))
      out
    (if (= pos 1)
        (json-stringify--array-aux 
         arr 
         (+ pos 1) 
         (concatenate 
          'string 
          out (json-stringify--element (nth pos arr))))
      (json-stringify--array-aux 
       arr 
       (+ pos 1) 
       (concatenate 
        'string 
        out ", " (json-stringify--element (nth pos arr)))))))
(defun json-stringify--array (arr out)
  (concatenate 
   'string 
   out "[" (json-stringify--array-aux arr 1 out) "]"))

; %% Converto un oggetto in una stringa
(defun json-stringify--object-aux (obj pos out)
  (if (= pos (list-length obj))
      out
    (if (= pos 1)
        (json-stringify--object-aux 
         obj 
         (+ pos 1) 
         (concatenate 
          'string 
          out (json-stringify--pair (nth pos obj))))
      (json-stringify--object-aux
       obj 
       (+ pos 1) 
       (concatenate 
        'string 
        out ", " (json-stringify--pair (nth pos obj)))))))
(defun json-stringify--object (obj out)
  (concatenate 
   'string 
   out "{" (json-stringify--object-aux obj 1 out) "}"))

; %% Converto un json nella stringa che lo rappresenta
(defun json-stringify-aux (json out)
  (cond
   ((eql (car json) 'json-array)
    (json-stringify--array json out))
   ((eql (car json) 'json-obj)
    (json-stringify--object json out))
   (T
    (error-nil "Invalid JSON"))))
(defun json-stringify (json)
  (json-stringify-aux json ""))
				
				
				
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %% Inizio parte relativa al I/O da/su file
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%					

; %% Leggo ed effettuo il parse del json letto
(defun json-load--stream (stream &optional (prefix ""))
  (let ((line (read-line stream nil nil)))
    (if (equal line nil)
        prefix
      (concatenate 
       'string 
       prefix
       (format nil "~a~%" line)
       (json-load--stream stream prefix)))))
(defun json-load--file (name)
  (let ((in (open name)))
    (let ((x (json-load--stream in)))
      (close in)
      x)))
(defun json-load (name)
  (json-parse (json-load--file name)))

; %% Converto il json in una stringa e la scrivo su un file
(defun json-write--file (name str)
  (let ((out (open name 
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)))
    (format out str)
    (close out)
    str))			
(defun json-write (JSON filename)
  (json-write--file
   filename
   (json-stringify JSON))
  filename)