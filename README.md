# Json Parser in Lisp and Prolog

This software was written as a project for a University course by Adrian Castro and Nicola Saviano. The goal is to write a software in Prolog and Lisp that reads a JSON file and obtains a structured output relative to the language used. The only PDF in the project has all the specifications relative to it.

# JSON format

We're considering a simplified version of the JSON format:

```
JSON            ::= Object | Array
Object          ::= '{}' | '{' Members '}'
Members         ::= Pair | Pair ',' Members
Pair            ::= String ':' Value
Array           ::= '[]' | '[' Elements ']'
Elements        ::= Value | Value ',' Elements
Value           ::= JSON | Number | String
Number          ::= Digit+ | Digit+ '.' Digit+
Digit           ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
String          ::= '"' AnyCharSansDQ* '"' | '’' AnyCharSansSQ* '’'
AnyCharSansDQ   ::= <every character (ASCII) other than '"'>
AnyCharSansSQ   ::= <every character (ASCII) other than '’'>
```

# Examples

Some examples of the given simplified specification:

* Empty object
    ```json
    {}
    ```
* Empty array
    ```json
    []
    ```
* An object with two items:
    ```json
    {
        "nome": "Arthur",
        "cognome": "Dent"
    }
    ```
* A complex object, with a sub-object, which contains a number array (in general, arrays should not have to contain same type elements):
    ```json
    {
        "modello": "SuperBook 1234",
        "anno di produzione": 2014,
        "processore": {
            "produttore": "EsseTi",
            "velocità di funzionamento (GHz)": [1, 2, 4, 8]
        }
    }
    ```
* An example from Wikipedia (a possible menu item):
    ```json
    {
        "type": "menu",
        "value": "File",
        "items": [
            {"value": "New", "action": "CreateNewDoc"},
            {"value": "Open", "action": "OpenDoc"},
            {"value": "Close", "action": "CloseDoc"}
        ]
    }
    ```

# Usage

## Prolog
### Input as string
Input:
```prolog
?- json_parse('{"nome" : "Arthur", "cognome" : "Dent"}', O), json_get(O, ["nome"], R).
```
Output:
```
O = json_obj([(”nome”, ”Arthur”), (”cognome”, ”Dent”)])
R = "Arthur"
```
Input:
```prolog
?- json_parse('{"nome" : "Zaphod", "heads" : ["Head1", "Head2"]}', Z), json_get(Z, ["heads", 1], R).
```
Output:
```
Z = json_obj([(”name”, ”Zaphod”), (heads, json_array([”Head1”, ”Head2”]))])
R = "Head2"
```
Input:
```prolog
?- json_parse('[]', X).
```
Output:
```
X = json_array([]).
```

### Input/Output from and to file

Read from file:
```prolog
?- json_load('ex_arthurdent.json', JSON).
```
Write to file:
```prolog
?- json_write(json_obj([/* stuff */]), ’foo.json’).
```

## Lisp

### Input as string

Input:
```lisp
CL-prompt> (defparameter x (json-parse "{\"nome\" : \"Arthur\", \"cognome\" : \"Dent\"}"))
CL-prompt> x
```
Output:
```lisp
CL-prompt> x

(json-obj ("nome" "Arthur") ("cognome" "Dent"))
```

Using the previous example as base:

```lisp
CL-prompt> (json-get x "cognome")

"Dent"
```

Input:

```lisp
CL-prompt> (json-get (json-parse "{\"name\" : 
\"Zaphod\", \"heads\" : [[\"Head1\"], [\"Head2\"]]}") "heads" 1 0)
```
Output:
```lisp
"Head2"
```

Input:
```lisp
CL-prompt> (json-parse "[1, 2, 3]")
```
Output:
```lisp
(json-array 1 2 3)
```

### Input/Output from and to file

Read from file:
```lisp
CL-prompt> (json-load filename)
```
Write to file:
```lisp
(json-write JSON filename)
```