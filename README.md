# secret-code
A Common Lisp package for making and breaking simple ciphers

## To start
1. Clone this git repository
2. Open a Common Lisp interpreter such as [Steel Bank Common Lisp](http://www.sbcl.org/)
3. Load this package

## Load package


```common-lisp
(load "secret-code.lisp")
(in-package :secret-code)

;; Assumptions:
;; - the message is in English language
;; - only uses capital A-Z and no other characters (numbers, spaces, brackets, and punctuation)
;; We need those assumptions so we can use letter frequencies to crack the code.
```

## Caesar cipher with a single character key


```common-lisp
;; This encodes the message with a single character key -- in this case, Z.
(defparameter *c1*
  (caesar-encode "ITSOVERNINETHOUSAND"
		 (char->code #\Z)))

;; Encoded message:
*c1*
;; If you know the key, you can decode it:
(caesar-decode *c1* (char->code #\Z))

;; Otherwise try cracking the code automatically.
(caesar-crack *c1*)
```

    *C1*
    
    "HSRNUDQMHMDSGNTRZMC"
    
    "ITSOVERNINETHOUSAND"

    "ITSOVERNINETHOUSAND"


## Caesar cipher with repeated key


```common-lisp
;; This encodes a message with repeated key DOHDOHDOH...
(defparameter *c2*
  (repeating-caesar-encode
   "LISANEEDSBRACESDENTALPLAN"
   (string->code "DOH")))

*c2*

;; If you know the key, you can uncover the message
(repeating-caesar-decode *c2* (string->code "DOH"))

;; If you don't know the key, you can still crack the code
;; if you correctly guess the key is 3 characters long.
(repeating-caesar-crack *c2* 3)
```


    *C2*

    "OWZDBLHRZEFHFSZGSUWOSSZHQ"

    "LISANEEDSBRACESDENTALPLAN"

    "LISANEEDSBRACESDENTALPLAN"


## A long Caesar cipher with repeated key


```common-lisp
;; If the key length is longer than a few characters
;; then you need a longer ciphertext to crack it
(defparameter *c3*
  (repeating-caesar-encode
   "NOIMUSTKILLTHEDEMONSHESHOUTEDTHERADIOSAIDNOJOHNYOUARETHEDEMONSANDTHENJOHNWASAZOMBIE"
   (string->code "JOSON")))

*c3*

;; Message cracked if you guess the key is 5 characters long.
(repeating-caesar-crack *c3* 5)
```

    *C3*

    "WCAAHBHCWYUHZSQNAGBFQSKVBDHWRGQSJOQRCKOVMBGXBQBQCHJFWHUNRWABWGSBQCVWBWXVFKNBORCZKWW"

    "NOIMUSTKILLTHEDEMONSHESHOUTEDTHERADIOSAIDNOJOHNYOUARETHEDEMONSANDTHENJOHNWASAZOMBIE"


## A short Caesar cipher with repeated key


```common-lisp
;; But if you have a short message relative to the key length...
(defparameter *c4*
  (repeating-caesar-encode "WEARENUMBERONE"
    (string->code "ROBBIE")))

*c4*

;; You still can't crack the code even though you know the key is 6 characters long.
(repeating-caesar-crack *c4* 6)
```

    *C4*

    "NSBSMRLACFZSES"

    "NASRESLITERTEA"


```common-lisp
;; Your turn!
;; ATOXYNGVKTVDBGZLXVKXMVHWXL
```

# Author, license, acknowledgements

Copyright (C) 2019  Alan Tseng

License: GPLv3

Notes:
- English monogram frequencies file adapted from [here](http://practicalcryptography.com/cryptanalysis/letter-frequencies-various-languages/english-letter-frequencies/)
- This file was produced in Jupyter notebook with the `common-lisp-jupyter` kernel
