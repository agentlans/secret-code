#|
    secret-code : a Common Lisp package for making and breaking simple ciphers
    Copyright (C) 2019  Alan Tseng

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>. |#


(defpackage :secret-code
  (:use :common-lisp)
  (:export :char->code :code->char :string->code :code->string
:caesar-encode :caesar-decode :caesar-crack
:repeating-caesar-encode :repeating-caesar-decode
:repeating-caesar-crack))

(defun ngrams (str &optional (n 1))
  "Returns a list of ngrams of length n in str."
  (loop for i from 0 to (- (length str) n)
     collect (subseq str i (+ i n))))

(defun rotate-string (str n)
  "Shifts string n characters to the left.
The characters on the left are recycled to the right of the result.
Negative values for n are allowed."
  (let* ((len (length str))
	 (n2 (mod n len)))
    (concatenate 'string
		 (subseq str n2 len)
		 (subseq str 0 n2))))

(defun tally (lst)
  "For each element in lst, count number of times they appear."
  (let ((keys
	 (remove-duplicates lst :test #'equalp)))
    (loop for k in keys
       collect (cons k (count k lst :test #'equalp)))))

(defun combine-alists (alist &rest other-alists)
  "Returns an association list containing the keys of the given
association lists and their respective values."
  (let* ((all-alists (append (list alist) other-alists))
	 (get-keys (lambda (x) (mapcar #'car x)))
	 ;; All the keys present in at least one association list.
	 (keys
	  (remove-duplicates
	   (apply #'append
		  (mapcar get-keys all-alists))
	   :test #'equalp))
	 ;; Turn NIL to 0
	 (nil->0 (lambda (x)
		   (if x x 0))))
    ;; For each key, return the values of corresponding to each key
    ;; in the association lists.
    (loop for k in keys
       collect (cons k (loop for a in all-alists collect
			    (funcall nil->0
				     (cdr (assoc k a :test #'equalp))))))))

(defun normalize (num-lst)
  "Normalize the counts to their respective probabilities."
  (let ((sum (loop for i in num-lst sum i)))
    (mapcar #'(lambda (x) (/ x sum)) num-lst)))

(defun entropy (alist)
  "Returns the entropy based on counts in association list."
  (- (loop for p in 
	  (normalize (mapcar #'cdr alist))
	sum (* p (log p)))))
;; Example: can use this to
;; test whether given collection of letters has
;; a similar distribution to the English language

(defun kl-divergence (alist-p alist-q)
  "Returns Kullback-Leibler divergence KL(P || Q) based on counts in two
association lists."
  (let* ((combined (combine-alists alist-p alist-q))
	 (p (normalize (mapcar #'cadr combined)))
	 (q (normalize (mapcar #'caddr combined))))
    (- (loop for px in p
	  for qx in q
	  sum (if (equalp px 0) 0
		  (* px (- (log qx) (log px))))))))

(defun char->code (x)
  "Returns letter code of character x where the
code of A = 0 and Z = 25."
  (- (char-code x) 65))

(defun code->char (x)
  "Returns letter corresponding to code number x
where 0 -> A and 25 -> Z."
  (code-char (+ 65 x)))

(defun string->code (str)
  "Returns the codes for characters in the string."
  (loop for c across str collect
       (char->code c)))
;; Example:
;; (string->code "ABC")

(defun code->string (codes)
  "Returns the string from given codes.
Note: A->0 and Z->25"
  (concatenate 'string
	       (loop for x in codes collect
		    (code->char x))))
;; Example:
;; (code->string '(0 1 2 3))

(defun remove-non-alpha-characters (str)
  "Converts str to uppercase and removes
any characters that aren't from A-Z."
  (concatenate
   'string
   (loop for c across (string-upcase str)
      when (and (char>= c #\A) (char<= c #\Z))
      collect c)))

(defun caesar-encode (str shift)
  "Encodes a string using the Caesar cipher
with specified left shift. That is, if shift = 3, then A encoded to D."
  (concatenate
   'string
   (loop for c across (remove-non-alpha-characters str)
      collect (code->char
	       (mod (+ (char->code c) shift) 26)))))

(defun caesar-decode (str shift)
  "Decodes a string encoded by Caesar cipher
with specified left shift."
  (caesar-encode (remove-non-alpha-characters str)
		 (- shift)))

;; Letter frequencies in English
(defparameter *letter-freq*
  (car (with-open-file (stream "english_monograms.lisp")
    (loop for line = (read stream nil)
       while line collect line))))

(defun min-element (alist)
  "Returns the entry in association list with the smallest datum."
  (let ((min-y (apply #'min (mapcar #'cdr alist))))
    (rassoc min-y alist)))

(defun caesar-crack (str)
  "Tries to crack Caesar cipher by trying all shifts
and finding the one that minimizes KL divergence
with English language text."
  (let ((best-key
	 (car (min-element
	       (loop for i from 0 to 25 collect
		    (let ((decoded (caesar-decode str i)))
		      (cons
		       i (kl-divergence (tally (ngrams decoded))
					*letter-freq*))))))))
    (caesar-decode str best-key)))
;; Example:
;; (caesar-crack "DYLOYBXYDDYLODRKDSCDROAEOCDSYX")

(defun break-into-groups (str n)
  "Breaks string into groups n letters long."
  (loop for i from 0 to (/ (- (length str) 1) n)
     collect 
       (let ((start (* i n)))
	 (subseq str start
		 (min (+ start n)
		      (length str))))))

(defun repeat-string (str n)
  "Repeats a string n times and returns result
as one string."
  (apply #'concatenate 'string
	       (loop for i from 1 to n
		  collect str)))

(defun pad-strings (lst)
  "Adds trailing spaces to strings in lst so they all
have the same length."
  (let ((max-len (apply #'max (mapcar #'length lst)))
	(add-spaces
	 (lambda (x n)
	   (concatenate 'string x
		  (repeat-string " " n)))))
    (loop for x in lst
       collect (funcall add-spaces
			x (- max-len (length x))))))

(defun transpose (lst)
  "Given a list of strings for each row, arranges
the characters into a matrix and reads off the columns
from left to right."
  (let* ((padded (pad-strings lst))
	 (cols (length (car padded))))
    (loop for i from 0 to (- cols 1)
       collect
	 (concatenate 'string
		      (loop for s in padded
			 collect (char s i))))))
;; Example:
;; (transpose (break-into-groups "012345" 3))
;; The above breaks string into 2 groups of 3 characters,
;; which is transposed to get 3 groups of 2 characters.

(defun call-for-string-and-key (str key f)
  "Calls f(a, b) where a is character from the given string and
b is character from the key which is recycled along the length of str.
Returns a string from calling f(a, b)."
  (let ((key-len (length key)))
    (apply #'concatenate 'string
	   (loop for i from 0 to (- (length str) 1)
	      for a across str
	      collect (let ((b (nth (mod i key-len) key)))
			(funcall f (string a) b))))))

(defun repeating-caesar-encode (str shift-lst)
  "Encodes string using the shifts in shift-lst.
If string is longer than shift-lst, elements of shift-lst will
be recycled."
  (call-for-string-and-key
   str shift-lst #'caesar-encode))
;; Example:
;; (repeating-caesar-encode "ABCDEFGHI" '(0 1 2))

(defun repeating-caesar-decode (str shift-lst)
  "Decodes string using the shifts in shift-lst.
If string is longer than shift-lst, elements of shift-lst
will be recycled."
  (call-for-string-and-key
   str shift-lst #'caesar-decode))
;; Example:
;;(repeating-caesar-decode
;; (repeating-caesar-encode "ABCDEFGHI" '(0 1 2)) '(0 1 2))

(defun repeating-caesar-crack (str key-length)
  "Tries to crack Caesar cipher where the shifts repeat
along the string. For example: 0, 1, 2, 0, 1, 2, ...
Basically, several Caesar ciphers together
but you have to guess key-length first."
  (remove-non-alpha-characters
   (apply #'concatenate 'string
	  (transpose
	   (mapcar #'caesar-crack
		   (transpose
		    (break-into-groups str key-length)))))))

