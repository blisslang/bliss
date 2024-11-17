;----- Bliss stdlib -----;
(mod Bliss
	(mod IO
		(def puts [str]
		  (print-endline str)))

	(mod String
		(def of-float! [str]
			(string-of-float str)))

	(mod Math
		(def-rec fib [n]
		  (if (< n 2)
			  n
			  (+ (fib (- n 1))
				   (fib (- n 2)))))))
;----- Bliss stdlib -----;

(use Bliss)

(def main
	(IO/puts (String/of-float! (Math/fib 8))))
