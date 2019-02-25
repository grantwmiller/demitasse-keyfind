




;****************************************************************************************************
;                                                                                                   *
; KEYFIND.S RESURRECTED                                                                             *
;                                                                                                   *
; (C) Copyright Sunday 24-FEB-2019 Grant W. Miller.   portions borrowed (it was online so...)       *
; from Harvard Professor Mighty Matt, especially call to urandom which is CRITICAL!                 *
;                                                                                                   *
;                                                                                                   *
;****************************************************************************************************

(define keysfilename "desktop/demitasse-keyset.txt");name of output
(define keysfile 1)
(display "deleting unsaved key set if any")(newline)
(if (file-exists? keysfilename)
  (begin
  (delete-file keysfilename)
  (set! keysfile (open-output-file keysfilename))
  )
  (begin
  (set! keysfile (open-output-file keysfilename))
  (display "none found…")

))




     (define (count-the-bits n)
        (define (count-bits n)
         (if (>= n 1)
           (begin
              (+ 1 (count-bits ( - (/ n 2) 0.5)))
           );end
           (if (= n 0) 0 (count-bits (/ n 2)))
         );endif
       );close define
     (count-bits (+ n (expt 10 -100))));close computation of key size in bits
(newline)


; jacobi: computes the Jacobi symbol, an extension of the Legendre symbol.
; straight out of RSAPAPER.PDF

(define (jacobi a n)
(let ((result 0)(times-called 0))
(define (m1to p) (if (even? p) 1 -1))
(define (square x)(* x x))
(define (j a b)
     (cond ((= a 1) 1)
           ((even? a) (* (m1to (quotient (- (square b) 1) 8)) (j (quotient a 2) b)))
           (else      (* (m1to (* (- a 1) (quotient (* (- b 1) (- a 1)) 4 ))) (j (modulo b a) a)))
      )
)



   (if (and (and (= (gcd a n) 1) (< a n)) odd? a)
    (j a n)
    0
    )

 )

)

    



;; Random number utilities.

(define bc 0)

(define (random-char) 

  (call-with-input-file "/dev/urandom"

    (lambda (port)

     (begin (set! bc (+ bc 1))(read-char port)))))



(define (random-num)

  (let ((n (char->integer (random-char))))

    (if (= n 65533)

        (random-num)

        n)))



(define (random-bit) (modulo (random-num) 2))



(define (random-byte) (+ (modulo (random-num) 128) (* 128 (random-bit))))



(define (random bytes)

  (if (<= bytes 0)

      0

      (+ (* 256 (random (- bytes 1))) (random-byte))))



(begin

(define (square x) (* x x))

(define (modulo-power base exp n)

  (if (= exp 0)

      1

      (if (odd? exp)

          (modulo (* base (modulo-power base (- exp 1) n)) n)

          (modulo (square (modulo-power base (/ exp 2) n)) n))))





)





; is-solovay-strassen-prime?: 

; Check for many values of a:

;  jacobi(a,n) = a^((n - 1)/2) [mod n] ?

;  If yes, then prime with probability (at least) 1/2.

;  If no, then composite.

; Probability of false positive is lower than 1/2^iterations.

(define (is-solovay-strassen-prime? n iterations)


(cond 

    ((<= iterations 0) #t)

    ((and (even? n) (not (= n 2))) #f)

    (else (let* ((byte-size     (count-the-bits n)      )

                 (a (+ 1 (modulo (random byte-size) (- n 1)))))

            (let* ((jacobi-a-n (jacobi a n))

                   (exp (modulo-power a (/ (- n 1) 2) n)))


              (if (and (not (= jacobi-a-n 0))(not (= (modulo jacobi-a-n n) exp)))
                (if (= jacobi-a-n 0) (is-solovay-strassen-prime? n iterations))
              )

                   #f 

(is-solovay-strassen-prime? n (- iterations 1)))))))   



      

;; Prime generation.



; generate-solovay-strassen-prime(byte-size, iterations) 

;  yields a prime of 'byte-size' bytes with a probability of 1-1/2^iterations.

(define (generate-solovay-strassen-prime byte-size iterations)

  (let ((n (random byte-size)))

    (if (is-solovay-strassen-prime? n iterations)

     n

     (generate-solovay-strassen-prime byte-size iterations))))





; generate-s-s-prime(byte-size, iterations, floorx);specifically for finding D
;  yields a prime of 'byte-size' bytes with a probability of 1-1/2^iterations.

(define (generate-s-s-prime byte-size iterations floorx)

  (let ((n (random byte-size)))
    (if (> n floorx)
             
        
        (if (is-solovay-strassen-prime? n iterations)

          n
         
          (generate-s-s-prime byte-size iterations floorx)
        );endif
     ;else
       (generate-s-s-prime byte-size iterations floorx)
    
    );endif
   );close let
 );close define

(define (totient p q) (* (- p 1)(- q 1)))

(define truekeysize 0)

(define the-totient 0)

(define max-p-q 0)



(define (fudge size) 

(let ((result (- size 0));don’t know why but this helps accuracy of size?! NO!

      (x 0)

(factor (modulo (random-num) 4)));close let body 
; 2 least significant bits random

; drawn from a large enough set that the cryptanalyst can’t find

; it by direct search! and p and q should differ by several digits



(begin

;(display size )(newline)

;(display factor )(newline)

(set! x (+ (* (truncate (/ result 4)) 4) factor))

;(display x)(newline)

x
);end
);close let inside fudge
);close fudge





(define KEYNUM1 1)


(define (find-keysets niterations niterations1 howmany)

(let (

(p 0);not even remembered—therefore we can make them differ in length

(q 0);not even remembered by simply using fewer digits in p, more in q!

(n 0);place in pubic file

(d 0);place in a SAFE PLACE

(e 0);the encryption key goes in the PUBLIC FILE

(err #f)
(small 20);for GCD test on p-1 and q-1
(afewdigits 7)

(p-and-q-test-result #f))

;*******************************************************************************
; START OF DEFINITIONS USED DIRECTLY IN THE MAIN PROGRAM IN SCOPE OF THE LET   *
;*******************************************************************************

(define (sizeof n )(ceiling (/ (count-the-bits n)(log 10))));count the digits

(define (find-p nbytes niterations ppp err)
(if (not err)
(let ((ppp (generate-solovay-strassen-prime nbytes niterations)) );close let body
(begin
  (display "p found. digits=")(display (sizeof ppp))(newline) ppp
));end and close let
);endif
);close define


(define (find-q nbytes niterations qqq err)
(if (not err)
(let ((qqq (generate-solovay-strassen-prime nbytes niterations)) );close let body
(begin
  (display "q found. digits=")(display (sizeof qqq))(newline) qqq
));end and close let
);endif
);close define


(define (p-and-q-test p q small afewdigits)


(if (or(<= (abs (- (sizeof p) (sizeof q))) afewdigits)
       (< small (gcd (- p 1)(- q 1)))
    )
 (begin 
  (display "p and q were too close or GCD test was not small, retrying")(newline)
  (set! p (find-p (- (fudge (quotient keysize 4)) (fudge 4)) niterations p err))
  (set! q (find-q (+ (fudge (quotient keysize 4)) (fudge 4)) niterations q err))
  (if (not (p-and-q-test p q small afewdigits))
    (list p q)
  );endif
 );end
  #f
);endif
);end p-and-q-test


(define (multiply-and-test-n n p q truekeysize err)
  (define temp 0)


(if (not err)
(begin
  (set! n (* p q))

  (if (or (< n (max p q))

          (< (sizeof n) truekeysize) )

     (begin (display "size of n (")(display (sizeof n))(display" digits) was not suitable, recursing.")(newline);dontset! err #t)

(set! p (find-p (- (fudge (quotient keysize 4)) (fudge 4)) niterations err))
(set! q (find-q (+ (fudge (quotient keysize 4)) (fudge 4)) niterations err))
(set! temp (test-p-and-q p q afewdigits))
(if temp (begin (set! p (car temp))(set! q (cdr temp))))
(multiply-and-test-n n p q truekeysize err)


     );end
;else

     (begin (display "n computed. digits=")(display (sizeof n))(newline)
        (set! the-totient (totient p q))
     );end
  );endif
));close begin,if
n
);close define




(define (find-d nbytes niterations p q n err)
 

(if (not err)
 (begin;this
  (let ((ddd (generate-s-s-prime nbytes niterations (max p q))));close let body
    (begin
      (if (and (= (gcd ddd (totient p q)) 1)
               (> ddd (max p q))           );close let body

                   (begin (display "d found. digits=")(display (sizeof ddd))(newline) ddd)
           ;else
                (begin
                    (if ( not (<= (gcd ddd (totient p q)) 1 ))
(begin (display "GCD on D not unity, got " )(display (gcd ddd (totient p q)))
(newline))
                   ;else
            (if (< ddd (max p q))        

(begin (display "candidate d was less than max(p,q)")(newline))
                   )
                    )
                   (find-d nbytes niterations p q n err)
                 );end

      );end if

    );end
   );close let

   );end;this
  );endif err

);close define find-d


(define (compute-inverse d e err)


(define (inverse x m)
  (let loop ((x x) (b m) (a 0) (u 1))
  (if (zero? x)
    (if (= b 1) (modulo a m)
      (begin;error handler
      (display "could not compute multiplicative inverse...inverse must be coprime increase certainty level for d")(newline)
      (set! err #t)
      );end error handler
    )
    (if (not err)
      (let* ((q (quotient b x)))

      (loop (modulo b x) x u (- a (* u q)))))))
    );endif on not err-- don't loop
 (if (not err)
  (begin
  (set! e (inverse d the-totient))

  (display "e computed. digits=")(display(sizeof e))(newline) e
  );end
  ;else
  (begin(display "key genereation incomplete; this key set will be discarded")(newline) #f)
  );end if on err
);close define

(define (write-output n d e err KEYNUM1)

(if (not err)
(begin
(display ";key number: " keysfile)(display KEYNUM1 keysfile)(newline keysfile)(flush-output-port keysfile)
(display "n=" keysfile)(display n keysfile)(newline keysfile)(newline keysfile)

(flush-output-port keysfile)

(display "d=" keysfile)(display d keysfile)(newline keysfile)(newline keysfile)

(flush-output-port keysfile)
(display "e=" keysfile)(display e keysfile)(newline keysfile)

(newline keysfile)(newline keysfile)(flush-output-port keysfile)
);end
;else
(begin (newline keysfile)(display ";defective key# ")(display KEYNUM1 keysfile)(newline keysfile)(flush-output-port keysfile)
       (display "EXCEPTION: key# ")(display KEYNUM1)(display "was not written to keys file")(newline));end
);end if
);close define
(define (congruent-modulo a b n)(= (remainder a n)(remainder b n)))
;******************************************************************
;           END OF SUBROUTINES WITHIN LET                         *
;******************************************************************


(if (<= 1 howmany)

(begin

(newline)(display "generating KEY#")(display KEYNUM1)(newline)


  (set! p (find-p (- (fudge (quotient keysize 4)) (fudge 4)) niterations p err))
  (set! q (find-q (+ (fudge (quotient keysize 4)) (fudge 4)) niterations q err))
  (set! p-and-q-test-result (p-and-q-test p q small afewdigits))
  (if p-and-q-test-result (begin (set! p (car p-and-q—test-result))(set! q (cdr p-and-q-test-result))))
  (set! n (multiply-and-test-n n p q truekeysize err))
  (set! d (find-d (fudge (ceiling (/ keysize 1.67))) niterations p q n err))
  (set! e (compute-inverse d e err))
  (display "bytes of random used = ")(display bc)(set! bc 0)(newline)
  (write-output n d e err KEYNUM1)
  (display (congruent-modulo (* e d) 1 the-totient))(newline)
  (set! KEYNUM1 (+ KEYNUM1 1))

  (find-keysets niterations niterations1 (- howmany 1))



);end

);endif


);close let




;
);finished.


;forgot the main program

(begin

(define keysize 100)

(define iterations 0)
(define iterations1 0)
(define nkeyswanted 0)

(define conin (open-file-input-port "/dev/stdin" ));trouble is hardwiring this

;(define conout (open-output-file "/dev/console"));and this





(begin
(display "suggest 50-250 digits, 100 for 1 in 2^this error possibilities")(newline)
(display "if you want to mess with YPSILON heap use --heap-limit=1027 or whatever")(newline)(newline)
(display "what do you want for an APPROXIMATE MINIMUM key length in decimal digits? ")(newline)

(set! truekeysize (read))
(set! keysize (ceiling (* 0.68  truekeysize)));seems to work out ok but not for small numbers
                           ;^ this is a hangover from Mighty Matt Might's random routines
                           ;                         which I haven't messed with.  This and the
                           ;                         use of FUDGE is necessarry unless a major
                           ;                         rewrite is done.  I decline; it works.


(display "how many keys do you want?")(newline)

(set! nkeyswanted (read))

(display "what do you want for error probability (1 in 2^this?) for p and q?")(newline)
(set! iterations (read))

(display "what do you want for error probability (1 in 2^this) for finding d?")(newline)
(set! iterations1 (read))

(find-keysets iterations iterations1 nkeyswanted)(newline)
(close-output-port keysfile)
(display "PLEASE RENAME YOUR KEY SET BEFORE YOU RUN THIS PROGRAM AGAIN")(newline)
(display "key generation finished")(newline))

);END.
