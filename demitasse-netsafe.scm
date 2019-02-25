;**************************************************************************
;
; RSA implementation Grant W Miller (c) Copyright 1996-2019 Grant W. Miller
;
; This program was Copyrighted in the U.S.A. by Grant W. Miller
;     TXu691-537, July 1, 1996.
;
; The program and all its derivatives are covered by the completed version
;     Copyrighted in the U.S.A. by Grant W. Miller
;     TXu 1-950-897, January 12, 2015.
;
; ALL RIGHTS ARE RESERVED.
; Expressly NO permission is granted to use this program for any purpose
; whatsoever without express written permission of Grant W. Miller
;
;**************************************************************************
;
;
;
;
;
(define syskeyn
646858685376107603527224097154545447546013249617080147114204039589527767995025218761497419577195934820132182528676764967861808733018803014227093843168355079819078058075727892287307560046959504456904587915312154686808719942326625413385453302425246490762453647547506909185142060856269690209203758283750197260403583678728791457457683087044931679055223323947681660794837389018179633790272769602760536414326181766556028739607066154803428445004634639036499520298064151835364410716122222321800282303805996415990624060890955579172863996203099188996220313299437563880670059519244032252191282428784447768843563)
(define syskeyd
5366435365456359165467489122165129111225311230507358550330325286057929175932858686862913280195981860252088802052703128220840882612783872807763457674422980134572358864626082994335369669988457891153384880699862204817990867801937678736605531879051370489900638577482029133235117948257884917900843973225375624944165174357669974843817751316268812546631699682507524118995072375515043910366969141348716403054407054118070310968007105853411883978830596422232033881466851514515863505432318665754198406708519163871425745948881723894216922993872744494058538458337326747522375894488010506372903709473509206191)
(define syskeye
301654305889613567168268111964144897906122382250317747284234306710870263972276695045653566855941329244251154360961994233379936493863750387887605390727077469305385287267970038717864929486098915867829320185136642083200170158492467458965024441336900334969503144122429180957018071482706553700273591598467740454635304650957479986144148036254142656322243467690966663465042988853792322833745881473110879343054942592351044325429008790751291659763729279830740263508519571740811029383323068311007549351334988238654856330852628160770382607254427262401986707956667960636076110157082499905158248596015457583132511)



;

(define (encrypt efile ofile e n)

   (define (do-encryption-blocks)
     (let((number-of-blocks 0)
          (number-of-bits-exactly 0)
          (number-of-bits-in-n 0)
          (number-of-loops-per-block 0)
          (number-of-chars-in-last-block 0)
          (accumulator 0)
          (ch "")
          (eofpresent1 #f)
          (nbytes 1)
          (eport '())
          (oport '())
        );close let body
     (define (read-each-char aport);get size of file in bytes
         (if (not (port-eof? aport))
            (begin
            (set! ch (get-u8 aport))
            (set! nbytes (+ 1 nbytes))
            (read-each-char aport)
            );end
        ;else
          nbytes
        );endif
    );close define read-each-char
     (define (keysize-in-bits n)
        (define (count-bits n)
         (if (>= n 1)
           (begin
              (+ 1 (count-bits ( - (/ n 2) 0.5)))
           );end
           (if (= n 0) 0 (count-bits (/ n 2)))
         );endif
       );close define
     (count-bits (+ n (expt 10 -100))));close computation of key size in bits
  (define (expmod b ee m)
       (define (square x) (* x x))
  (cond((= ee 0) 1)
          ((even? ee)(remainder(square(expmod b (/ ee 2) m)) m))
          ((not (even? ee))(remainder(* b (expmod b (- ee 1) m))  m))
  );close cond
  );close define expmod
(define (convert-to-number s); move right to left across the string
(let((length (string-length s))
     (mult 1)
     (result 0))
  (if (= 0 length) (set! result 0)
  (begin
    (do ((i 0 (+ 1 i)))
       ((= i length) i)
       (begin
         (set! result (+ result (* mult (char->integer(string-ref s i)))))
         (set! mult (* 256 mult))
       );end
     );end do
   );end
   );endif
  result
  ));close let, define


    (begin;{0}
 (begin
   (if (file-exists? efile)
     (set! eport (open-file-input-port efile));input is binary and uses set-position imperative
     ;else
     (error "encrypt::" "input file does not exist.")
   )
   (if (not (file-exists? ofile))
     (set! oport (open-output-file ofile));output is all integer and textual
     ;else
     (error "encrypt::" "output file already exists.");output is textual
   )
  );end





      (set-port-position! eport 0)
      (set! number-of-bits-exactly (* 8 (read-each-char eport)))
      (set-port-position! eport 0)
      (set! number-of-bits-in-n (keysize-in-bits n))
      (set! number-of-loops-per-block (floor (/ number-of-bits-in-n 8)))
      (if (not (= 0 number-of-loops-per-block))
        (set! number-of-blocks (inexact->exact (quotient number-of-bits-exactly (* 8 number-of-loops-per-block))))
        (set! number-of-blocks 0)
      );end if
      (set! number-of-chars-in-last-block (- nbytes (* number-of-blocks number-of-loops-per-block)))
      (if (not (= 0 number-of-chars-in-last-block)) (set! number-of-blocks (+ 1 number-of-blocks)))
      (write (expmod (+ 4 number-of-blocks) syskeye syskeyn) oport)
      (newline oport)
      (write (expmod (+ 4 number-of-loops-per-block) syskeye syskeyn) oport)
      (newline oport)
      (write (expmod  (+ 4 number-of-chars-in-last-block) syskeye syskeyn) oport)
      (newline oport)
      (write (expmod(+ 4 (convert-to-number user-full-name)) syskeye syskeyn) oport)
      (newline oport)
      (write (expmod (+ 4 (convert-to-number user-email-1)) syskeye syskeyn) oport)
      (newline oport)
      (write (expmod (+ 4 (convert-to-number user-email-2)) syskeye syskeyn) oport)
      (newline oport)
      (write (expmod (+ 4 (convert-to-number user-email-3)) syskeye syskeyn) oport)
      (newline oport)
    );end{0}

(begin;{1}

  (do((i 1 (+ 1 i)));{start do on i}
        ((> i number-of-blocks) i)
        (begin;{2}
        (do ((j 1 (+ 1 j)));{start do on j}
               ((> j number-of-loops-per-block) j)

               (begin;{3}
               (if (not eofpresent1)
                  (set! ch (get-u8 eport))
                )
               (if (or eofpresent1 (eof-object? ch))
                 (begin
                  (set! eofpresent1 #t)
                  (set! ch 0)
                 );end
               );end if
               (begin;{4}
                 (set! accumulator (inexact->exact
                          (* 256 accumulator)));shift left
                 (set! accumulator
                   (+ accumulator
                      (if eofpresent1 0 ch));close add and if

                 );close set
                 
                 );end{3}
                );end{4}
       );end do on j
       (begin
         (display(expmod accumulator e n) oport )
         (newline oport)
         (set! accumulator 0)
       );end
       );end{2}
   );end do on i
(close-output-port oport);this should work
(close-input-port eport)
);END{1}
);close let
);close define do-encryption-blocks
(define (main-loop)
   (do-encryption-blocks)
);close main-loop define
(main-loop)
#t
);close define

;***********************************************************************
(define (decrypt ifile ofile d n)
  (let*((number-of-blocks 0)
         (number-of-loops-per-block 0)
         (number-of-chars-in-last-block 0)
         (sender-full-name "")
         (sender-email-1 "")
         (sender-email-2 "")
         (sender-email-3 "")
         (iport '())
         (oport '())
       );close let* body

(define (do-decryption-blocks)
  (let((accumulator 0)
          (chx #\space)
          (shifter 0)
          (k 0)
          (eofpresent #f))

  (define (expmod b ee m)
       (define (square x) (* x x))
  (cond((= ee 0) 1)
          ((even? ee)(remainder(square(expmod b (/ ee 2) m)) m))
          ((not (even? ee))(remainder(* b (expmod b (- ee 1) m))  m))
  );close cond
  );close define
(define (convert-to-string n)
(let((result "")
     (mult 1))
  (begin
    (do ((i mult (+ 0 mult)))
       ((= n 0) n); a bit inelegant but...
       (begin
         (set! result (string-append result (make-string 1 (integer->char (remainder n 256)))))
         (set! n (inexact->exact(quotient n 256)))
       );end
     );end do
   );end
   result
  ));close let, define

(if (file-exists? ifile)(set! iport (open-input-file ifile)); input is textual and all integer numeric
    ;else
    (error "decrypt::" "input file does not exist."))
(if (not(file-exists? ofile))(set! oport (open-file-output-port ofile))
    ;else
    (error "decrypt::" "output file exists, not overwriting!"))
(set! number-of-blocks (- (expmod (read iport) syskeyd syskeyn) 4))
(set! number-of-loops-per-block ( - (expmod (read iport) syskeyd syskeyn) 4))
(set! number-of-chars-in-last-block
             (- (expmod (read iport) syskeyd syskeyn) 4))

(set! sender-full-name (convert-to-string
           (- (expmod (read iport) syskeyd syskeyn) 4)
                        )
)
;

(set! sender-email-1 (convert-to-string
          (- (expmod (read iport) syskeyd syskeyn) 4)
                     );close convert to string
)

(set! sender-email-2 (convert-to-string
          (- (expmod (read iport) syskeyd syskeyn) 4)
                     );close convert to string
)
(set! sender-email-3 (convert-to-string
          (- (expmod (read iport) syskeyd syskeyn) 4)
                     );close convert to string
)

; DO THE JOB
;
(do  ((i 1 (+ 1 i)));{start of do on i}
        ((> i number-of-blocks) i)
        (begin;{1}
            (set! shifter (inexact->exact
                             (expt 2 (* 8
                                (- number-of-loops-per-block
                                1))
                                         )))
          ;  (if (not( port-eof? iport));this should have been handled
                                       ;by encrypt!
                 (set! accumulator (read iport))
                 ;else -debugging Thursday for signing
                ; 0) ; read a block
            (set! accumulator (expmod accumulator d n))
            (do ((j 1 (+ 1 j)));{ do on j}
                   ((> j number-of-loops-per-block) j)
                      (begin;{2}
                         (set! chx (inexact->exact(quotient
                                                       accumulator
                                                            shifter)))
                         (if (not (= i number-of-blocks)) ;if not lastblock
                             (put-u8 oport chx)
                         ;else
                         (begin;{3}
                         (if (< k (- number-of-chars-in-last-block 1))
                            (begin
                            (put-u8 oport chx)
                            (set! k (+ 1 k))
                            )
           ; (close-output-port oport);it don't belong here
                          );end if
                        );end{3}
                        );end if
                        (set! accumulator (remainder accumulator shifter))
                        (set! shifter (quotient shifter 256))
                    );end{2}
             );end do on j
        );END{1}
      );end do on i
(newline)
(display "*************************** HEADER INFORMATON *************************")(newline)
(display "sender full name was=")(display sender-full-name)(newline)
(display "sender email-1 was=")(display sender-email-1)(newline)
(display "sender email-2 was=")(display sender-email-2)(newline)
(display "sender email-3 was=")(display sender-email-3)(newline)
(display "************************ END HEADER INFORMATON *************************")(newline)

(close-output-port oport)(close-input-port iport)
   );close let after define of do-decryption blocks
 );close define of do-decryption-blocks

(define (main-loop)
   (begin
   (do-decryption-blocks)
   )
); close define
(main-loop)
#t
));close let*, define

;************************************************************************************************************************************************************************

(define user-signature-n
33132907948393758958008011051678110570876791293674391431134786540778738716010312510615350051683721670954127559332524154262489978352925900477427589090961484071135527160495977810485664576791589374447713748657153202873092834257567583215364331560488427705408437215820682745671149839045055642980164467311)

(define user-signature-d
475264850293875136549526423471759270630664783823613442398427725955111778320331023223852283891221640580902640046972148142276400766464136897802416653280340439304867963914232311730381246530628412277730373795018705443953492419772545071212828658154620164146004321121612713263189752243081923687403054762353)

(define user-signature-e
24274468319353020253994696010845531924745433380243212843074280717502217813184792517033113965375172399720960383214167756476144448354803666460961235241029943487345967471053635638736395183551259912710451961199619519752063809987479125991881310299424468242264872651608182557447549334601283718390669143753)

(define user-encryption-n
5032609661181007613543727982123828713130776792818214670512463294305709267326084647584105032997452985924490604349936675059422928088615554361967523643700868189954905275155717433655830699753137573935837638923575219127790790014046583953264957209322062668072751404006934068511758994706100810934304504149)

(define user-encryption-d
7990656577977657587257054442193850149490800755792670303859128758245673383700509268651022235234744025037970405555316435897404530969171100747385574852761614533495909782823133181232555388115605054477045803552980443254569751794715296381486252127907356306867374250137887080264225173864618490387042281398103)

(define user-encryption-e
4715082881528495266245943490523983331198074209091471074156111813694422961351837192278296614349636033348546973689003132766256587137695611535813679655379689808407392230269725105704121534217020082358013003282163195596862860501697080884116714939407843407893809178361980533306509116677729613280323151207)

(define user-full-name "beta tester");put your actual name here.
(define user-email-1 "")
(define user-email-2 "")
(define user-email-3 "")
;************************************************************************************************************************************************************************
(define conin (open-input-file "/dev/stdin"))
(define conout(open-output-file "/dev/stdout"))
(define file-name-to-encrypt "")
(define mode "X")

(begin; BEGIN
  (begin
  (display "enter uppercase E to sign and encrypt, or uppercase D to unsign and decrypt")(newline)
  (flush-output-port conout)
  (set! mode (symbol->string(read conin)))(flush-output-port conout)
  (display "enter filename to encrypt or decrypt...DO NOT SPECIFY.rsa for decryption and prepend the path name in any case eg desktop/rsapaper.pdf?")(newline)
  (flush-output-port conout)  (set! file-name-to-encrypt (symbol->string(read conin)))
  );end
  (begin;{1}
   (if (or (string=? mode "E")(string=? mode "D"))
    (if (string=? mode "E")
      (begin;{encrypt}
      (display "signing.")(newline)
      (encrypt file-name-to-encrypt (string-append file-name-to-encrypt ".sig" ) user-signature-e user-signature-n);SIGN WITH USER E for signing
      (display "encrypting.")(newline)
      (encrypt (string-append file-name-to-encrypt ".sig")(string-append file-name-to-encrypt ".rsa") user-encryption-e user-encryption-n);encrypt with USER E encryption key
    );end;{encrypt}
        ;else
      (begin;{decrypt}
        (display "decrypting.")(newline)
        (decrypt (string-append file-name-to-encrypt ".rsa") (string-append file-name-to-encrypt ".decr") user-encryption-d user-encryption-n);unencrypt with USER D
        (display "unsigning.")(newline)
                (decrypt (string-append file-name-to-encrypt ".decr");input
          (string-append
            (string-append (substring file-name-to-encrypt
                            0
                            (- (string-length file-name-to-encrypt) 4);main
                        )
           ".clear"
            );close inner string append
           (substring file-name-to-encrypt
             (- (string-length file-name-to-encrypt) 4)
             (string-length file-name-to-encrypt);ext
           ))
       user-signature-d user-signature-n);unsign with SECRET D
     );{end decrypt}
         );{endif}
         ;else
   (display "either file does not exist or you have or not E or D specified, quitting, please try again.")

);end {1}
);end


   );END.
