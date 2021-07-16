#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index flag tt et queue) #:transparent)

(define (empty-counter index)
  (make-counter index #t 0 0 empty-queue))

(define (update-aux f counters index acc)
  (cond
    ((null? counters) acc)
    ((if (= (counter-index (car counters)) index) (update-aux f (cdr counters) index (append acc (list (f (car counters)))))
         (update-aux f (cdr counters) index (append acc (list (car counters))))))))

(define (update f counters index)
  (update-aux f counters index '()))

(define tt+
  (lambda (minutes)
    (lambda (C)
        (match C [(counter index flag tt et queue) (make-counter index flag (+ tt minutes) et queue)]))))

(define et+
  (lambda (minutes)
    (lambda (C)
        (match C [(counter index flag tt et queue) (make-counter index flag tt (+ et minutes) queue)]))))

(define (add-to-counter name items)     
  (λ (C)                              
   (if (queue-empty? (counter-queue C))
       (make-counter (counter-index C) (counter-flag C) (+ (counter-tt C) items) (+ (counter-et C) items) (enqueue (cons name items) (counter-queue C)))
       (make-counter (counter-index C) (counter-flag C) (+ (counter-tt C) items) (counter-et C) (enqueue (cons name items) (counter-queue C))))))

(define min-helper
 (lambda (f) 
    (lambda (counters)
      (foldl (lambda (e res) (if (and (and (counter-flag e) #t) (< (f e) (cdr res))) (cons (counter-index e) (f e)) res)) (cons 999999 999999) counters)
      )))

(define min-tt (min-helper counter-tt)) ; folosind funcția de mai sus
(define min-et (min-helper counter-et)) ; folosind funcția de mai sus

(define (remove-first-from-counter C)   ; testată de checker
  (if (> (+ (queue-size-l (counter-queue C)) (queue-size-r (counter-queue C))) 1)
  (make-counter (counter-index C) (counter-flag C) (- (counter-tt C) (counter-et C)) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C)))
  (make-counter (counter-index C) (counter-flag C) 0 0 empty-queue)))

(define (pass-time-through-counter minutes)
  (λ (C)
    (if (queue-empty? (counter-queue C))
        (if ( < minutes (counter-et C)) 
            (make-counter (counter-index C) (counter-flag C) (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-queue C))
            (make-counter (counter-index C) (counter-flag C) 0 0 (counter-queue C)))
       (make-counter (counter-index C) (counter-flag C) (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-queue C)) )))


  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
(define (sum-tt counters sum)
      (if (null? counters) sum (sum-tt (cdr counters) (+ sum (counter-tt (car counters))))))

(define (average-tt counters)
   (/ (sum-tt counters 0) (length counters)))

(define (open-counters counters)
    (filter (λ (C) (if (and (counter-flag C) #t) #t #f)) counters))

(define (counters-with-customers counters)
    (filter (λ (C) (if (queue-empty? (counter-queue C)) #f #t)) counters))

(define (queues-list counters)
    (foldr (lambda (C acc) (append (list(cons (counter-index C) (counter-queue C))) acc)) null counters))
    
(define (sort-index counters)
  (sort counters (λ (C1 C2) (if (< (counter-et C1) (counter-et C2)) #t #f))))

(define (update-after-x-minutes x)
  (λ (C) 
    (if (< x (counter-et C)) ((pass-time-through-counter x) C)
        (if (<= (+ (queue-size-l (counter-queue C)) (queue-size-r (counter-queue C))) 1)
            (remove-first-from-counter C)
            (if (< (- x (counter-et C)) (cdr (top (dequeue (counter-queue C)))))
                ((pass-time-through-counter (- x (counter-et C))) (remove-first-from-counter C))
                ((update-after-x-minutes (- x (counter-et C)))(remove-first-from-counter C))
            )))))

(define (counters-with-et-lower-than-minutes counters minutes)
  (sort (filter (λ (C) (if (and (<= (counter-et C) minutes) (not (queue-empty? (counter-queue C)))) #t #f)) counters) (λ (C1 C2) (if (< (counter-et C1) (counter-et C2)) #t #f))))

(define close-counter
  (λ (C) (match C [(counter index flag tt et queue) (make-counter index #f tt et queue)])))

(define (exits counters minutes acc)
  (if (null? counters) acc
      (if ( >= 1 ( + (queue-size-l (counter-queue (car counters))) (queue-size-r (counter-queue (car counters)))))
          (exits (cdr counters) minutes (append acc (list (cons (counter-index (car counters)) (car (top (counter-queue (car counters)))))))) 
          (if (< (- minutes (counter-et (car counters))) (cdr (top (dequeue (counter-queue (car counters))))))
              (exits (cdr counters) minutes (append acc (list (cons (counter-index (car counters)) (car (top (counter-queue (car counters))))))))
              (exits (sort (sort-index (append (list (make-counter (counter-index (car counters)) (counter-flag (car counters)) (- (counter-tt (car counters)) minutes)
                                                                   (- (cdr (top (dequeue (counter-queue (car counters))))) (- (counter-et (car counters)) minutes))
                                                 (dequeue (counter-queue (car counters))))) (cdr counters))) (λ (C1 C2) (if (< (counter-et C1) (counter-et C2)) #t #f)))
                     minutes (append acc (list (cons (counter-index (car counters)) (car (top (counter-queue (car counters)))))))))                                                                                                  
      )))

(define (serve-helper requests fast-counters slow-counters iesiri)

  (define (check-n-items n-items)
    (if (<= n-items ITEMS) (append fast-counters slow-counters) slow-counters))
  
  (if (null? requests)
      (cons  iesiri (queues-list (counters-with-customers (append fast-counters slow-counters))))
      (match (car requests)
        
         [(list 'close index)
         (serve-helper (cdr requests) (update close-counter fast-counters index) (update close-counter slow-counters index) iesiri)]
        
       [(list 'ensure average)
           (if (> (average-tt (open-counters (append fast-counters slow-counters))) average)
              (serve-helper requests fast-counters (append slow-counters (list (empty-counter (+ (counter-index (last slow-counters)) 1)))) iesiri)
               (serve-helper (cdr requests) fast-counters slow-counters iesiri))]
        
       [(list name n-items)
        (serve-helper (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (check-n-items n-items))))
                                       (update (add-to-counter name n-items) slow-counters (car (min-tt (check-n-items n-items)))) iesiri)]

       [(list 'delay index minutes)
        (serve-helper (cdr requests) (update (et+ minutes) (update (tt+ minutes) fast-counters index) index)
                                            (update (et+ minutes) (update (tt+ minutes) slow-counters index) index) iesiri)]
       [x
        (serve-helper (cdr requests) (map (update-after-x-minutes x) fast-counters)
                      (map (update-after-x-minutes x) slow-counters)
                      (append iesiri (exits (counters-with-et-lower-than-minutes (append fast-counters slow-counters) x) x '()))) ]

        )
      )
  )

(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))


