#lang sicp

; Dependencies

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symb tree)
    (cond
      ((or (null? tree) (leaf? tree)) '())
      ((not (element-of-set? symb (symbols tree)))
           (error "symbol not in tree"))
      ((element-of-set? symb (symbols (left-branch tree)))
       (cons 0 (encode-symbol symb (left-branch tree))))
      ((element-of-set? symb (symbols (right-branch tree)))
       (cons 1 (encode-symbol symb (right-branch tree))))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      
      (let ((smaller (car leaves))
            (small (cadr leaves)))
        (successive-merge
         (adjoin-set
          (make-code-tree small smaller)
          (cddr leaves))))))

; Answer

(define rock-pairs '((a 2) (boom 1) (Get 2) (job 2) (na 16)
                           (Sha 3) (yip 9) (Wah 1)))

(define rock-tree (generate-huffman-tree rock-pairs))

(define rock-msg
  '(Get a job
Sha na na na na na na na na

Get a job
Sha na na na na na na na na

Wah yip yip yip yip 
yip yip yip yip yip
Sha boom))

(define coded-msg (encode rock-msg rock-tree))

; Variable-Length Code Bits:
; (length coded-msg) = 84, so 84 bits for encoding

; Fixed-Length Code Bits:
; log2 8 = 3, so 3 bits per symbol.
; (length rock-msg) = 36
; 36 * 3 = 108 bits for encoding






