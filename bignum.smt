;; Starter code for SmallTalk assignement.
;; Author: Richard Townsend 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;             Exercise 1 classes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(class Natural
   [subclass-of Magnitude]

   (class-method fromSmall: (n)
       ((n = 0) ifTrue:ifFalse:
         {(NatZero new)}
         {(NatNonzero first:rest: (n mod: (self base)) (self fromSmall: (n div: (self base))))  }
       )
   )
   (class-method base () 10) ; private

   ; private methods suggested from textbook (page 681)
   (method modBase () (self subclassResponsibility)) 
   (method divBase () (self subclassResponsibility)) 
   (method timesBase () (self subclassResponsibility)) 
   (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock) 
      (self subclassResponsibility)) 
   (method plus:carry: (aNatural c) (self subclassResponsibility)) 
   (method minus:borrow: (aNatural c) (self subclassResponsibility)) 

   (method timesDigit:plus: (d r) (self subclassResponsibility)) ; private

   (method = (aNatural) (self leftAsExercise))
   (method < (aNatural) (self leftAsExercise))

   (method + (aNatural) (self leftAsExercise))
   (method * (aNatural) (self subclassResponsibility))
   (method subtract:withDifference:ifNegative: (aNatural diffBlock exnBlock)
      (self leftAsExercise))

   (method sdivmod:with: (n aBlock) (self subclassResponsibility))

   (method decimal () (self leftAsExercise))
   (method isZero  () (self subclassResponsibility))

   ; methods that are already implemented for you
   (method - (aNatural)
      (self subtract:withDifference:ifNegative:
            aNatural
            [block (x) x]
            {(self error: 'Natural-subtraction-went-negative)}))
   (method sdiv: (n) (self sdivmod:with: n [block (q r) q]))
   (method smod: (n) (self sdivmod:with: n [block (q r) r]))
   (method print () ((self decimal) do: [block (x) (x print)]))

   ;private methods for testing
   (method validated ()
    ((self invariant) ifFalse:
      {(self printrep)
       (self error: 'invariant-violation)})
    self)

   (method compare-symbol: (aNat)
    (self compare:withLt:withEq:withGt: aNat {'LT} {'EQ} {'GT}))

   (class-method first:rest: (anInteger aNatural)
      (((anInteger = 0) & (aNatural isZero )) ifTrue:ifFalse:
         {(NatZero new)}
         {((NatNonzero new) first:rest:) anInteger aNatural )}
      )
   )

)

; Represents a 0 natural number
(class NatZero
  [subclass-of Natural]
  (method invariant () true) ;; private
  (method isZero () true)
  (method timesDigit:plus: (d r) (Natural fromSmall: r)) ; private

  ;; for debugging
  (method printrep () (0 print))
)

; Represents a natural number greater than 0
(class NatNonzero
  [subclass-of Natural]
  [ivars m d] ; a non-zero natural number is of the form d + m * b, where d
              ; is an integer representing a digit of base b, and m is a natural
              ; number
  (method isZero () false)
  (method invariant () (((d < (Natural base)) & (d >= 0)) &  ;; private
                       (((m isZero) & (d = 0)) not)))

  ; addition with a carry bit
  (method plus:carry: (aNatural c) [locals sum least cout]
     (set sum ((d + (aNatural modBase)) + c))
     (set least (sum mod: (Natural base)))
     (set cout  (sum div: (Natural base)))
     (NatNonzero first:rest: least (m plus:carry: (aNatural divBase) cout)))
      
  ; subtraction with a borrow bit
  (method minus:borrow: (aNatural b) [locals diff least bout]
     (set diff (d - ((aNatural modBase) + b)))
     ((diff < 0) ifTrue:ifFalse:
         {(set diff (diff + (Natural base)))
          (set bout 1)}
         {(set bout 0)})
     (NatNonzero first:rest: diff (m minus:borrow: (aNatural divBase) bout)))

  ; multiplication
  (method * (aNatural) [locals d1 d2 m1 m2]
     ; simple method; fastest; based on this law:
     ;   (d + b * m) * n == (d * n) + b * (m * n)
     ((aNatural timesDigit:plus: d 0) + ((m * aNatural) timesBase)))

  (method timesDigit:plus: (dig r) ; private, answers self * d + r
      [locals pp]
      (set pp ((d * dig) + r))
      (NatNonzero first:rest: (pp mod: (Natural base))
                  (m timesDigit:plus: dig (pp div: (Natural base)))))
  
  ;; debugging method
  (method printrep () (m printrep) (', print) (d print))

  (class-method first:rest: (anInteger aNatural)
      (set m aNatural) (set d anInteger) self)
  

)

; For testing naturals
(class DebugNat
  [subclass-of Object]
  [ivars nat] ; a natural number
  (class-method of: (aNat) ((self new) init: aNat))
  (method init: (n) (set nat n) self) ; private
  (method print () (nat printrep))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put your unit tests for Exercise 1 here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-assert ((Natural fromSmall: 0) isZero) )
(Natural fromSmall: 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;             Exercise 2 classes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class LargeInteger
  [subclass-of Integer]
  [ivars magnitude]

  (class-method withMagnitude: (aNatural)
      ((self new) magnitude: aNatural))
  (method magnitude: (aNatural) ; private, for initialization
      (set magnitude aNatural)
      self)

  (method magnitude () magnitude)

  (class-method fromSmall: (anInteger)
     ((anInteger isNegative) ifTrue:ifFalse: 
        {(((self fromSmall: 1) + (self fromSmall: ((anInteger + 1) negated)))
          negated)}
        {((LargePositiveInteger new) magnitude: 
                 (Natural fromSmall: anInteger))}))
  (method isZero () (magnitude isZero))
  (method = (anInteger) ((self - anInteger)     isZero))
  (method < (anInteger) ((self - anInteger) isNegative))

  (method div: (n) (self sdiv: n))
  (method mod: (n) (self smod: n))

  (method sdiv: (n) (self subclassResponsibility))
  (method smod: (n) (self - ((LargeInteger fromSmall: n) * (self sdiv: n))))
)

; Represents a positive integer
(class LargePositiveInteger
  [subclass-of LargeInteger]


  ;; short division (already implemented for you)
  (method sdiv: (anInteger)
    ((anInteger isStrictlyPositive) ifTrue:ifFalse: 
       {(LargePositiveInteger withMagnitude:  (magnitude sdiv: anInteger))}
       {((((self - (LargeInteger fromSmall: anInteger)) -
                                                  (LargeInteger fromSmall: 1))
             sdiv: (anInteger negated))
            negated)}))
)

;; Represents a negative integer
(class LargeNegativeInteger
  [subclass-of LargeInteger]

  ;; short division (already implemented for you)
  (method sdiv: (anInteger)
    ((self negated) sdiv: (anInteger negated)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put your unit tests for Exercise 2 here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
