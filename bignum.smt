;; Starter code for SmallTalk assignement.
;; Author: Richard Townsend 
;; Edited by: Caleb Pekowsky and Jacob Zimmerman

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
      {(NatNonzero first:rest: 
          (n mod: (self base))
          (self fromSmall: (n div: (self base))))}))

  (class-method first:rest: (anInteger aNatural)
    (((anInteger = 0) & (aNatural isZero )) ifTrue:ifFalse:
      {(NatZero new)}
      {((NatNonzero new) first:rest: anInteger aNatural)}))

  (class-method base () 32767) ; private

  ; private methods suggested from textbook (page 681)
  (method modBase () (self subclassResponsibility)) 
  (method divBase () (self subclassResponsibility)) 
  (method timesBase () (self subclassResponsibility)) 
  (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock) 
    (self subclassResponsibility)) 
  (method plus:carry: (aNatural c) (self subclassResponsibility)) 
  (method minus:borrow: (aNatural c) (self subclassResponsibility)) 

  (method timesDigit:plus: (d r) (self subclassResponsibility)) ; private

  (method = (aNatural) 
    (self compare:withLt:withEq:withGt:
      aNatural {false} {true} {false}))
  (method < (aNatural) 
    (self compare:withLt:withEq:withGt:
      aNatural {true} {false} {false}))

  (method + (aNatural) (self plus:carry: aNatural 0))
  (method * (aNatural) (self subclassResponsibility))
  (method subtract:withDifference:ifNegative: (aNatural diffBlock exnBlock)
    ((self < aNatural) ifTrue:ifFalse: 
      exnBlock 
      {(self minus:borrow: aNatural 0)}))

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

)

; Represents a 0 natural number
(class NatZero
  [subclass-of Natural]
  (method invariant () true) ;; private
  (method isZero () true)
  (method timesDigit:plus: (d r) (Natural fromSmall: r)) ; private
  (method divBase () self )
  (method modBase () 0 )
  (method timesBase () self )
  (method * (nat) self )
  (method sdivmod:with: (v K) ((v < 1) ifFalse:ifTrue: ;; TODO check divisors
    {(K value:value: self 0)}
    {(self error: 'Bad-divisor)}))
  (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock)
    ((aNatural isZero) ifTrue:ifFalse: {(eqBlock value)} {(ltBlock value)}))

  (method plus:carry: (aNatural c)
    ((aNatural isZero) ifTrue:ifFalse:
      {(Natural fromSmall: c)}
      {(aNatural + (Natural fromSmall: c))}))

  (method minus:borrow: (aNatural b) 
    (((aNatural isZero) & (b = 0)) ifTrue:ifFalse:
      {self}
      {(self error: 'subtraction-went-negative)}))

  ;; for debugging
  (method decimal () ((List new) addFirst: 0))
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

  (method first:rest: (anInteger aNatural)
      (set m aNatural) (set d anInteger) self)
  
  (method divBase () m)
  (method modBase () d)
  
  (method timesBase () ((NatNonzero new) first:rest: 0 self))

  (method sdivmod:with: (v K) [locals b X' x0]
    (set b (Natural base))
    (set X' (self divBase))
    (set x0 (self modBase))
    ((v < 1) ifFalse:ifTrue: ;; TODO see if necessary to compare v and base
      {(X' sdivmod:with: v 
        (block [Q r] 
          (K value:value: 
            (Q timesDigit:plus: b (((r * b) + x0) div: v)) ;; Q'
            (((r * b) + x0) mod: v))))} ;; r'
      {(self error: 'Bad-divisor)}))

  (method decimal () [locals digits X] 
    (set digits (List new))
    (set X self)
    ({(X isZero)} whileFalse: 
      {(set digits (digits addFirst: (X smod: 10)))
       (set X (X sdiv: 10))})
    digits)

  (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock)
    ((self divBase) compare:withLt:withEq:withGt: (aNatural divBase)
        ltBlock 
        {(((self modBase) = (aNatural modBase)) ifTrue:ifFalse:
          {(eqBlock value)}
          {(((self modBase) < (aNatural modBase)) ifTrue:ifFalse:
            {(ltBlock value)}
            {(gtBlock value)})})}
        gtBlock))
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

;; shorthand constructor
(val Nat [block (n) (Natural fromSmall: n)])

        (check-assert ((Nat value: 0) isZero))
        (check-assert (((Nat value: 43) isZero) not))

        ;; test for step 10
        (check-assert (((Nat value: 4096) divBase) isKindOf: Natural))
        (check-assert (((Nat value: 0) divBase) isKindOf: Natural))
        (check-assert (((Nat value: 4096) timesBase) isKindOf: Natural))
        (check-assert (((Nat value: 0) timesBase) isKindOf: Natural))
        (check-assert (((Nat value: 4096) modBase) isKindOf: SmallInteger))
        (check-assert (((Nat value: 0) modBase) isKindOf: SmallInteger))
        (check-assert (3 isKindOf: SmallInteger))
        ;; (check-print  ((Nat value: 4096) modBase) 4096)
        ;; (check-print  ((Nat value: 4097) modBase) 4097)
        ;; (check-print  ((Nat value: 1) modBase ) 1)
        (check-print ((Nat value: 0) divBase) 0)
        ;; (check-print ((Nat value: 4096) modBase) 4096)
        (check-print ((Nat value: 0) timesBase) 0)

        ;;testing addition

        ;;testing division 
        (check-error ((Nat value: 12) sdiv: 0))
        (check-error ((Nat value: 0) sdiv: 0))

        ;;testing mod 
        (check-print ((Nat value: 15) smod: 4) 3)
        (check-print ((Nat value: 0) smod: 2) 0)
        (check-print ((Nat value: 12) smod: 4) 0)
        (check-error ((Nat value: 12) smod: 0))

        ;; test decimal
        (check-print (Nat value: 0) 0)
        (check-print (Nat value: 1) 1)
        (check-print (Nat value: 1000) 1000)
        (check-print (Nat value: 1234) 1234)

        ;; comparison checks
        (check-print ((Nat value: 0) compare-symbol: (Nat value: 0)) EQ)
        (check-print ((Nat value: 10) compare-symbol: (Nat value: 0)) GT)
        (check-print ((Nat value: 0) compare-symbol: (Nat value: 10)) LT)
        (check-print ((Nat value: 1007) compare-symbol: (Nat value: 1009)) LT)
        (check-print ((Nat value: 10009) compare-symbol: (Nat value: 1009)) GT)
        (check-print ((Nat value: 1009) compare-symbol: (Nat value: 1009)) EQ)
        (check-print ((Nat value: 1009) compare-symbol: (Nat value: 9)) GT)
        (check-print ((Nat value: 9) compare-symbol: (Nat value: 1009)) LT)
        (check-print ((Nat value: 8) compare-symbol: (Nat value: 1009)) LT)

        (check-assert ((Nat value: 0) = (Nat value: 0)))
        (check-assert ((Nat value: 10) > (Nat value: 0)))
        (check-assert ((Nat value: 0) < (Nat value: 10)))
        (check-assert ((Nat value: 1007) < (Nat value: 1009)))
        (check-assert ((Nat value: 10009) > (Nat value: 1009)))
        (check-assert ((Nat value: 1009) = (Nat value: 1009)))
        (check-assert ((Nat value: 1009) > (Nat value: 9)))
        (check-assert ((Nat value: 9) < (Nat value: 1009)))
        (check-assert ((Nat value: 8) < (Nat value: 1009)))

        ;; test subtraction

        (check-print ((Nat value: 10) - (Nat value: 8)) 2)
        (check-print ((Nat value: 10) - (Nat value: 10)) 0)
        (check-print ((Nat value: 10) - (Nat value: 0)) 10)
        (check-print ((Nat value: 0) - (Nat value: 0)) 0)
        (check-error ((Nat value: 0) - (Nat value: 1)))
        (check-error ((Nat value: 8) - (Nat value: 10)))

        (check-print ((Nat value: 87654321) * (Nat value: 12345678))
            1082152022374638)

;; stress test

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
     ((anInteger isNegative) ifTrue:ifFalse: ;; TODO test 0 - anInteger?
;;        {(((self fromSmall: 1) + (self fromSmall: ((anInteger + 1) negated)))
;;              negated)}
        {((LargeNegativeInteger new) magnitude: (Natural fromSmall: (0 - anInteger)))}
        {((LargePositiveInteger new) magnitude: 
                 (Natural fromSmall: anInteger))}))
  (method isZero () (magnitude isZero))
  (method = (anInteger) ((self - anInteger)     isZero))
  (method < (anInteger) ((self - anInteger) isNegative))

  (method div: (n) (self sdiv: n))
  (method mod: (n) (self smod: n))

  (method sdiv: (n) (self subclassResponsibility))
  (method smod: (n) (self - ((LargeInteger fromSmall: n) * (self sdiv: n))))

  (method print () (self subclassResponsibility))
  (method isNegative () (self subclassResponsibility))
  (method isNonnegative () (self subclassResponsibility))
  (method isStrictlyPositive () (self subclassResponsibility))
  (method negated () (self subclassResponsibility))
  (method * (anInteger) (self subclassResponsibility) )
  (method + (anInteger) (self subclassResponsibility))
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

  (method print () ((self magnitude) print))

  (method isNegative () false)
  (method isNonnegative () true)
  (method isStrictlyPositive () 
    (((self magnitude) isZero) ifTrue:ifFalse: {false} {true}))
  (method negated () (LargeNegativeInteger withMagnitude: (self magnitude)))

  (method * (anInteger) (anInteger multiplyByLargePositiveInteger: self))
  (method multiplyByLargePositiveInteger: (anInteger) 
    (LargePositiveInteger withMagnitude: 
      ((self magnitude) * (anInteger magnitude))))
  (method multiplyByLargeNegativeInteger: (anInteger)
    (LargeNegativeInteger withMagnitude: 
      ((self magnitude) * (anInteger magnitude))))
 
  (method + (anInteger) (anInteger addLargePositiveIntegerTo: self) )
  (method addLargePositiveIntegerTo: (anInteger) 
    (LargePositiveInteger withMagnitude:
      ((self magnitude) + (anInteger magnitude)))) 
  (method addLargeNegativeIntegerTo: (anInteger) 
    (((self magnitude) > (anInteger magnitude)) ifTrue:ifFalse:
      {(LargePositiveInteger withMagnitude: 
        ((self magnitude) - (anInteger magnitude)))}
      {(LargeNegativeInteger withMagnitude: 
        ((anInteger magnitude) - (self magnitude)))})) 

)

;; Represents a negative integer
(class LargeNegativeInteger
  [subclass-of LargeInteger]

  ;; short division (already implemented for you)
  (method sdiv: (anInteger)
    ((self negated) sdiv: (anInteger negated)))

  (method print () 
    (((self magnitude) isZero) ifFalse: {('- print)})
    ((self magnitude) print)) 

  (method isNegative () 
    (((self magnitude) isZero) ifTrue:ifFalse: {false} {true}))

  (method isNonnegative () false)
  (method isStrictlyPositive () false)
  (method negated () (LargePositiveInteger withMagnitude: (self magnitude)))

  (method * (anInteger) (anInteger multiplyByLargeNegativeInteger: self))
  (method multiplyByLargePositiveInteger: (anInteger) 
    (LargeNegativeInteger withMagnitude: 
        ((self magnitude) * (anInteger magnitude))))
  (method multiplyByLargeNegativeInteger: (anInteger) 
    (LargePositiveInteger withMagnitude: 
        ((self magnitude) * (anInteger magnitude))))

  (method + (anInteger) (anInteger addLargeNegativeIntegerTo: self) )
  (method addLargePositiveIntegerTo: (anInteger)
    (((self magnitude) > (anInteger magnitude)) ifTrue:ifFalse:
        {(LargeNegativeInteger withMagnitude: 
            ((self magnitude) - (anInteger magnitude)))}
        {(LargePositiveInteger withMagnitude: 
            ((anInteger magnitude) - (self magnitude)))})) 

  (method addLargeNegativeIntegerTo: (anInteger) 
    (LargeNegativeInteger withMagnitude: 
        ((self magnitude) + (anInteger magnitude)))) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put your unit tests for Exercise 2 here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shorthand constructors
(val Pos [block (n) (LargePositiveInteger withMagnitude: (Nat value: n))])
(val Neg [block (n) (LargeNegativeInteger withMagnitude: (Nat value: n))])
(val Int [block (n) (LargeInteger fromSmall: n)])

;;step 4 sign-query tests  

        (check-assert ((Pos value: 4096) isStrictlyPositive))
        (check-assert (((Neg value: 0) isStrictlyPositive) not))
        (check-assert (((Neg value: 4096) isStrictlyPositive) not))

        (check-assert ((Pos value: 4096) isNonnegative))
        (check-assert (((Neg value: 4096) isNonnegative) not))

        (check-assert (((Pos value: 4096) isNegative) not))
        (check-assert (((Pos value: 0) isNegative) not))
        (check-assert ((Neg value: 4096) isNegative))

        ;;step 6 testing negation and printing
        (check-assert ((((Pos value: 4096) negated) isStrictlyPositive) not))
        (check-assert ((((Pos value: 0) negated) isStrictlyPositive) not))
        (check-assert (((Neg value: 4096) negated) isStrictlyPositive))

        (check-assert ((((Pos value: 4096) negated) isNonnegative) not))
        (check-assert (((Neg value: 4096) negated) isNonnegative))
        (check-assert (((Pos value: 4096) negated) isNegative))
        (check-assert ((((Pos value: 0) negated) isNegative) not))
        (check-assert ((((Neg value: 4096) negated) isNegative) not))

        (check-print (Pos value: 4096) 4096)
        (check-print (Pos value: 0) 0)
        (check-print (Pos value: 123123112) 123123112)
        (check-print (Neg value: 0) 0)
        (check-print (Neg value: 3432342) -3432342)
        (check-print ((Int value: 0) negated) 0)
        (check-print ((Int value: -1000) * (Int value: 2)) -2000)
        (check-print ((Int value: -1) negated) 1)
        (check-print ((Int value: 1) negated) -1)

        ;;step 8: testing multiplication
        (check-print ((Pos value: 3) * (Pos value: 4)) 12)
        (check-print ((Pos value: 5) * (Pos value: 4)) 20)
        (check-print ((Pos value: 5) * (Pos value: 4)) 20)
        (check-print ((Neg value: 3) * (Pos value: 4)) -12)
        (check-print ((Neg value: 5) * (Pos value: 4)) -20)
        (check-print ((Neg value: 3) * (Neg value: 10)) 30)
        (check-print ((Pos value: 3) * (Pos value: 0)) 0)
        (check-print ((Pos value: 3) * (Neg value: 0)) 0)
        (check-print ((Pos value: 3) * (Neg value: 0)) 0)

        ;;step 10 testing addition
        (check-print ((Pos value: 3) + (Pos value: 4)) 7)
        (check-print ((Pos value: 3) + (Neg value: 4)) -1)
        (check-print ((Pos value: 100) + (Neg value: 4)) 96)
        (check-print ((Neg value: 3) + (Pos value: 4)) 1)
        (check-print ((Neg value: 30) + (Pos value: 4)) -26)
        (check-print ((Neg value: 3) + (Neg value: 4)) -7)

        ; We tested -158223 > -158223 (<False>),
        ; starting from literals of class LargeInteger
        (((Int value: 223)
            +
             ((Int value: 1000)
             *
              ((Int value: 158)
              +
               ((Int value: 1000) * (Int value: 0)))))
            negated)

        (check-print
          ((((Int value: 223)
            +
             ((Int value: 1000)
             *
              ((Int value: 158)
              +
               ((Int value: 1000) * (Int value: 0)))))
           negated)
           >
          (((Int value: 223)
            +
             ((Int value: 1000)
             *
              ((Int value: 158)
              +
               ((Int value: 1000) * (Int value: 0)))))
            negated))
          <False>)
