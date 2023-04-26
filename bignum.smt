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

  ; function contract in textbook
  (class-method fromSmall: (n)
    ((n = 0) ifTrue:ifFalse:
      {(NatZero new)}
      {(NatNonzero first:rest: 
          (n mod: (self base))
          (self fromSmall: (n div: (self base))))}))

  ; takes arguments anInteger and aNatural and answers a Natural number
  ; representing anInteger + aNatural * b.
  (class-method first:rest: (anInteger aNatural)
    (((anInteger = 0) & (aNatural isZero )) ifTrue:ifFalse:
      {(NatZero new)}
      {((NatNonzero new) first:rest: anInteger aNatural)}))

  ; base number for the class.
  (class-method base () 500) ; private

  ; returns the smallnum representing num modded by base
  (method modBase () (self subclassResponsibility)) 

  ; returns the natural representing current num divided by base, right shift
  (method divBase () (self subclassResponsibility)) 

  ; returns the natural representing current num * by base, left shift
  (method timesBase () (self subclassResponsibility)) 

  ; compare another natural with current, and pass in 3 blocks for <, >, =.
  (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock) 
    (self subclassResponsibility)) 

  ;; helper function for addition, takes in a natural number and 
  ;; a small and returns natural number representing them added togather.
  (method plus:carry: (aNatural c) (self subclassResponsibility)) 

  ;; helper function for minus, takes in a natural number and 
  ;; a small and returns natural number representing nat minus small.
  (method minus:borrow: (aNatural c) (self subclassResponsibility)) 

  ;; returns nat of self * d + r
  (method timesDigit:plus: (d r) (self subclassResponsibility)) ; private

  ;; check if current number is equal to another natural.
  (method = (aNatural) 
    (self compare:withLt:withEq:withGt:
      aNatural {false} {true} {false}))

  ;; check if current number is less than another natural.
  (method < (aNatural) 
    (self compare:withLt:withEq:withGt:
      aNatural {true} {false} {false}))

  ;; contract in textbook
  (method + (aNatural) (self plus:carry: aNatural 0))

  ;; contract in textbook
  (method * (aNatural) (self subclassResponsibility))
  (method subtract:withDifference:ifNegative: (aNatural diffBlock exnBlock)
    ((self < aNatural) ifTrue:ifFalse: 
      exnBlock 
      {(self minus:borrow: aNatural 0)}))


  ;; helper function for division.
  ;; (X sdivmod:with: v K) results in evaluating
  ;; (K value:value: Q' r'), where Q' = X div v and r' = X mod v.
  (method sdivmod:with: (n aBlock) (self subclassResponsibility))

  (method decimal () (self leftAsExercise))

  ;; checks if natural number is zero.
  (method isZero  () (self subclassResponsibility))

  ; methods that are already implemented for you
  ;; contract in textbook
  (method - (aNatural)
    (self subtract:withDifference:ifNegative:
      aNatural
      [block (x) x]
      {(self error: 'Natural-subtraction-went-negative)}))

  ; dividing current number by smallnum
  (method sdiv: (n) (self sdivmod:with: n [block (q r) q]))

  ; mod current number by smallnum
  (method smod: (n) (self sdivmod:with: n [block (q r) r]))

  ; print current number
  (method print () ((self decimal) do: [block (x) (x print)]))

 ;private methods for testing
  ; testing function to check if invariant is validated.
  (method validated ()
    ((self invariant) ifFalse:
    {(self printrep)
     (self error: 'invariant-violation)})
    self)


  ; testing function to help compare symbols.
  (method compare-symbol: (aNat)
    (self compare:withLt:withEq:withGt: aNat {'LT} {'EQ} {'GT}))

)

; Represents a 0 natural number
(class NatZero
  [subclass-of Natural]
  ;; invariant method is always true. zero is always valid.
  (method invariant () true) ;; private

  ; check if NatZero is zero, always true.
  (method isZero () true)

  ; add function to multiply by a digit and add second one.
  (method timesDigit:plus: (d r) (Natural fromSmall: r)) ; private

  ; divide NatZero by base, basically a right shift. returns NatZero.
  (method divBase () self )

  ; mod self by base, return NatZero.
  (method modBase () 0 )

  ; multiply NatZero by base, basically a left shift. returns NatZero.
  (method timesBase () self )

  ; multiply self by another natural, return NatZero.
  (method * (nat) self )

  ; (X sdivmod:with: v K) 
  ; results in evaluating (K value:value: Q' r'), where Q' = X div v 
  ; and r' = X mod v. 
  (method sdivmod:with: (v K) ((v < 1) ifFalse:ifTrue: ;; TODO check divisors
    {(K value:value: self 0)}
    {(self error: 'Bad-divisor)}))

  ; compare self with a natural, passing in another natural and 
  ; blocks for <, >, =.
  (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock)
    ((aNatural isZero) ifTrue:ifFalse: {(eqBlock value)} {(ltBlock value)}))

  ; takes in a natural and smallnum. returns natural+smallnum+self.
  (method plus:carry: (aNatural c)
    ((aNatural isZero) ifTrue:ifFalse:
      {(Natural fromSmall: c)}
      {(aNatural + (Natural fromSmall: c))}))

  ; takes in a natural and smallnum. returns self-natural-b. 
  ; returns error if this is negative.
  (method minus:borrow: (aNatural b) 
    (((aNatural isZero) & (b = 0)) ifTrue:ifFalse:
      {self}
      {(self error: 'subtraction-went-negative)}))

  ;; for debugging
  ; returns decimal representation.
  (method decimal () ((List new) addFirst: 0))
  ; print 0.
  (method printrep () (0 print))
)

; Represents a natural number thats not 0
(class NatNonzero
  [subclass-of Natural]
  [ivars m d] ; a non-zero natural number is of the form d + m * b, where d
              ; is an integer representing a digit of base b, and m is
              ; a natural number

  ; checks if natnonzero is zero, always false.
  (method isZero () false)

  ; checks if invariant is true, m and d are not both zero
  (method invariant () (((d < (Natural base)) & (d >= 0)) &  ;; private
                       (((m isZero) & (d = 0)) not)))

  ; addition with a natural and a  carry smallnum, 
  (method plus:carry: (aNatural c) [locals sum least cout]
     (set sum ((d + (aNatural modBase)) + c))
     (set least (sum mod: (Natural base)))
     (set cout  (sum div: (Natural base)))
     (NatNonzero first:rest: least (m plus:carry: (aNatural divBase) cout)))
      
  ; subtraction with a natural and a borrow smallnum
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


  ; returns self * d + r as natural number
  (method timesDigit:plus: (dig r) ; private, answers self * d + r
      [locals pp]
      (set pp ((d * dig) + r))
      (NatNonzero first:rest: (pp mod: (Natural base))
        (m timesDigit:plus: dig (pp div: (Natural base)))))
  
  ;; debugging method
  (method printrep () (m printrep) (', print) (d print))

  ; takes arguments anInteger and aNatural and answers a Natural number
  ; representing anInteger + aNatural * b.
  (method first:rest: (anInteger aNatural)
      (set m aNatural) (set d anInteger) self)
  

  ; returns the number as natural divided by base as natural number, 
  ; basically right shift.
  (method divBase () m)

  ; returns smallnum of number modded by base.
  ; basically right shift.
  (method modBase () d)
  
  ; returns the number as natural multiplied by base as natural 
  ; number, basically left shift.
  (method timesBase () ((NatNonzero new) first:rest: 0 self))

  ; helper function for division.
  ;(X sdivmod:with: v K) results in evaluating (K value:value: Q' r'), 
  ; where Q' = X div v and r' = X mod v
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


  ; returns list of digits of natural number.
  (method decimal () [locals digits X] 
    (set digits (List new))
    (set X self)
    ({(X isZero)} whileFalse: 
      {(set digits (digits addFirst: (X smod: 10)))
       (set X (X sdiv: 10))})
    digits)

  ; compare self with another natural number, 
  ; also passing in blocks in case of <, >, or =.
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
        (check-print ((Nat value: 2) timesBase) 1000)
        (check-print ((Nat value: 16) timesBase) 8000)

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

;; All the tests that require base of 2, now commented out
;;       (check-print (DebugNat of: (Natural fromSmall: 0))
;;                    0)
;;        (check-print (DebugNat of: 
;;                        (Natural fromSmall: ((Natural base) * (Natural base))))
;;                    0,1,0,0)  ;; or it might have a leading zero
;;        (check-print (DebugNat of: (Natural fromSmall: 1))
;;                    0,1)
;;        (check-print (DebugNat of: (Natural fromSmall: 1000))
;;                    0,1,1,1,1,1,0,1,0,0,0)
;;        (check-print (DebugNat of: (Natural fromSmall: 1234))
;;                    0,1,0,0,1,1,0,1,0,0,1,0)
;;        (check-print (DebugNat of: (Natural fromSmall: 4096))
;;                    0,1,0,0,0,0,0,0,0,0,0,0,0,0)
;;
;;        (check-print (DebugNat of: ((Natural fromSmall: 4096) divBase ))
;;                    0,1,0,0,0,0,0,0,0,0,0,0,0)

;;        (check-print (DebugNat of: ((Natural fromSmall: 0) + 
;;                                    (Natural fromSmall: 0)))
;;                    0)
;;        (check-print (DebugNat of: ((Natural fromSmall: 0) + 
;;                                    (Natural fromSmall: 1)))
;;                    0,1)
;;        (check-print (DebugNat of: ((Natural fromSmall: 4) + 
;;                                    (Natural fromSmall: 33)))
;;                    0,1,0,0,1,0,1)
;;        (check-print (DebugNat of: ((Natural fromSmall: 100) + 
;;                                    (Natural fromSmall: 0)))
;;                    0,1,1,0,0,1,0,0)

;;        (check-print (DebugNat of: ((Natural fromSmall: 15) sdiv: 2))
;;                    0,1,1,1)
;;        (check-print (DebugNat of: ((Natural fromSmall: 0) sdiv: 2))
;;                    0)
;;        (check-print (DebugNat of: ((Natural fromSmall: 11) sdiv: 4))
;;                    0,1,0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;             Exercise 2 classes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class LargeInteger
  [subclass-of Integer]
  [ivars magnitude]

  ; function contract in textbook
  (class-method withMagnitude: (aNatural)
      ((self new) magnitude: aNatural))

  ; function contract in textbook
  (method magnitude: (aNatural) ; private, for initialization
      (set magnitude aNatural)
      self)

  ; function contract in textbook
  (method magnitude () magnitude)

  ; function contract in textbook
  (class-method fromSmall: (anInteger)
     ((anInteger isNegative) ifTrue:ifFalse:
        {((LargeNegativeInteger new) magnitude: 
            (Natural fromSmall: (0 - anInteger)))}
        {((LargePositiveInteger new) magnitude: 
            (Natural fromSmall: anInteger))}))

  ; checks if a large integer is zero
  (method isZero () (magnitude isZero))

  ; checks if another LargeInteger is equal to the current one.
  (method = (anInteger) ((self - anInteger)     isZero))

  ; checks if another LargeInteger is greater then the current one.
  (method < (anInteger) ((self - anInteger) isNegative))

  ;divide current LargeInteger by smallnum
  (method div: (n) (self sdiv: n))
  
  ;mod current LargeInteger by smallnum
  (method mod: (n) (self smod: n))

  ; function contract in textbook
  (method sdiv: (n) (self subclassResponsibility))

  ; function contract in textbook
  (method smod: (n) (self - ((LargeInteger fromSmall: n) * (self sdiv: n))))

  ;print the LargeInteger
  (method print () (self subclassResponsibility))

  ;check if LargeInteger is negative
  (method isNegative () (self subclassResponsibility))

  ;check if LargeInteger is non-negative
  (method isNonnegative () (self subclassResponsibility))

  ;check if LargeInteger is greater than zero
  (method isStrictlyPositive () (self subclassResponsibility))

  ;return the LargeInteger, multiplied by -1
  (method negated () (self subclassResponsibility))

  ; multiply current LargeInteger by another
  (method * (anInteger) (self subclassResponsibility) )

  ; add current LargeInteger to another.
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

  ; prints the LargePositiveInteger
  (method print () ((self magnitude) print))

  ; checks if the LargePositiveInteger is negative, always false
  (method isNegative () false)

  ; checks if the LargePositiveInteger is non-negative, always true
  (method isNonnegative () true)

  ; checks if the LargePositiveInteger isn't zero
  (method isStrictlyPositive () 
    (((self magnitude) isZero) ifTrue:ifFalse: {false} {true}))
  (method negated () (LargeNegativeInteger withMagnitude: (self magnitude)))

   ; multiplies two largenumbers togather
  (method * (anInteger) (anInteger multiplyByLargePositiveInteger: self))
  (method multiplyByLargePositiveInteger: (anInteger) 
    (LargePositiveInteger withMagnitude: 
      ((self magnitude) * (anInteger magnitude))))
  (method multiplyByLargeNegativeInteger: (anInteger)
    (LargeNegativeInteger withMagnitude: 
      ((self magnitude) * (anInteger magnitude))))
 
   ; adds two largenumbers numbers togather
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

  ; prints LargeNegativeInteger
  (method print () 
    (((self magnitude) isZero) ifFalse: {('- print)})
    ((self magnitude) print)) 

  ;checks if LargeNegativeInteger is negative, so if it's nonzero.
  (method isNegative () 
    (((self magnitude) isZero) ifTrue:ifFalse: {false} {true}))

  ;checks if LargeNegativeInteger is nonnegative, so always false.
  (method isNonnegative () false)

  ;checks if LargeNegativeInteger is greater than zero, so always false.
  (method isStrictlyPositive () false)

  ;returns the number multiplied by -1.
  (method negated () (LargePositiveInteger withMagnitude: (self magnitude)))


  ;multiples current LargeNegativeInteger with another number.
  (method * (anInteger) (anInteger multiplyByLargeNegativeInteger: self))
  (method multiplyByLargePositiveInteger: (anInteger) 
    (LargeNegativeInteger withMagnitude: 
        ((self magnitude) * (anInteger magnitude))))
  (method multiplyByLargeNegativeInteger: (anInteger) 
    (LargePositiveInteger withMagnitude: 
        ((self magnitude) * (anInteger magnitude))))

  ;adds current LargeNegativeInteger with another number.
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