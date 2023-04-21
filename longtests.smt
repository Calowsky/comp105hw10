(Natural addSelector:withMethod: 'squared
  (compiled-method () (self * self)))
  (Natural addSelector:withMethod: 'coerce:
    (compiled-method (i) (Natural fromSmall: i)))
    (Natural addSelector:withMethod: 'raisedToInteger:
      (Number compiledMethodAt: 'raisedToInteger:))
      (check-print ((Natural fromSmall: 10) raisedToInteger: 10) 10000000000)
      (check-print ((Natural fromSmall:  9) raisedToInteger:  9)   387420489)
      (check-print ((Natural fromSmall: 99) raisedToInteger: 99) 369729637649726772657187905628805440595668764281741102430259972423552570455277523421410650010128232727940978889548326540119429996769494359451621570193644014418071060667659301384999779999159200499899) 
