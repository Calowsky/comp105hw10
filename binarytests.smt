
(check-print (DebugNat of: (Natural fromSmall: 0))
             0)
(check-print (DebugNat of: 
                (Natural fromSmall: ((Natural base) * (Natural base))))
             0,1,0,0)  ;; or it might have a leading zero
(check-print (DebugNat of: (Natural fromSmall: 1))
             0,1)
(check-print (DebugNat of: (Natural fromSmall: 1000))
             0,1,1,1,1,1,0,1,0,0,0)
(check-print (DebugNat of: (Natural fromSmall: 1234))
             0,1,0,0,1,1,0,1,0,0,1,0)
(check-print (DebugNat of: (Natural fromSmall: 4096))
             0,1,0,0,0,0,0,0,0,0,0,0,0,0)

(check-print (DebugNat of: ((Natural fromSmall: 4096) divBase ))
             0,1,0,0,0,0,0,0,0,0,0,0,0)

(check-print (DebugNat of: ((Natural fromSmall: 0) + (Natural fromSmall: 0)))
             0)
(check-print (DebugNat of: ((Natural fromSmall: 0) + (Natural fromSmall: 1)))
             0,1)
(check-print (DebugNat of: ((Natural fromSmall: 4) + (Natural fromSmall: 33)))
             0,1,0,0,1,0,1)
(check-print (DebugNat of: ((Natural fromSmall: 100) + (Natural fromSmall: 0)))
             0,1,1,0,0,1,0,0)

(check-print (DebugNat of: ((Natural fromSmall: 15) sdiv: 2))
             0,1,1,1)
(check-print (DebugNat of: ((Natural fromSmall: 0) sdiv: 2))
             0)
(check-print (DebugNat of: ((Natural fromSmall: 11) sdiv: 4))
             0,1,0)
