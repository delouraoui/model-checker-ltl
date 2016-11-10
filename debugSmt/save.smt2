
(set-info :status 0)
(set-logic LIA)
(assert
(exists ((z0t Int) (z1t Int) (z2t Int) (z3t Int)
(z0p Int) (z1p Int) (z2p Int) (z3p Int)
(z0it Int) (z1it Int) (z2it Int) (z3it Int)
(q0 Int) (p Int))
   
	   (and
		(and (= z0t z0p) (= z0p z0it) (> z0p  0) (< z0p 4)
		(=>  (< z0p 1)	
     		 (and (= z1t z1p) (= z1p z1it) (= z1p  0)) )
		(=>  (< z0p 2)
     		 (and (= z2t z2p) (= z2p z2it) (= z2p  0)) )
		(=>  (< z0p 3)
     		 (and (= z3t z3p) (= z3p z3it) (= z3p  0)) )
 	   	 )

	        (and
		 (<= z1t 1) (>= z2t 0) (<= z2t 1) (>= z2t 0) (<= z3t 1) (>= z3t 0)
   		)

		(and
   		 (=> (<= 1 z0p) (=> (= z1t 0) (and (<= z1p 2) (>= z1p 1)) ))
   		 (=> (<= 2 z0p) (=> (= z2t 0) (and (<= z2p 2) (>= z2p 1)) ))
   		 (=> (<= 3 z0p) (=> (= z3t 0) (and (<= z3p 2) (>= z3p 1)) ))
		)

		(and
   		 (=> (<= 1 z0p) (=> (= z1t 0) (and (<= z1p 1) (>= z1p 1)) ))
   		 (=> (<= 2 z0p) (=> (= z2t 0) (and (<= z2p 1) (>= z2p 1)) ))
       		 (=> (<= 3 z0p) (=> (= z3t 0) (and (<= z3p 1) (>= z3p 1)) ))
		)

		(and
  		 (=> (= 1 z0p) (= z1t 1)) (=> (= 2 z0p) (= z2t 1)) (=> (= 2 z0p) (= z2t 1))	
		 )

		 (and 
  		  (=> (<= 1 z0p)
   		   (and
		   (=> (<= 2 z0p) (or (not (= z1p z2p)) (not (= z1t z2t)) ))
		   (=> (<= 3 z0p) (or (not (= z1p z3p)) (not (= z1t z3t))))
  		   )
  		  )
  		  (=> (<= 2 z0p)
  		   (and
	 	    (=> (<= 1 z0p) (or (not (= z1p z2p)) (not (= z1t z2t)))
		    (=> (<= 3 z0p) (or (not (= z2p z3p)) (not (= z2t z3t))))
  		    )
  		   )
  		  )
  		(=> (<= 3 z0p)
  		(and
		 (=> (<= 2 z0p) (or (not (= z3p z2p)) (not (= z3t z2t))))
		 (=> (<= 1 z0p) (or (not (= z1p z3p)) (not (= z1t z3t))))
  		)
  		)     
	       )

	       (and 
 	        (=> (< 1 z0p) (or
		 (and (= z1t 0) (= z1p 1) (= z2t 0) (= z2p 1))
    		 (and (= z1t 0) (= z1p 1) (= z2t 0) (= z2p 1))
    		 (and (= z1t 1) (= z1p 1) (= z2t 1) (= z2p 1))
    	 	 (and (= z1t 1) (= z1p 1) (= z2t 0) (= z2p 1))
    	        )
 	       )
 	       (=> (< 2 z0p) (or
		(and (= z2t 0) (= z2p 1) (= z3t 0) (= z3p 1))
    		(and (= z2t 0) (= z2p 1) (= z3t 0) (= z3p 1))
    		(and (= z2t 1) (= z2p 1) (= z3t 1) (= z3p 1))
    		(and (= z2t 1) (= z2p 1) (= z3t 0) (= z3p 1))
    	       )
	       )
		)

		(and
		 (=> (<= 1 z0p) (and (> z1it 0) (=> (= z1t 0) (= z1it 1))) )
      	    	 (=> (<= 2 z0p) (and (> z2it 0) (=> (= z2t 0) (= z2it 1))) )
  		 (=> (<= 3 z0p) (and (> z3it 0) (=> (= z3t 0) (= z3it 1))) )
      	    	 )
	)

)
)
(check-sat)