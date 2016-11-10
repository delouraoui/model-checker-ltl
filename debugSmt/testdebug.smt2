
(set-info :status 0)
(set-logic LIA)
(assert
(exists ((z0t Int) (z1t Int) (z2t Int) (z3t Int)
(z0p Int) (z1p Int) (z2p Int) (z3p Int)
(z0it Int) (z1it Int) (z2it Int) (z3it Int)
(q0 Int) (p Int))

   (and
	(= q0 0)
    (= p 0)
	   
	 (exists ((q Int)) 
	  (and
	   
	   (or (= q 0))
	   
	   (exists ((p0 Int) (p1 Int) (p2 Int) (p3 Int))
	   (and 

	   	 	
	(and 
		(and 
			(=> (< z0p 3) 
				(and (= z3t z3p) (= z3p z3it) (= z3it 0))) 
			(=> (< z0p 2) 
				(and (= z2t z2p) (= z2p z2it) (= z2it 0))) 
			(=> (< z0p 1) 
				(and (= z1t z1p) (= z1p z1it) (= z1it 0))) 

			(and (= z0t z0p) (= z0it z0p) (> z0p 0) (< z0p 4))
		) 
		(and 
			(and (<= z3t 1) (<= 0 z3t)) 
			(and (<= z2t 1) (<= 0 z2t)) 
			(and (<= z1t 1) (<= 0 z1t))
		) 
		(and 
			(=> (and (<= 3 z0p) (= z3t 0)) (and (< z3p 2) (>= z3p 0))) 
			(=> (and (<= 2 z0p)(= z2t 0)) (and (< z2p 2) (>= z2p 0))) 
			(=> (and (<= 1 z0p) (= z1t 0)) (and (< z1p 2) (>= z1p 0)))
		) 
		(and 
			(=> (and (<= 3 z0p) (= z3t 1)) (and (< z3p 1) (>= z3p 0))) 
			(=> (and (<= 2 z0p) (= z2t 1)) (and (< z2p 1) (>= z2p 0))) 
			(=> (and (<= 1 z0p) (= z1t 1)) (and (< z1p 1) (>= z1p 0)))
		) 
		(and 
			(=> (= 3 z0p) (= z3t 1)) 
			(=> (= 2 z0p) (= z2t 1)) 
			(=> (= 1 z0p) (= z1t 1))
		) 
		(and 
			(=> (<= 3 z0p) 
				(and 
					(=> 
						(<= 2 z0p) 
						(or (not (= z3p z2p)) (not (= z3t z2t)))
					) 
					(=> 
						(<= 1 z0p) 
						(or (not (= z3p z1p)) (not (= z3t z1t)))
					)
				)
			) 
			(=> (<= 2 z0p) 
				(and 
					(=> 
						(<= 3 z0p) 
						(or (not (= z2p z3p)) (not (= z2t z3t)))
					) 
					(=> 
						(<= 1 z0p) 
						(or (not (= z2p z1p)) (not (= z2t z1t)))
					)
				)
			)
			(=> 
				(<= 1 z0p) 
				(and 
					(=> 
						(<= 3 z0p) 
						(or (not (= z1p z3p)) (not (= z1t z3t)))
					) 
					(=> 
						(<= 2 z0p) 
						(or (not (= z1p z2p)) (not (= z1t z2t)))
					)
				)
			)
		) 

		(and 
			(=> 

				(< 2 z0p) 
				(or 
					(and 
						(= 1 z2t) (= 0 z2p) (= 0 z3t) (= 1 z3p)
					) 
					(and 
						(= 0 z2t) (= 0 z2p) (= 1 z3t) (= 0 z3p)
					) 
					(and 
						(= 0 z2t) (= 1 z2p) (= 1 z3t) (= 0 z3p)
					) 
					(and 
						(= 0 z2t) (= 0 z2p) (= 0 z3t) (= 1 z3p)
					) 
					(and 
						(= 0 z2t) (= 1 z2p) (= 0 z3t) (= 1 z3p))
					)
				) 
			(=> 
				(< 1 z0p) 
				(or 

					(and (= 1 z1t) (= 0 z1p) (= 0 z2t) (= 1 z2p)) 
					(and (= 0 z1t) (= 0 z1p) (= 1 z2t) (= 0 z2p)) 
					(and (= 0 z1t) (= 1 z1p) (= 1 z2t) (= 0 z2p)) 
					(and (= 0 z1t) (= 0 z1p) (= 0 z2t) (= 1 z2p)) 
					(and (= 0 z1t) (= 1 z1p) (= 0 z2t) (= 1 z2p))
				)
			)
		) 
		(and 
			(=> (= z0p 3) (= z3it 0)) 
			(=> (< 3 z0p) (and (> z3it 0) (=> (= z3t 0) (= z3it 1)))) 
			(=> (= z0p 2) (= z2it 0)) 
			(=> (< 2 z0p) (and (> z2it 0) (=> (= z2t 0) (= z2it 1)))) 
			(=> (= z0p 1) (= z1it 0)) 
			(=> (< 1 z0p) (and (> z1it 0) (=> (= z1t 0) (= z1it 1))))
		)
	) 




      (and 
		(or 
			(or (and (= z1t 0) (= z1p 0) (= 0 q0)) (and (= z1t 1) (= z1p 0) (= 1 q0))) 
			(or (and (= z1t 0) (= z1p 1) (= 1 q0)) false) 
			(or false false)
		) 
		(or 
			(and 
				(=> (and (< 3 z0t) (= z3t 0)) (= p3 (+ 1 p2))) 
				(=> (and (= z3t 1) (= z3p 1) (< 3 z0t)) false)
			) 
			(and 
				(=> (and (< 3 z0t) (= z3t 0)) (= p3 (+ 1 p2))) 
				(=> (and (= z3t 1) (= z3p 0) (< 3 z0t)) (= p3 (+ (* 1 z3it) p2)))
			)
		) 
		(or 
			(and 
				(=> (and (< 2 z0t) (= z2t 0)) (= p2 (+ 1 p1))) 
				(=> (and (= z2t 1) (= z2p 1) (< 2 z0t)) false)
			) 
			(and 
				(=> (and (< 2 z0t) (= z2t 0)) (= p2 (+ 1 p1))) 
				(=> (and (= z2t 1) (= z2p 0) (< 2 z0t)) (= p2 (+ (* 1 z2it) p1)))
			)
		) 
		(or 
			(and 
				(=> (and (< 1 z0t) (= z1t 0)) (= p1 (+ 1 p0))) 
				(=> (and (= z1t 1) (= z1p 1) (< 1 z0t)) false)
			) 
			(and 
				(=> (and (< 1 z0t) (= z1t 0)) (= p1 (+ 1 p0))) 
				(=> (and (= z1t 1) (= z1p 0) (< 1 z0t)) (= p1 (+ (* 1 z1it) p0)))
			)
		) 
		(= p0 0)
	) 

			   

		(or 
		
		
		(and 
			(=> 
				(and (<= 0 z0t) (= p p0)) 
				(or 
					(or 
						(and (= z1t 1) (= z1p 0) (= 1 q)) 
						(and (= z1t 0) (= z1p 0) (= 0 q))
					) 
					(or 
						false 
						(and (= z1t 0) (= z1p 1) (= 1 q))
					)
				)
			) 
			(=> 
				(and 
					(or 
						(and (<= 0 z0t) (< p0 p) (< p p1)) 
						(and (= 0 z0t) (< p0 p))
					) 
					(= z1t 1)
				) 
				(or 
					(and (= z1p 0) (or (and (= (- p p0) (mod 1 1)) (= q 1))))
				)
			)
		)
	)
	   )
	   )
	  )
	)
   )
)
)

(check-sat)
