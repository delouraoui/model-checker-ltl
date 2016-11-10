; schema
(set-info :status 0)
(set-logic LIA)
(assert
 (exists ((y0 Int) (q0 Int) (z0t Int) (z1t Int) (z2t Int) (z3t Int) (z0p Int) (z1p Int) (z2p Int) (z3p Int) (z0it Int) (z1it Int) (z2it Int) (z3it Int) )(let (($x421 (= q0 0)))
(let (($x682 (exists ((y1 Int) )(let (($x678 (exists ((q1 Int) )(let (($x425 (exists ((p0 Int) (p1 Int) (p2 Int) (p3 Int) )(let (($x421 (= z1p 0)))
(let (($x429 (and $x421 (or (and (= (- y1 p0) (mod 1 1)) (= q1 1))))))
(let (($x431 (= z1t 1)))
(let (($x436 (or (and (<= 0 z0t) (< p0 y1) (< y1 p1)) (and (= 0 z0t) (< p0 y1)))))
(let (($x445 (or (and $x431 $x421 (= 1 q1)) (and (= z1t 0) $x421 (= 0 q1)))))
(let (($x446 (or $x445 (or false (and (= z1t 0) (= z1p 1) (= 1 q1))))))
(let (($x449 (and (=> (and (<= 0 z0t) (= y1 p0)) $x446) (=> (and $x436 $x431) (or $x429)))))
(let (($x456 (=> (and $x431 $x421 (< 1 z0t)) (= p1 (+ (* 1 z1it) p0)))))
(let (($x459 (and (=> (and (< 1 z0t) (= z1t 0)) (= p1 (+ 1 p0))) $x456)))
(let (($x462 (and (=> (and (< 1 z0t) (= z1t 0)) (= p1 (+ 1 p0))) (=> (and $x431 (= z1p 1) (< 1 z0t)) false))))
(let (($x471 (=> (and (= z2t 1) (= z2p 0) (< 2 z0t)) (= p2 (+ (* 1 z2it) p1)))))
(let (($x475 (and (=> (and (< 2 z0t) (= z2t 0)) (= p2 (+ 1 p1))) $x471)))
(let (($x479 (and (=> (and (< 2 z0t) (= z2t 0)) (= p2 (+ 1 p1))) (=> (and (= z2t 1) (= z2p 1) (< 2 z0t)) false))))
(let (($x488 (=> (and (= z3t 1) (= z3p 0) (< 3 z0t)) (= p3 (+ (* 1 z3it) p2)))))
(let (($x492 (and (=> (and (< 3 z0t) (= z3t 0)) (= p3 (+ 1 p2))) $x488)))
(let (($x496 (and (=> (and (< 3 z0t) (= z3t 0)) (= p3 (+ 1 p2))) (=> (and (= z3t 1) (= z3p 1) (< 3 z0t)) false))))
(let (($x208 (or false false)))
(let (($x504 (or (and (= z1t 0) $x421 (= 0 q0)) (and $x431 $x421 (= 1 q0)))))
(let (($x505 (or $x504 (or (and (= z1t 0) (= z1p 1) (= 1 q0)) false) $x208)))
(let (($x506 (and $x505 (or $x496 $x492) (or $x479 $x475) (or $x462 $x459) (= p0 0))))
(let (($x512 (=> (< 1 z0p) (and (> z1it 0) (=> (= z1t 0) (= z1it 1))))))
(let (($x521 (=> (< 2 z0p) (and (> z2it 0) (=> (= z2t 0) (= z2it 1))))))
(let (($x530 (=> (< 3 z0p) (and (> z3it 0) (=> (= z3t 0) (= z3it 1))))))
(let (($x534 (and (=> (= z0p 3) (= z3it 0)) $x530 (=> (= z0p 2) (= z2it 0)) $x521 (=> (= z0p 1) (= z1it 0)) $x512)))
(let (($x548 (or (and (= 1 z1t) (= 0 z1p) (= 0 z2t) (= 1 z2p)) (and (= 0 z1t) (= 0 z1p) (= 1 z2t) (= 0 z2p)) (and (= 0 z1t) (= 1 z1p) (= 1 z2t) (= 0 z2p)) (and (= 0 z1t) (= 0 z1p) (= 0 z2t) (= 1 z2p)) (and (= 0 z1t) (= 1 z1p) (= 0 z2t) (= 1 z2p)))))
(let (($x559 (or (and (= 1 z2t) (= 0 z2p) (= 0 z3t) (= 1 z3p)) (and (= 0 z2t) (= 0 z2p) (= 1 z3t) (= 0 z3p)) (and (= 0 z2t) (= 1 z2p) (= 1 z3t) (= 0 z3p)) (and (= 0 z2t) (= 0 z2p) (= 0 z3t) (= 1 z3p)) (and (= 0 z2t) (= 1 z2p) (= 0 z3t) (= 1 z3p)))))
(let (($x576 (and (=> (<= 3 z0p) (or (not (= z1p z3p)) (not (= z1t z3t)))) (=> (<= 2 z0p) (or (not (= z1p z2p)) (not (= z1t z2t)))))))
(let (($x577 (<= 1 z0p)))
(let (($x591 (and (=> (<= 3 z0p) (or (not (= z2p z3p)) (not (= z2t z3t)))) (=> $x577 (or (not (= z2p z1p)) (not (= z2t z1t)))))))
(let (($x567 (<= 2 z0p)))
(let (($x605 (and (=> $x567 (or (not (= z3p z2p)) (not (= z3t z2t)))) (=> $x577 (or (not (= z3p z1p)) (not (= z3t z1t)))))))
(let (($x574 (<= 3 z0p)))
(let (($x614 (and (=> (= 3 z0p) (= z3t 1)) (=> (= 2 z0p) (= z2t 1)) (=> (= 1 z0p) $x431))))
(let (($x630 (and (=> (and $x574 (= z3t 1)) (and (< z3p 1) (>= z3p 0))) (=> (and $x567 (= z2t 1)) (and (< z2p 1) (>= z2p 0))) (=> (and $x577 $x431) (and (< z1p 1) (>= z1p 0))))))
(let (($x643 (and (=> (and $x574 (= z3t 0)) (and (< z3p 2) (>= z3p 0))) (=> (and $x567 (= z2t 0)) (and (< z2p 2) (>= z2p 0))) (=> (and $x577 (= z1t 0)) (and (< z1p 2) (>= z1p 0))))))
(let (($x653 (and (and (<= z3t 1) (<= 0 z3t)) (and (<= z2t 1) (<= 0 z2t)) (and (<= z1t 1) (<= 0 z1t)))))
(let (($x663 (=> (< z0p 1) (and (= z1t z1p) (= z1p z1it) (= z1it 0)))))
(let (($x668 (=> (< z0p 2) (and (= z2t z2p) (= z2p z2it) (= z2it 0)))))
(let (($x673 (=> (< z0p 3) (and (= z3t z3p) (= z3p z3it) (= z3it 0)))))
(let (($x674 (and $x673 $x668 $x663 (and (= z0t z0p) (= z0it z0p) (> z0p 0) (< z0p 4)))))
(let (($x675 (and $x674 $x653 $x643 $x630 $x614 (and (=> $x574 $x605) (=> $x567 $x591) (=> $x577 $x576)) (and (=> (< 2 z0p) $x559) (=> (< 1 z0p) $x548)) $x534)))
(and $x675 $x506 (or $x449))))))))))))))))))))))))))))))))))))))))))))
))
(let (($x378 (or (= 1 q1))))
(and $x378 $x425))))
))
(and (= (+ y0 1) y1) $x678)))
))
(and (and $x682 (= y0 0) $x421)))))
)
(check-sat)
