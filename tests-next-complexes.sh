./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_1.ltl

cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_2.ltl
cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_3.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_4.ltl

cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_5.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : UnSat"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_6.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_7.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_8.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_9.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_10.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_11.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_12.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_13.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_14.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_15.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_complexes/model_1.fmc -ltl Tests/tests_complexes/f1_16.ltl
cvc4 -v  --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

