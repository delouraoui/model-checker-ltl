
./check.native -z3 -m Tests/tests_Complex_Temporelle/model_1.fmc -ltl Tests/tests_Complex_Temporelle/u1_1.ltl
#cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -z3 -m Tests/tests_Complex_Temporelle/model_1.fmc -ltl Tests/tests_Complex_Temporelle/u1_2.ltl
#cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : UnSat"

./check.native -z3 -m Tests/tests_Complex_Temporelle/model_1.fmc -ltl Tests/tests_Complex_Temporelle/u1_3.ltl
#cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -z3 -m Tests/tests_Complex_Temporelle/model_1.fmc -ltl Tests/tests_Complex_Temporelle/u1_4.ltl
#cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -z3 -m Tests/tests_Complex_Temporelle/model_1.fmc -ltl Tests/tests_Complex_Temporelle/u1_5.ltl
#cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"

./check.native -m Tests/tests_Complex_Temporelle/model_1.fmc -ltl Tests/tests_Complex_Temporelle/u1_6.ltl
cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"