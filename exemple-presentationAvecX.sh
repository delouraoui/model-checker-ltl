./check.native -z3 -m Tests/tests_presentation/m_1.fmc -ltl Tests/tests_presentation/f1_1.ltl
#cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"