./check.native -m Tests/tests_presentation/m_2.fmc -ltl Tests/tests_presentation/f2_1.ltl
cvc4 -v --rewrite-divk Check.smt2
echo "Reponse attendu : SAT"