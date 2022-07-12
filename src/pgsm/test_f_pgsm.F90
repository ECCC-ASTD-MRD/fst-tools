      FUNCTION test_pi_value()
        INTEGER :: test_pi_value
        real expected
        real results

        expected = 3.14159
!       CALL FUNCTION TO TEST
!       call ...
        result = 3.1416

!       COMPARE RESULT WITH EXPECTED VALUE
!       AND REPORT SUCCESS OR FAIL TO CALLER
        IF(abs(expected - result) < 0.001) THEN
            write(*,*), "test_pi_value() SUCCESS"
            test_pi_value=0
        ELSE
            write(*,*), "Wrong value for PI: expected:", &
                expected, "result: ", result
            test_pi_value=1
        ENDIF
      END FUNCTION

      FUNCTION test_answer_to_lifes_question()
        INTEGER :: test_answer_to_lifes_question
        real expected
        real results

        expected = 42
!       CALL FUNCTION TO TEST
!       call ...
        result = 41.98

!       COMPARE RESULT WITH EXPECTED VALUE
!       AND REPORT SUCCESS OR FAIL TO CALLER
        IF(abs(expected - result) < 0.05) THEN
            write(*,*), "test_answer_to_lifes_question() SUCCESS"
            test_answer_to_lifes_question=0
        ELSE
            write(*,*), "Could not calculate the answer to life's &
                question: expected:", expected, "result: ", result
            test_answer_to_lifes_question=1
        ENDIF
      END FUNCTION

      PROGRAM TESTS
        IMPLICIT NONE
        INTEGER nbfail
        INTEGER :: test_pi_value
        INTEGER :: test_answer_to_lifes_question

!       RUN ALL test_* FUNCTIONS AND COUNT FAILURES
        nbfail = 0
        nbfail = nbfail + test_pi_value()
        nbfail = nbfail + test_answer_to_lifes_question()


!       PRINT RESULT
        IF(nbfail > 0)THEN
            write(*,*) nbfail, " tests have failed"
        ELSE
            write(*,*) "ALL TESTS HAVE PASSED"
        ENDIF

!       REPORT RESULT TO CALLING PROGRAM
        CALL EXIT(nbfail)

      END PROGRAM

