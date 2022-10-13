      FUNCTION test_average_floats()
        integer test_average_floats
        real a,b,c,result
        real expected
       a = 12.0
       b = 15.0
       c = 15.0
       expected = (a+b+c)/3
       result = (a+b+c)/3

!       COMPARE RESULT WITH EXPECTED VALUE
!       AND REPORT SUCCESS OR FAIL TO CALLER
        IF(abs(expected - result) < 0.00001) THEN
            test_average_floats=0
        ELSE
            test_average_floats=1
        ENDIF
      END FUNCTION

      program addNumbers
        IMPLICIT NONE
        INTEGER nbfail
        INTEGER :: test_humrel

!       RUN ALL test_* FUNCTIONS AND COUNT FAILURES
        nbfail = 0
!       call test_average_floats()
!       nbfail = nbfail + test_average_floats()


!       PRINT RESULT
        IF(nbfail > 0)THEN
            write(*,*) nbfail, " tests have failed"
        ELSE
            write(*,*) "ALL TESTS HAVE PASSED"
        ENDIF

!       REPORT RESULT TO CALLING PROGRAM
        CALL EXIT(nbfail)


      end program addNumbers


