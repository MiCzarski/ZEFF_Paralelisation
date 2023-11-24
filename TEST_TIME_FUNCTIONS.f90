MODULE TEST_TIME_FUNCTIONS
    PRIVATE :: I, NUM_TESTS, NUM_OF_RUNES_PER_TEST
    PRIVATE :: RESULTS

    REAL, DIMENSION(:), ALLOCATABLE :: RESULTS
    INTEGER :: I, NUM_TESTS, NUM_OF_RUNES_PER_TEST

    CONTAINS 

    SUBROUTINE SPEED_TEST(NUMBER_OF_TESTS, NUMBER_OF_RUNES_PER_TEST, LINEAR)
        NUM_TESTS = NUMBER_OF_TESTS
        NUM_OF_RUNES_PER_TEST = NUMBER_OF_RUNES_PER_TEST

        ! Allocate an array to store the results
        ALLOCATE(RESULTS(NUM_TESTS))

        CALL CALCULATE_SPEED_TEST(LINEAR)
        CALL PRESENT_RESULTS

        ! Deallocate the array
        DEALLOCATE(RESULTS)

    END SUBROUTINE SPEED_TEST

    SUBROUTINE CALCULATE_SPEED_TEST(LINEAR)
        ! Call the function multiple times and store the results
        DO I = 1, NUM_TESTS
            IF (LINEAR == 0) THEN
                RESULTS(I) = ONE_SPEED_TEST_LINEAR() ! Pass different inputs if needed
            ELSE
                RESULTS(I) = ONE_SPEED_TEST_PARALLEL()
            ENDIF
        END DO

    END SUBROUTINE CALCULATE_SPEED_TEST

    SUBROUTINE PRESENT_RESULTS
        REAL :: AVERAGE, STD_DEV

        ! Print the array of results
        WRITE(*,*)
        WRITE(*,*) "Array of values for ONE_SPEED_TEST:"
        DO I = 1, NUM_TESTS
            WRITE(*,*) "Test", i, ":", RESULTS(i)
        END DO

        ! Calculate average
        AVERAGE = SUM(RESULTS) / REAL(NUM_TESTS)

        ! Calculate standard deviation
        STD_DEV = SQRT(SUM((RESULTS - AVERAGE)**2) / REAL(NUM_TESTS - 1))

        WRITE(*,*)
        WRITE(*,*) "Average:    ", AVERAGE
        WRITE(*,*) "STD_DEV:    ", STD_DEV
        WRITE(*,*)

    END SUBROUTINE PRESENT_RESULTS

    REAL FUNCTION ONE_SPEED_TEST_LINEAR()
        IMPLICIT NONE
        INTEGER :: J
        REAL :: START_TIME, END_TIME

        ! Get the starting time
        CALL CPU_TIME(START_TIME)

        DO J = 1, NUM_OF_RUNES_PER_TEST
            CALL XYCALCZEFF_LINEAR
        END DO

        CALL CPU_TIME(END_TIME)

        ! Calculate the elapsed time
        ONE_SPEED_TEST_LINEAR = END_TIME - START_TIME

    END FUNCTION ONE_SPEED_TEST_LINEAR

    REAL FUNCTION ONE_SPEED_TEST_PARALLEL()
        IMPLICIT NONE
        INTEGER :: J
        REAL :: START_TIME, END_TIME

        ! Get the starting time
        CALL CPU_TIME(START_TIME)

        DO J = 1, NUM_OF_RUNES_PER_TEST
            CALL XYCALCZEFF_PARALLEL
        END DO

        CALL CPU_TIME(END_TIME)

        ! Calculate the elapsed time
        ONE_SPEED_TEST_PARALLEL = END_TIME - START_TIME

    END FUNCTION ONE_SPEED_TEST_PARALLEL

END MODULE TEST_TIME_FUNCTIONS