MODULE TEST_CONTENT_FUNCTIONS

    REAL, DIMENSION(:,:), ALLOCATABLE :: NITOT_COPY
    REAL, DIMENSION(:,:), ALLOCATABLE :: NE_COPY
    REAL, DIMENSION(:,:), ALLOCATABLE :: ZEFF_COPY

    INTEGER :: NUMBER_OF_DECIMAL_PLACES

    PRIVATE :: NITOT_COPY, NE_COPY, ZEFF_COPY, NUMBER_OF_DECIMAL_PLACES

    

    CONTAINS

    SUBROUTINE LOAD_FIRST_RESULT(NITOT, NE, ZEFF)
        REAL, INTENT(INOUT) :: NITOT(:,:)
        REAL, INTENT(INOUT) :: NE(:,:)
        REAL, INTENT(INOUT) :: ZEFF(:,:)

        NUMBER_OF_DECIMAL_PLACES = 4
        
        ! Copy the input array to the private variable
        ALLOCATE(NITOT_COPY(SIZE(NITOT, 1), SIZE(NITOT, 2)))
        NITOT_COPY = NITOT

        ! Copy the input array to the private variable
        ALLOCATE(NE_COPY(SIZE(NE, 1), SIZE(NE, 2)))
        NE_COPY = NE

        ! Copy the input array to the private variable
        ALLOCATE(ZEFF_COPY(SIZE(ZEFF, 1), SIZE(ZEFF, 2)))
        ZEFF_COPY = ZEFF

        ! Get the dimensions of the array
        NROWS = SIZE(NITOT_COPY, 1)
        NCOLS = SIZE(NITOT_COPY, 2)

        NITOT_COPY = RoundArrayElements(NITOT_COPY)
        NE_COPY = RoundArrayElements(NE_COPY)
        ZEFF_COPY = RoundArrayElements(ZEFF_COPY)

        ! Deallocate the temporary arrays if needed
        ! DEALLOCATE(NITOT_COPY, NE_COPY, ZEFF_COPY)
    END SUBROUTINE LOAD_FIRST_RESULT

    SUBROUTINE COMPARE(NITOT, NE, ZEFF)
        REAL, INTENT(INOUT) :: NITOT(:,:)
        REAL, INTENT(INOUT) :: NE(:,:)
        REAL, INTENT(INOUT) :: ZEFF(:,:)

        WRITE(*,*) "#####################################"
        WRITE(*,*) "NITOT: "

        NITOT = RoundArrayElements(NITOT)

        CALL COMPARE_TABLE(NITOT_COPY, NITOT)

        WRITE(*,*) "#####################################"
        WRITE(*,*) "NE: "

        NE = RoundArrayElements(NE)

        CALL COMPARE_TABLE(NE_COPY, NE)

        WRITE(*,*) "#####################################"
        WRITE(*,*) "ZEFF: "

        ZEFF = RoundArrayElements(ZEFF)

        CALL COMPARE_TABLE(ZEFF_COPY, ZEFF)

    END SUBROUTINE COMPARE

    SUBROUTINE COMPARE_TABLE(TABLE, TABLE_TO_COMPARE)
        REAL, INTENT(IN) :: TABLE(:,:)
        REAL, INTENT(IN) :: TABLE_TO_COMPARE(:,:)
        INTEGER :: i, j, k

        k = 0

        ! Assuming the dimensions of the arrays are the same
        IF (SIZE(TABLE, 1) /= SIZE(TABLE_TO_COMPARE, 1) .OR. SIZE(TABLE, 2) /= SIZE(TABLE_TO_COMPARE, 2)) THEN
            WRITE(*,*) "Error: Arrays have different dimensions."
            RETURN
        END IF

        DO i = 1, SIZE(TABLE, 1)
            DO j = 1, SIZE(TABLE, 2)
                IF (TABLE(i, j) /= TABLE_TO_COMPARE(i, j)) THEN
                    WRITE(*,*) "Arrays are not equal at position (", i, ",", j, ")"
                    WRITE(*,*) TABLE(i, j), TABLE_TO_COMPARE(i, j)
                    WRITE(*,*) ""
                    k = k + 1
                END IF
            END DO
        END DO

    IF (k /= 0) THEN
        WRITE(*,*) "Arrays are not equal, ", k
    ELSE
        WRITE(*,*) "Arrays are equal."
    END IF
    END SUBROUTINE COMPARE_TABLE

    FUNCTION ROUND_NUMBER(number, decimals) RESULT(roundedNumber)
        REAL, INTENT(IN) :: number
        INTEGER, INTENT(IN) :: decimals
        REAL :: roundedNumber
        INTEGER :: exponent
        REAL :: scaledNumber

        ! Check if the number is NaN or ±inf
        IF (isnan(number) .OR. abs(number) == HUGE(0.0)) THEN
            roundedNumber = 0.0
            RETURN
        END IF

        ! Check if the number is zero or negative
        IF (number <= 0.0) THEN
            roundedNumber = 0.0
            RETURN
        END IF

        ! Get the exponent of the number
        exponent = FLOOR(LOG10(number)) + 1

        ! Scale the number to avoid integer overflow during rounding
        
        scaledNumber = number * 10.0**(decimals - exponent)

        ! Round the scaled number and scale it back
        roundedNumber = NINT(scaledNumber) / 10.0**(decimals - exponent)
    END FUNCTION ROUND_NUMBER

    FUNCTION RoundArrayElements(arr) RESULT(roundedArr)
        REAL, DIMENSION(:,:) :: arr
        REAL, DIMENSION(:,:), ALLOCATABLE :: roundedArr
        INTEGER :: NROWS, NCOLS
        INTEGER :: i, j

        ! Get the dimensions of the array
        NROWS = SIZE(arr, 1)
        NCOLS = SIZE(arr, 2)

        ! Allocate the result array
        ALLOCATE(roundedArr(NROWS, NCOLS))

        ! Round all elements of the array
        DO i = 1, NROWS
            DO j = 1, NCOLS
                roundedArr(i, j) = ROUND_NUMBER(arr(i, j), NUMBER_OF_DECIMAL_PLACES)
                ! WRITE(*,*) roundedArr(i, j), arr(i, j)
            END DO
        END DO

    END FUNCTION RoundArrayElements
    
END MODULE TEST_CONTENT_FUNCTIONS
