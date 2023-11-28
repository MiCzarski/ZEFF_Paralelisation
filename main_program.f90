INCLUDE "mod.f90"

SUBROUTINE XYCALCZEFF_LINEAR
    USE PLASMA, ONLY: NE, NI, NITOT, ZEFF
    USE IMPUR_MOD, ONLY: NZ
    USE IMPDAT,    ONLY: LSTTIM, NOIM
    USE NET, ONLY: IMX, IMY
    USE WARIANT, ONLY: INZ
    IMPLICIT NONE
    INTEGER :: I, J, IMI, L
    REAL :: RL1, SUM0, SUM1, SUM2
    
    DO I=1,IMX+1 ! 10
        DO J=1,IMY+1 ! 10
            SUM0=0.
            SUM1=0.
            SUM2=0.
            IF(INZ.EQ.0) THEN
                DO IMI=1,NOIM
                    DO L=2,LSTTIM(IMI)+1 ! 20
                        RL1=REAL(L-1)
                        SUM0=SUM0+NZ(I,J,L,IMI)
                        SUM1=SUM1+NZ(I,J,L,IMI)*RL1
                        SUM2=SUM2+NZ(I,J,L,IMI)*RL1*RL1
                    ENDDO ! 20
                ENDDO
            ENDIF
            NITOT(I,J)=NI(I,J)+SUM0
            NE(I,J)=NI(I,J)+SUM1
            ZEFF(I,J)=(NI(I,J)+SUM2)/NE(I,J)
        ENDDO !10
    ENDDO !10
    ! WRITE(*,*) ZEFF(1, 1)
END SUBROUTINE XYCALCZEFF_LINEAR


SUBROUTINE XYCALCZEFF_PARALLEL
    USE PLASMA, ONLY: NE, NI, NITOT, ZEFF
    USE IMPUR_MOD, ONLY: NZ
    USE IMPDAT,    ONLY: LSTTIM, NOIM
    USE NET, ONLY: IMX, IMY
    USE WARIANT, ONLY: INZ
    IMPLICIT NONE

    REAL, DIMENSION(:), ALLOCATABLE :: NZ_VECTOR, NI_VECTOR
    REAL, DIMENSION(:), ALLOCATABLE :: NITOT_VECTOR, NE_VECTOR, ZEFF_VECTOR
    INTEGER, DIMENSION(:, :), ALLOCATABLE :: NI_INDEXES
    INTEGER, DIMENSION(:, :), ALLOCATABLE :: NZ_INDEXES
    INTEGER :: I, J, L, IMI, Q
    REAL :: SUM0, SUM1, SUM2, RL1sq, RL1
    INTEGER, PARAMETER :: STEPS = 3 ! Set the appropriate number of steps
    INTEGER :: alloc_stat

    
    
    ! Allocate memory for NZVector
    ALLOCATE(NZ_VECTOR(IMX * IMY * SUM(LSTTIM) * NOIM))
    ALLOCATE(NI_VECTOR(IMX * IMY))
    ALLOCATE(NITOT_VECTOR(SIZE(NI_VECTOR)))
    ALLOCATE(NE_VECTOR(SIZE(NI_VECTOR)))
    ALLOCATE(ZEFF_VECTOR(SIZE(NI_VECTOR)))
    ALLOCATE(NI_INDEXES(2, SIZE(NI_VECTOR)))
    ALLOCATE(NZ_INDEXES(4, SIZE(NZ_VECTOR)))

    

    Q = 0
    DO I = 1, IMX
        DO J = 1, IMY
            DO IMI = 1, NOIM
                DO L = 2, LSTTIM(IMI) + 1
           
                    Q = Q + 1
                    NZ_VECTOR(Q) = NZ(I, J, L, IMI)

                    NZ_INDEXES(1, Q) = I
                    NZ_INDEXES(2, Q) = J
                    NZ_INDEXES(3, Q) = L
                    NZ_INDEXES(4, Q) = IMI
                ENDDO
            ENDDO
        ENDDO
    ENDDO


    Q = 0
    DO I = 1, IMX
        DO J = 1, IMY
            Q = Q + 1
            NI_VECTOR(Q) = NI(I, J)
            
            NI_INDEXES(1, Q) = I
            NI_INDEXES(2, Q) = J
        ENDDO
    ENDDO

    ! WRITE(*,*) SIZE(NI_VECTOR), SIZE(NI_INDEXES(1,:)), SIZE(NZ_INDEXES(1,:))

    NITOT_VECTOR = NI_VECTOR
    NE_VECTOR = NI_VECTOR


    IF (INZ == 0) THEN
        I = 1

        SUM0=0.
        SUM1=0.
        SUM2=0.

        Q = 1
        DO Q = 1, SIZE(NZ_VECTOR)
           
            IF (NZ_INDEXES(1, Q) /= NI_INDEXES(1, I) .OR. NZ_INDEXES(2, Q) /= NI_INDEXES(2, I)) THEN
                NITOT_VECTOR(I) = NITOT_VECTOR(I) + SUM0
                NE_VECTOR(I) = NE_VECTOR(I) + SUM1
                ZEFF_VECTOR(I) = (NI_VECTOR(I) / NE_VECTOR(I)) + (SUM2/NE_VECTOR(I))

                SUM0=0.
                SUM1=0.
                SUM2=0.

                I = I + 1
            ENDIF
    
            RL1=REAL(NZ_INDEXES(3, Q)-1)
            
            SUM0=SUM0+NZ_VECTOR(Q)
            SUM1=SUM1+NZ_VECTOR(Q)*RL1
            SUM2=SUM2+NZ_VECTOR(Q)*RL1*RL1

        END DO
        
        NITOT_VECTOR(I) = NITOT_VECTOR(I) + SUM0
        NE_VECTOR(I) = NE_VECTOR(I) + SUM1
        ZEFF_VECTOR(I) = (NI_VECTOR(I) / NE_VECTOR(I)) + (SUM2/NE_VECTOR(I))

    ELSE
        ZEFF_VECTOR = NI_VECTOR / NE_VECTOR
    ENDIF


    ! Reconstruct values from NI_VECTOR using NI_INDEXES
    DO Q = 1, SIZE(NI_VECTOR)
        NI(NI_INDEXES(1, Q), NI_INDEXES(2, Q)) = NI_VECTOR(Q)
        NITOT(NI_INDEXES(1, Q), NI_INDEXES(2, Q)) = NITOT_VECTOR(Q)
        NE(NI_INDEXES(1, Q), NI_INDEXES(2, Q)) = NE_VECTOR(Q)
        ZEFF(NI_INDEXES(1, Q), NI_INDEXES(2, Q)) = ZEFF_VECTOR(Q)
    END DO
    
    

    DEALLOCATE(NZ_VECTOR)
    DEALLOCATE(NZ_INDEXES)

    DEALLOCATE(NI_VECTOR)
    DEALLOCATE(NITOT_VECTOR)
    DEALLOCATE(NE_VECTOR)
    DEALLOCATE(ZEFF_VECTOR)

    DEALLOCATE(NI_INDEXES)


END SUBROUTINE XYCALCZEFF_PARALLEL

SUBROUTINE XYCALCZEFF_PARALLEL_WITH_GRZEGORZ
    USE PLASMA, ONLY: NE, NI, NITOT, ZEFF
    USE IMPUR_MOD, ONLY: NZ
    USE IMPDAT,    ONLY: LSTTIM, NOIM
    USE WARIANT, ONLY: INZ
    IMPLICIT NONE
    INTEGER :: IMI, L, LK, RR
    REAL :: RL1, RL1sq
    INTEGER, DIMENSION(:), ALLOCATABLE :: rr2imi
 
    nitot= 0.0_8
    ne= 0.0_8
    Zeff= 0.0_8
    IF(INZ == 0) THEN
        allocate(rr2imi(sum(lsttim)))
 
        lk= 0
        do imi= 1, noim
            rr2imi(lk + 1 : lk + lsttim(imi))= imi
            lk= lk + lsttim(imi)
        end do
        imi= 0;   l= 2;   rl1= 1.0_8
 
        !$acc parallel loop private(rr, rl1sq) firstprivate(imi, l, rl1) reduction(+: nitot, ne, zeff)
        do rr= 1, lk
            if(imi /= rr2imi(rr)) then
               l= 2;   rl1= 1.0_8;   imi= rr2imi(rr)
            else
               l= l + 1;   rl1= rl1 + 1.0_8
            end if
            RL1sq= RL1 * RL1
            NITOT(:,:)= NITOT(:,:) + NZ(:,:,L,IMI)
            ne(:,:)= ne(:,:) + NZ(:,:,L,IMI) * RL1
            zeff(:,:)= zeff(:,:) + NZ(:,:,L,IMI) * RL1sq
        end do
        !$acc end parallel loop
 
    ENDIF
    NITOT= nitot + NI
    NE= ne + NI
    ZEFF= (Zeff + NI) / ne
END SUBROUTINE XYCALCZEFF_PARALLEL_WITH_GRZEGORZ

SUBROUTINE XYCALCZEFF_PARALLEL_WITH_KRZYSZTOF
    USE PLASMA, ONLY: NE, NI, NITOT, ZEFF
    USE IMPUR_MOD, ONLY: NZ
    USE IMPDAT,    ONLY: LSTTIM, NOIM
    USE WARIANT, ONLY: INZ
    IMPLICIT NONE
    INTEGER :: IMI, L, LK, RR
    REAL :: RL1, RL1sq
 
    NITOT= NI
    NE= NI
    ZEFF= NI

    IF(INZ == 0) THEN
        OVER_IMI: DO IMI=1,NOIM
            OVER_L: DO L=2, LSTTIM(IMI) + 1
                RL1= REAL(L-1)
                RL1SQ= RL1 * RL1
                NITOT= NITOT + NZ(:,:,L,IMI)
                NE= NE + NZ(:,:,L,IMI) * RL1
                ZEFF= ZEFF + NZ(:,:,L,IMI) * RL1SQ
            ENDDO OVER_L
        ENDDO OVER_IMI
    ENDIF

    NITOT= nitot + NI
    NE= ne + NI
    ZEFF= (Zeff + NI) / ne
END SUBROUTINE XYCALCZEFF_PARALLEL_WITH_KRZYSZTOF



PROGRAM MainProgram
    USE TEST_TIME_FUNCTIONS   ! Use the module that contains the function
    USE TEST_CONTENT_FUNCTIONS
    USE WARIANT, ONLY: INZ
    USE IMPDAT, ONLY: NOIM, LSTTIM
    USE NET, ONLY: IMX, IMY
    USE PLASMA, ONLY:  NI
    USE IMPUR_MOD, ONLY: NZ

    USE PLASMA, ONLY: NE, NI, NITOT, ZEFF
    USE IMPUR_MOD, ONLY: NZ
    USE IMPDAT,    ONLY: LSTTIM, NOIM
    USE NET, ONLY: IMX, IMY
    USE WARIANT, ONLY: INZ

    IMPLICIT NONE

    INTEGER ::FSTAT, IMI, I, J, L, LNST1
    REAL ::  UNQNOWN
    CHARACTER(*),parameter :: PATH = "DATA.DAT"
    CHARACTER(*),parameter :: FRMT1="(1P,10E18.8)"  

    INTEGER :: NUMBER_OF_TESTS
    INTEGER :: NUMBER_OF_RUNES_PER_TEST

    NUMBER_OF_TESTS = 100
    NUMBER_OF_RUNES_PER_TEST = 100000
   
    NOIM = 1
    LSTTIM = [6, 0, 0, 0, 0]
    INZ = 0

    IMX = 101
    IMY = 43

    open(99,file=PATH, status='old',IOSTAT=FSTAT)
    IF(FSTAT.NE.0) STOP 'Error on TEXIDAT file open'

    WRITE(*,*) "READING OLD TECXY DATA:"," NI"
    READ(99,FRMT1) ((NI(I,J),I=1,IMX+1),J=1,IMY+1)
    
    READ(99, FRMT1) UNQNOWN

    DO IMI=1,NOIM
       LNST1=LSTTIM(IMI)+1
       WRITE(*,*) "READING OLD TECXY DATA, IMPURITY", IMI,":"," NZ"
       READ(99,FRMT1) (((NZ(I,J,L,IMI),L=1,LNST1),J=1,IMY+1),I=1,IMX+1)
    ENDDO

    close(99)

    CALL SPEED_TEST(NUMBER_OF_TESTS=NUMBER_OF_TESTS, NUMBER_OF_RUNES_PER_TEST=NUMBER_OF_RUNES_PER_TEST, LINEAR=0)
    CALL LOAD_FIRST_RESULT(NITOT, NE, ZEFF)

    open(99,file=PATH, status='old',IOSTAT=FSTAT)
    IF(FSTAT.NE.0) STOP 'Error on TEXIDAT file open'

    WRITE(*,*) "READING OLD TECXY DATA:"," NI"
    READ(99,FRMT1) ((NI(I,J),I=1,IMX+1),J=1,IMY+1)
    
    READ(99, FRMT1) UNQNOWN

    DO IMI=1,NOIM
       LNST1=LSTTIM(IMI)+1
       WRITE(*,*) "READING OLD TECXY DATA, IMPURITY", IMI,":"," NZ"
       READ(99,FRMT1) (((NZ(I,J,L,IMI),L=1,LNST1),J=1,IMY+1),I=1,IMX+1)
    ENDDO

    close(99)

    CALL SPEED_TEST(NUMBER_OF_TESTS=NUMBER_OF_TESTS, NUMBER_OF_RUNES_PER_TEST=NUMBER_OF_RUNES_PER_TEST, LINEAR=1)
    CALL COMPARE(NITOT, NE, ZEFF)


END PROGRAM MainProgram
