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
    INTEGER :: I, J, IMI, L, LK, RR
    REAL :: RL1, SUM0, SUM1, SUM2, RL1sq
    INTEGER, DIMENSION(:), ALLOCATABLE :: rr2imi


    NITOT= NI
    NE= NI
    ZEFF= NI

    IF(INZ == 0) THEN
        allocate(rr2imi(sum(lsttim)))

        lk= 0
        do imi= 1, noim
            rr2imi(lk + 1 : lk + lsttim(imi))= imi
            lk= lk + lsttim(imi)
        end do
        imi= 0

        !$OMP PARALLEL DO PRIVATE(rr, l, rl1, rl1sq) FIRSTPRIVATE(imi) SHARED(nitot, nz, ne, zeff, rr2imi)
        do rr= 1, lk
            if(imi /= rr2imi(rr)) then
            l= 2;   rl1= 2.0_8;   imi= rr2imi(rr)
            else
            l= l + 1;   rl1= rl1 + 1.0_8
            end if
            RL1sq= RL1 * RL1
            NITOT(:,:)= NITOT(:,:) + NZ(:,:,L,IMI)
            ne(:,:)= ne(:,:) + NZ(:,:,L,IMI) * RL1
            zeff(:,:)= zeff(:,:) + NZ(:,:,L,IMI) * RL1sq
        end do
        !$OMP END PARALLEL DO

    ENDIF

    ZEFF= zeff / NE
END SUBROUTINE XYCALCZEFF_PARALLEL


PROGRAM MainProgram
    USE TEST_FUNCTIONS   ! Use the module that contains the function
    USE WARIANT, ONLY: INZ
    USE IMPDAT, ONLY: NOIM, LSTTIM
    USE NET, ONLY: IMX, IMY
    USE PLASMA, ONLY:  NI
    USE IMPUR_MOD, ONLY: NZ

    IMPLICIT NONE

    INTEGER ::FSTAT, IMI, I, J, L, LNST1
    REAL ::  UNKNOWN
    CHARACTER(*),parameter :: PATH = "DATA.DAT"
    CHARACTER(*),parameter :: FRMT1="(1P,10E18.8)"  
   
    NOIM = 1
    LSTTIM = [6, 0, 0, 0, 0]
    INZ = 0

    IMX = 101
    IMY = 43

    open(99,file=PATH, status='old',IOSTAT=FSTAT)
    IF(FSTAT.NE.0) STOP 'Error on TEXIDAT file open'

    WRITE(*,*) "READING OLD TECXY DATA:"," NI"
    READ(99,FRMT1) ((NI(I,J),I=1,IMX+1),J=1,IMY+1)
    
    READ(99, FRMT1) UNKNOWN

    DO IMI=1,NOIM
       LNST1=LSTTIM(IMI)+1
       WRITE(*,*) "READING OLD TECXY DATA, IMPURITY", IMI,":"," NZ"
       READ(99,FRMT1) (((NZ(I,J,L,IMI),L=1,LNST1),J=1,IMY+1),I=1,IMX+1)
    ENDDO

    close(99)

    CALL SPEED_TEST(NUMBER_OF_TESTS=10, NUMBER_OF_RUNES_PER_TEST=10000, LINEAR=1)
   
    NOIM = 1
    LSTTIM = [6, 0, 0, 0, 0]
    INZ = 0

    IMX = 101
    IMY = 43

    open(99,file=PATH, status='old',IOSTAT=FSTAT)
    IF(FSTAT.NE.0) STOP 'Error on TEXIDAT file open'

    WRITE(*,*) "READING OLD TECXY DATA:"," NI"
    READ(99,FRMT1) ((NI(I,J),I=1,IMX+1),J=1,IMY+1)
    
    READ(99, FRMT1) UNKNOWN

    DO IMI=1,NOIM
       LNST1=LSTTIM(IMI)+1
       WRITE(*,*) "READING OLD TECXY DATA, IMPURITY", IMI,":"," NZ"
       READ(99,FRMT1) (((NZ(I,J,L,IMI),L=1,LNST1),J=1,IMY+1),I=1,IMX+1)
    ENDDO

    close(99)

    CALL SPEED_TEST(NUMBER_OF_TESTS=10, NUMBER_OF_RUNES_PER_TEST=10000, LINEAR=0)


END PROGRAM MainProgram