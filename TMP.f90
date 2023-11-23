SUBROUTINE XYCALCZEFF_PARALLEL
    USE PLASMA, ONLY: NE, NI, NITOT, ZEFF
    USE IMPUR_MOD, ONLY: NZ
    USE IMPDAT,    ONLY: LSTTIM, NOIM
    USE NET, ONLY: IMX, IMY
    USE WARIANT, ONLY: INZ
    IMPLICIT NONE
    INTEGER :: I, J, IMI, L
    REAL :: RL1, SUM0, SUM1, SUM2, RL1sq

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
    ZEFF= ZEFF / NE
END SUBROUTINE XYCALCZEFF_PARALLEL


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

        !$OMP PARALLEL DO PRIVATE(rr, imi, l, rl1, rl1sq) SHARED(nitot, nz, ne, zeff, rr2imi)
        do rr = 1, lk
            imi = rr2imi(rr)
            
            if (imi /= rr2imi(rr-1)) then
                l = 2
                rl1 = 2.0
            else
                l = l + 1
                rl1 = rl1 + 1.0
            end if

            rl1sq = rl1 * rl1
            nitot(:, :) = nitot(:, :) + nz(:, :, l, imi)
            ne(:, :) = ne(:, :) + nz(:, :, l, imi) * rl1
            zeff(:, :) = zeff(:, :) + nz(:, :, l, imi) * rl1sq
        end do
        !$OMP END PARALLEL DO


    ENDIF

    ZEFF= zeff / NE
END SUBROUTINE XYCALCZEFF_PARALLEL