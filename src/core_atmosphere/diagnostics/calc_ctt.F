module calculate_ctt

contains

SUBROUTINE get_ctt(prs, tk, qci, qcw, ctt, haveqci,&
             fill_nocloud, missing, opt_thresh, nz, prsctt)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nz,  haveqci, fill_nocloud
    REAL(KIND=8), DIMENSION(nz), INTENT(IN) :: prs, tk, qci, qcw
    REAL, INTENT(OUT) :: ctt, prsctt
    REAL(KIND=8), INTENT(IN) :: missing
    REAL(KIND=8), INTENT(IN) :: opt_thresh
    REAL(KIND=8), DIMENSION(nz):: pf

    ! LOCAL VARIABLES
    INTEGER i,j,k,ripk
    REAL(KIND=8) :: opdepthu, opdepthd, dp, arg1, fac, ratmix
    REAL(KIND=8) :: arg2, agl_hgt, vt

    REAL(KIND=8) :: p1, p2

   REAL(KIND=8), PARAMETER :: ABSCOEFI = .17D0  ! cloud ice absorption coefficient in m^2/g , was 0.272 as default

! https://www.ecmwf.int/sites/default/files/elibrary/2019/19056-improved-ice-cloud-modeling-capabilities-community-radiative-transfer-model.pdf
   REAL(KIND=8), PARAMETER :: ABSCOEF = .145D0   ! cloud water absorption coefficient in m^2/g
   REAL(KIND=8), PARAMETER :: CELKEL = 273.15D0   ! cloud water absorption coefficient in m^2/g
   REAL(KIND=8), parameter :: gravity = 9.80616D0             !< Constant: Acceleration due to gravity [m s-2]

    DO k=1,nz-1
                ripk = nz-k+1
                pf(k) = .5D0*(prs(ripk) + prs(ripk-1))
    END DO

            opdepthd = 0.D0
            k = 0
            prsctt = -1

            ! Integrate downward from model top, calculating path at full
            ! model vertical levels.

            DO k=2,nz
                opdepthu = opdepthd
                ripk = nz - k + 1

                IF (k .NE. 1) THEN
                    dp = 100.D0*(pf(k) - pf(k-1))  ! should be in Pa
                ELSE
                    dp = 200.D0*(pf(1) - prs(nz))  ! should be in Pa
                END IF

                IF (haveqci .EQ. 0) then
                    IF (tk(ripk) .LT. CELKEL) then
                        ! Note: abscoefi is m**2/g, qcw is g/kg, so no convrsion needed
                        opdepthd = opdepthu + ABSCOEFI*qcw(ripk)*1000.D0 * dp/gravity
                    ELSE
                        opdepthd = opdepthu + ABSCOEF*qcw(ripk)*1000.D0 * dp/gravity
                    END IF
                ELSE
                    opdepthd = opdepthd + (ABSCOEF*qcw(ripk)*1000.D0 + ABSCOEFI*qci(ripk)*1000.D0)*dp/gravity
                END IF

                IF (opdepthd .LT. opt_thresh .AND. k .LT. nz) THEN
                    CYCLE

                ELSE IF (opdepthd .LT. opt_thresh .AND. k .EQ. nz) THEN
                    IF (fill_nocloud .EQ. 0) THEN
                        prsctt = prs(1)
                    ENDIF
                    EXIT
                ELSE
                    fac = (1. - opdepthu)/(opdepthd - opdepthu)
                    prsctt = pf(k-1) + fac*(pf(k) - pf(k-1))
                    prsctt = REAL(MIN(prs(1), MAX(prs(nz), prsctt)))
                    EXIT
                END IF
            END DO

            ! prsctt should only be 0 if fill values are used
            IF (prsctt .GT. -1) THEN
                DO k=2,nz
                    ripk = nz - k + 1
                    p1 = prs(ripk+1)
                    p2 = prs(ripk)
                    IF (prsctt .GE. p1 .AND. prsctt .LE. p2) THEN
                        fac = (prsctt - p1)/(p2 - p1)
                        arg1 = fac*(tk(ripk) - tk(ripk+1)) - CELKEL
                        ctt = REAL(tk(ripk+1) + arg1)
                        EXIT
                    END IF
                END DO
            ELSE
                ctt = REAL(missing)
            END IF

    RETURN

END SUBROUTINE get_ctt

end module calculate_ctt
