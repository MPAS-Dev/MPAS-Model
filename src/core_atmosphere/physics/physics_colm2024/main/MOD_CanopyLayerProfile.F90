#include <define.h>

MODULE MOD_CanopyLayerProfile

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER SUBROUTINE/FUNCTIONS:

   PUBLIC :: uprofile, kprofile
   PUBLIC :: uintegral, uintegralz, kintegral
   PUBLIC :: ueffect, ueffectz, fuint, fkint, frd
   PUBLIC :: udiff, kdiff, ufindroots, kfindroots

   PUBLIC :: cal_z0_displa

!-----------------------------------------------------------------------

CONTAINS

   real(r8) FUNCTION uprofile(utop, fc, bee, alpha, z0mg, htop, hbot, z)

   USE MOD_Precision
   USE MOD_FrictionVelocity
   IMPLICIT NONE

   real(r8), intent(in) :: utop
   real(r8), intent(in) :: fc
   real(r8), intent(in) :: bee
   real(r8), intent(in) :: alpha
   real(r8), intent(in) :: z0mg
   real(r8), intent(in) :: htop
   real(r8), intent(in) :: hbot
   real(r8), intent(in) :: z

   real(r8) :: ulog,uexp

      ! A simple version of wind profile based on Dai et al., 2019.
      ! A combination of u of canopy area and bare soil wighted by
      ! their fractional cover.
      !
      ! - Canopy area wind: min(uexp, ulog)  - bare soil: ulog
      ! fc: vegetation fractional cover, bee: free parameter = 1.

      ulog = utop*log(z/z0mg)/log(htop/z0mg)
      uexp = utop*exp(-alpha*(1-(z-hbot)/(htop-hbot)))

      uprofile = bee*fc*min(uexp,ulog) + (1-bee*fc)*ulog

      RETURN
   END FUNCTION uprofile


   ! Exchange coefficient K profile based on Dai et al., 2019.
   real(r8) FUNCTION kprofile(ktop, fc, bee, alpha, &
                     displah, htop, hbot, obu, ustar, z)

   USE MOD_Precision
   USE MOD_FrictionVelocity
   IMPLICIT NONE

   real(r8), parameter :: com1 = 0.4
   real(r8), parameter :: com2 = 0.08

   real(r8), intent(in) :: ktop
   real(r8), intent(in) :: fc
   real(r8), intent(in) :: bee
   real(r8), intent(in) :: alpha
   real(r8), intent(in) :: displah
   real(r8), intent(in) :: htop
   real(r8), intent(in) :: hbot
   real(r8), intent(in) :: obu
   real(r8), intent(in) :: ustar
   real(r8), intent(in) :: z

   real(r8) :: fac
   real(r8) :: kcob, klin, kexp

      klin = ktop*z/htop

      fac  = 1. / (1.+exp(-(displah-com1)/com2))
      kcob = 1. / (fac/klin + (1.-fac)/kmoninobuk(0.,obu,ustar,z))

      kexp     = ktop*exp(-alpha*(htop-z)/(htop-hbot))
      kprofile = 1./( bee*fc/min(kexp,kcob) + (1-bee*fc)/kcob )

      RETURN
   END FUNCTION kprofile


   ! numerical solution for wind profile integration (not used now)
   real(r8) FUNCTION uintegral(utop, fc, bee, alpha, z0mg, htop, hbot)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: utop
   real(r8), intent(in) :: fc
   real(r8), intent(in) :: bee
   real(r8), intent(in) :: alpha
   real(r8), intent(in) :: z0mg
   real(r8), intent(in) :: htop
   real(r8), intent(in) :: hbot

   integer  :: i, n
   real(r8) :: dz, z, u

      ! 09/26/2017: change fixed n -> fixed dz
      dz = 0.001
      n  = int( (htop-hbot) / dz ) + 1

      uintegral = 0.

      DO i = 1, n
         IF (i < n) THEN
            z = htop - (i-0.5)*dz
         ELSE
            dz = htop - hbot - (n-1)*dz
            z  = hbot + 0.5*dz
         ENDIF

         u = uprofile(utop, fc, bee, alpha, z0mg, htop, hbot, z)

         u = max(0._r8, u)
         !uintegral = uintegral + sqrt(u)*dz / (htop-hbot)
         ! 03/04/2020, yuan: NOTE: the above is hard to solve
         !NOTE: The integral cannot be solved analytically after
         !the square root sign of u, and the integral can be approximated
         !directly for u, In this way, there is no need to square
         uintegral = uintegral + u*dz / (htop-hbot)
      ENDDO

      RETURN
   END FUNCTION uintegral


   ! numerical solution for wind profile integration (not used now)
   real(r8) FUNCTION uintegralz(utop, fc, bee, alpha, z0mg, &
                    htop, hbot, ztop, zbot)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: utop
   real(r8), intent(in) :: fc
   real(r8), intent(in) :: bee
   real(r8), intent(in) :: alpha
   real(r8), intent(in) :: z0mg
   real(r8), intent(in) :: htop
   real(r8), intent(in) :: hbot
   real(r8), intent(in) :: ztop
   real(r8), intent(in) :: zbot

   integer  :: i, n
   real(r8) :: dz, z, u

      ! 09/26/2017: change fixed n -> fixed dz
      dz = 0.001
      n  = int( (ztop-zbot) / dz ) + 1

      uintegralz = 0.

      DO i = 1, n
         IF (i < n) THEN
            z = ztop - (i-0.5)*dz
         ELSE
            dz = ztop - zbot - (n-1)*dz
            z  = zbot + 0.5*dz
         ENDIF

         u = uprofile(utop, fc, bee, alpha, z0mg, htop, hbot, z)

         u = max(0._r8, u)
         !uintegral = uintegral + sqrt(u)*dz / (htop-hbot)
         ! 03/04/2020, yuan: NOTE: the above is hard to solve
         !NOTE: The integral cannot be solved analytically after
         !the square root sign of u, and the integral can be approximated
         !directly for u, In this way, there is no need to square
         uintegralz = uintegralz + u*dz / (ztop-zbot)
      ENDDO

      RETURN
   END FUNCTION uintegralz


   real(r8) FUNCTION ueffect(utop, htop, hbot, &
                             z0mg, alpha, bee, fc)
   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: utop
   real(r8), intent(in) :: htop
   real(r8), intent(in) :: hbot
   real(r8), intent(in) :: z0mg
   real(r8), intent(in) :: alpha
   real(r8), intent(in) :: bee
   real(r8), intent(in) :: fc

   real(r8) :: roots(2), uint
   integer  :: rootn

      rootn = 0
      uint  = 0.

      ! The dichotomy method to find the root satisfies a certain accuracy,
      ! assuming that there are at most 2 roots
      CALL ufindroots(htop,hbot,(htop+hbot)/2., &
         utop, htop, hbot, z0mg, alpha, roots, rootn)

      IF (rootn == 0) THEN !no root
         uint = uint + fuint(utop, htop, hbot, &
            htop, hbot, z0mg, alpha, bee, fc)
      ENDIF

      IF (rootn == 1) THEN
         uint = uint + fuint(utop, htop, roots(1), &
            htop, hbot, z0mg, alpha, bee, fc)
         uint = uint + fuint(utop, roots(1), hbot, &
            htop, hbot, z0mg, alpha, bee, fc)
      ENDIF

      IF (rootn == 2) THEN
         uint = uint + fuint(utop, htop,     roots(1), &
            htop, hbot, z0mg, alpha, bee, fc)
         uint = uint + fuint(utop, roots(1), roots(2), &
            htop, hbot, z0mg, alpha, bee, fc)
         uint = uint + fuint(utop, roots(2), hbot,     &
            htop, hbot, z0mg, alpha, bee, fc)
      ENDIF

      ueffect = uint / (htop-hbot)

      RETURN
   END FUNCTION ueffect


   ! Calculate the effective wind speed between ztop and zbot
   real(r8) FUNCTION ueffectz(utop, htop, hbot, &
                ztop, zbot, z0mg, alpha, bee, fc)
   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: utop
   real(r8), intent(in) :: htop
   real(r8), intent(in) :: hbot
   real(r8), intent(in) :: ztop
   real(r8), intent(in) :: zbot
   real(r8), intent(in) :: z0mg
   real(r8), intent(in) :: alpha
   real(r8), intent(in) :: bee
   real(r8), intent(in) :: fc

   real(r8) :: roots(2), uint
   integer  :: rootn

      rootn = 0
      uint  = 0.

      ! The dichotomy method to find the root satisfies a certain accuracy,
      ! assuming that there are at most 2 roots
      CALL ufindroots(ztop,zbot,(ztop+zbot)/2., &
         utop, htop, hbot, z0mg, alpha, roots, rootn)

      IF (rootn == 0) THEN !no root
         uint = uint + fuint(utop, ztop, zbot, &
            htop, hbot, z0mg, alpha, bee, fc)
      ENDIF

      IF (rootn == 1) THEN
         uint = uint + fuint(utop, ztop, roots(1), &
            htop, hbot, z0mg, alpha, bee, fc)
         uint = uint + fuint(utop, roots(1), zbot, &
            htop, hbot, z0mg, alpha, bee, fc)
      ENDIF

      IF (rootn == 2) THEN
         uint = uint + fuint(utop, ztop,     roots(1), &
            htop, hbot, z0mg, alpha, bee, fc)
         uint = uint + fuint(utop, roots(1), roots(2), &
            htop, hbot, z0mg, alpha, bee, fc)
         uint = uint + fuint(utop, roots(2), zbot,     &
            htop, hbot, z0mg, alpha, bee, fc)
      ENDIF

      ueffectz = uint / (ztop-zbot)

      RETURN
   END FUNCTION ueffectz


   real(r8) FUNCTION fuint(utop, ztop, zbot, &
         htop, hbot, z0mg, alpha, bee, fc)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: utop, ztop, zbot
   real(r8), intent(in) :: htop, hbot
   real(r8), intent(in) :: z0mg, alpha
   real(r8), intent(in) :: bee, fc

   ! local variables
   real(r8) :: fuexpint, fulogint

      fulogint = utop/log(htop/z0mg) *&
         (ztop*log(ztop/z0mg) - zbot*log(zbot/z0mg) + zbot - ztop)

      IF (udiff((ztop+zbot)/2.,utop,htop,hbot,z0mg,alpha) <= 0) THEN
         ! uexp is smaller
         fuexpint = utop*(htop-hbot)/alpha*( &
            exp(-alpha*(htop-ztop)/(htop-hbot))-&
            exp(-alpha*(htop-zbot)/(htop-hbot)) )

         fuint = bee*fc*fuexpint + (1.-bee*fc)*fulogint
      ELSE
         ! ulog is smaller
         fuint = fulogint
      ENDIF

      RETURN
   END FUNCTION fuint


   RECURSIVE SUBROUTINE ufindroots(ztop,zbot,zmid, &
      utop, htop, hbot, z0mg, alpha, roots, rootn)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: ztop, zbot, zmid
   real(r8), intent(in) :: utop, htop, hbot
   real(r8), intent(in) :: z0mg, alpha

   real(r8), intent(inout) :: roots(2)
   integer,  intent(inout) :: rootn

   ! local variables
   real(r8) :: udiff_ub, udiff_lb

      udiff_ub = udiff(ztop,utop,htop,hbot,z0mg,alpha)
      udiff_lb = udiff(zmid,utop,htop,hbot,z0mg,alpha)

      IF (udiff_ub*udiff_lb == 0) THEN
         IF (udiff_lb == 0) THEN !root found
            rootn = rootn + 1
            IF (rootn > 2) THEN
               rootn = 2
               print *, "Warning: U root number > 2, only the first 2 are used!"
               RETURN !CALL abort
            ENDIF
            roots(rootn) = zmid
         ENDIF
      ELSEIF (udiff_ub*udiff_lb < 0) THEN
         IF (ztop-zmid < 0.01) THEN
            rootn = rootn + 1 !root found
            IF (rootn > 2) THEN
               rootn = 2
               print *, "Warning: U root number > 2, only the first 2 are used!"
               RETURN !CALL abort
            ENDIF
            roots(rootn) = (ztop+zmid)/2.
         ELSE
            CALL ufindroots(ztop,zmid,(ztop+zmid)/2., &
               utop, htop, hbot, z0mg, alpha, roots, rootn)
         ENDIF
      ENDIF

      udiff_ub = udiff(zmid,utop,htop,hbot,z0mg,alpha)
      udiff_lb = udiff(zbot,utop,htop,hbot,z0mg,alpha)

      IF (udiff_ub*udiff_lb == 0) THEN
         IF (udiff_ub == 0) THEN !root found
            rootn = rootn + 1
            IF (rootn > 2) THEN
               rootn = 2
               print *, "Warning: U root number > 2, only the first 2 are used!"
               RETURN !CALL abort
            ENDIF
            roots(rootn) = zmid
         ENDIF
      ELSEIF (udiff_ub*udiff_lb < 0) THEN
         IF (zmid-zbot < 0.01) THEN
            rootn = rootn + 1 !root found
            IF (rootn > 2) THEN
               rootn = 2
               print *, "Warning: U root number > 2, only the first 2 are used!"
               RETURN !CALL abort
            ENDIF
            roots(rootn) = (zmid+zbot)/2.
         ELSE
            CALL ufindroots(zmid,zbot,(zmid+zbot)/2., &
               utop, htop, hbot, z0mg, alpha, roots, rootn)
         ENDIF
      ENDIF
   END SUBROUTINE ufindroots


   real(r8) FUNCTION udiff(z, utop, htop, hbot, z0mg, alpha)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: z, utop, htop, hbot
   real(r8), intent(in) :: z0mg, alpha

   real(r8) :: uexp, ulog

      uexp = utop*exp(-alpha*(htop-z)/(htop-hbot))
      ulog = utop*log(z/z0mg)/log(htop/z0mg)

      udiff = uexp - ulog

      RETURN
   END FUNCTION udiff


   ! numerical solution for K profile integration (not used now)
   real(r8) FUNCTION kintegral(ktop, fc, bee, alpha, z0mg, &
                     displah, htop, hbot, obu, ustar, ztop, zbot)
   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: ktop
   real(r8), intent(in) :: fc
   real(r8), intent(in) :: bee
   real(r8), intent(in) :: alpha
   real(r8), intent(in) :: z0mg
   real(r8), intent(in) :: displah
   real(r8), intent(in) :: htop
   real(r8), intent(in) :: hbot
   real(r8), intent(in) :: obu
   real(r8), intent(in) :: ustar
   real(r8), intent(in) :: ztop
   real(r8), intent(in) :: zbot

   integer  :: i, n
   real(r8) :: dz, z, k

      kintegral = 0.

      IF (ztop <= zbot) THEN
         RETURN
      ENDIF

      ! 09/26/2017: change fixed n -> fixed dz
      dz = 0.001
      n  = int( (ztop-zbot) / dz ) + 1

      DO i = 1, n
         IF (i < n) THEN
            z  = ztop - (i-0.5)*dz
         ELSE
            dz = ztop - zbot - (n-1)*dz
            z  = zbot + 0.5*dz
         ENDIF

         k = kprofile(ktop, fc, bee, alpha, &
            displah, htop, hbot, obu, ustar, z)

         kintegral = kintegral + 1./k * dz

      ENDDO

      RETURN
   END FUNCTION kintegral


   real(r8) FUNCTION frd(ktop, htop, hbot, &
         ztop, zbot, displah, z0h, obu, ustar, &
         z0mg, alpha, bee, fc)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: ktop, htop, hbot
   real(r8), intent(in) :: ztop, zbot
   real(r8), intent(in) :: displah, z0h, obu, ustar
   real(r8), intent(in) :: z0mg, alpha, bee, fc

   ! local parameters
   real(r8), parameter :: com1 = 0.4
   real(r8), parameter :: com2 = 0.08

   real(r8) :: roots(2), fac, kint
   integer  :: rootn

      rootn = 0
      kint  = 0.

      ! calculate fac
      fac = 1. / (1.+exp(-(displah-com1)/com2))
      roots(:) = 0.

      CALL kfindroots(ztop,zbot,(ztop+zbot)/2., &
         ktop, htop, hbot, obu, ustar, fac, alpha, roots, rootn)

      IF (rootn == 0) THEN !no root
         kint = kint + fkint(ktop, ztop, zbot, htop, hbot, &
            z0h, obu, ustar, fac, alpha, bee, fc)
      ENDIF

      IF (rootn == 1) THEN
         kint = kint + fkint(ktop, ztop, roots(1), htop, hbot, &
            z0h, obu, ustar, fac, alpha, bee, fc)
         kint = kint + fkint(ktop, roots(1), zbot, htop, hbot, &
            z0h, obu, ustar, fac, alpha, bee, fc)
      ENDIF

      IF (rootn == 2) THEN
         kint = kint + fkint(ktop, ztop, roots(1), htop, hbot, &
            z0h, obu, ustar, fac, alpha, bee, fc)
         kint = kint + fkint(ktop, roots(1), roots(2), htop, hbot, &
            z0h, obu, ustar, fac, alpha, bee, fc)
         kint = kint + fkint(ktop, roots(2), zbot, htop, hbot, &
            z0h, obu, ustar, fac, alpha, bee, fc)
      ENDIF

      frd = kint

      RETURN
   END FUNCTION frd


   real(r8) FUNCTION fkint(ktop, ztop, zbot, htop, hbot, &
         z0h, obu, ustar, fac, alpha, bee, fc)

      USE MOD_Precision
      USE MOD_FrictionVelocity
      IMPLICIT NONE

      real(r8), intent(in) :: ktop, ztop, zbot
      real(r8), intent(in) :: htop, hbot
      real(r8), intent(in) :: z0h, obu, ustar, fac, alpha
      real(r8), intent(in) :: bee, fc

      ! local variables
      real(r8) :: fkexpint, fkcobint

      !NOTE:
      ! klin = ktop*z/htop
      ! kcob = 1./(fac/klin + (1.-fac)/kmoninobuk(0.,obu,ustar,z))
      fkcobint = fac*htop/ktop*(log(ztop)-log(zbot)) +&
         (1.-fac)*kintmoninobuk(0.,z0h,obu,ustar,ztop,zbot)

      IF (kdiff((ztop+zbot)/2.,ktop,htop,hbot,obu,ustar,fac,alpha) <= 0) THEN
         ! kexp is smaller
         IF (alpha > 0) THEN
            fkexpint = -(htop-hbot)/alpha/ktop*( &
               exp(alpha*(htop-ztop)/(htop-hbot))-&
               exp(alpha*(htop-zbot)/(htop-hbot)) )
         ELSE
            fkexpint = (ztop-zbot)/ktop
         ENDIF

         fkint = bee*fc*fkexpint + (1.-bee*fc)*fkcobint
      ELSE
         ! kcob is smaller
         fkint = fkcobint
      ENDIF

      RETURN
   END FUNCTION fkint


   RECURSIVE SUBROUTINE kfindroots(ztop,zbot,zmid, &
      ktop, htop, hbot, obu, ustar, fac, alpha, roots, rootn)

   USE MOD_Precision
   IMPLICIT NONE

   real(r8), intent(in) :: ztop, zbot, zmid
   real(r8), intent(in) :: ktop, htop, hbot
   real(r8), intent(in) :: obu, ustar, fac, alpha

   real(r8), intent(inout) :: roots(2)
   integer,  intent(inout) :: rootn

   ! local variables
   real(r8) :: kdiff_ub, kdiff_lb

      ! CALL recursive SUBROUTINE kfindroots
      kdiff_ub = kdiff(ztop,ktop,htop,hbot,obu,ustar,fac,alpha)
      kdiff_lb = kdiff(zmid,ktop,htop,hbot,obu,ustar,fac,alpha)

      IF (kdiff_ub*kdiff_lb == 0) THEN
         IF (kdiff_lb == 0) THEN !root found
            rootn = rootn + 1
            IF (rootn > 2) THEN
               rootn = 2
               print *, "Warning: K root number > 2, only the first 2 are used!"
               RETURN !CALL abort
            ENDIF
            roots(rootn) = zmid
         ENDIF
      ELSEIF (kdiff_ub*kdiff_lb < 0) THEN
         IF (ztop-zmid < 0.01) THEN
            rootn = rootn + 1 !root found
            IF (rootn > 2) THEN
               rootn = 2
               print *, "Warning: K root number > 2, only the first 2 are used!"
               RETURN !CALL abort
            ENDIF
            roots(rootn) = (ztop+zmid)/2.
         ELSE
            CALL kfindroots(ztop,zmid,(ztop+zmid)/2., &
               ktop, htop, hbot, obu, ustar, fac, alpha, roots, rootn)
         ENDIF
      ENDIF

      kdiff_ub = kdiff(zmid,ktop,htop,hbot,obu,ustar,fac,alpha)
      kdiff_lb = kdiff(zbot,ktop,htop,hbot,obu,ustar,fac,alpha)

      IF (kdiff_ub*kdiff_lb == 0) THEN
         IF (kdiff_ub == 0) THEN !root found
            rootn = rootn + 1
            IF (rootn > 2) THEN
               rootn = 2
               print *, "Warning: K root number > 2, only the first 2 are used!"
               RETURN !CALL abort
            ENDIF
            roots(rootn) = zmid
         ENDIF
      ELSEIF (kdiff_ub*kdiff_lb < 0) THEN
         IF (zmid-zbot < 0.01) THEN
            rootn = rootn + 1 !root found
            IF (rootn > 2) THEN
               rootn = 2
               print *, "Warning: K root number > 2, only the first 2 are used!"
               RETURN !CALL abort
            ENDIF
            roots(rootn) = (zmid+zbot)/2.
         ELSE
            CALL kfindroots(zmid,zbot,(zmid+zbot)/2., &
               ktop, htop, hbot, obu, ustar, fac, alpha, roots, rootn)
         ENDIF
      ENDIF
   END SUBROUTINE kfindroots


   real(r8) FUNCTION kdiff(z, ktop, htop, hbot, &
         obu, ustar, fac, alpha)

   USE MOD_Precision
   USE MOD_FrictionVelocity
   IMPLICIT NONE

   real(r8), intent(in) :: z, ktop, htop, hbot
   real(r8), intent(in) :: obu, ustar, fac, alpha

   real(r8) :: kexp, klin, kcob

      kexp = ktop*exp(-alpha*(htop-z)/(htop-hbot))

      klin = ktop*z/htop
      kcob = 1./(fac/klin + (1.-fac)/kmoninobuk(0.,obu,ustar,z))

      kdiff = kexp - kcob

      RETURN
   END FUNCTION kdiff


   SUBROUTINE cal_z0_displa (lai, h, fc, z0, displa)

   USE MOD_Const_Physical, only: vonkar
   IMPLICIT NONE

   real(r8), intent(in)  :: lai
   real(r8), intent(in)  :: h
   real(r8), intent(in)  :: fc
   real(r8), intent(out) :: z0
   real(r8), intent(out) :: displa

   real(r8), parameter :: Cd   = 0.2   !leaf drag coefficient
   real(r8), parameter :: cd1  = 7.5   !a free parameter for d/h calculation, Raupach 1992, 1994
   real(r8), parameter :: psih = 0.193 !psih = ln(cw) - 1 + cw^-1, cw = 2, Raupach 1994

   ! local variables
   real(r8) :: fai, sqrtdragc, temp1, delta , lai0

      ! when assume z0=0.01, displa=0
      ! to calculate lai0, delta displa
      !----------------------------------------------------
      sqrtdragc = -vonkar/(log(0.01/h) - psih)
      sqrtdragc = max(sqrtdragc, 0.0031**0.5)
      IF (sqrtdragc .le. 0.3) THEN
         fai = (sqrtdragc**2-0.003) / 0.3
         fai = min(fai, fc*(1-exp(-20.)))
      ELSE
         fai = 0.29
         print *, "z0m, displa error!"
      ENDIF

      ! calculate delta displa when z0 = 0.01
      lai0  = -log(1.-fai/fc)/0.5
      temp1 = (2.*cd1*fai)**0.5
      delta = -h * ( fc*1.1*log(1. + (Cd*lai0*fc)**0.25) + &
         (1.-fc)*(1.-(1.-exp(-temp1))/temp1) )

      ! calculate z0m, displa
      !----------------------------------------------------
      ! NOTE: potential bug below, only apply for spheric
      ! crowns. For other cases, fc*(...) ==> a*fc*(...)
      fai   = fc*(1. - exp(-0.5*lai))
      sqrtdragc = min( (0.003+0.3*fai)**0.5, 0.3 )
      temp1 = (2.*cd1*fai)**0.5

      IF (lai > lai0) THEN
         displa = delta + h*( &
            (  fc)*1.1*log(1. + (Cd*lai*fc)**0.25) + &
            (1-fc)*(1.-(1.-exp(-temp1))/temp1) )
      ELSE
         displa = h*( &
            (  fc)*1.1*log(1. + (Cd*lai*fc)**0.25) + &
            (1-fc)*(1.-(1.-exp(-temp1))/temp1) )
      ENDIF

      displa = max(displa, 0.)
      z0 = (h-displa) * exp(-vonkar/sqrtdragc + psih)

      IF (z0 < 0.01) THEN
         z0 = 0.01
         displa = 0.
      ENDIF

   END SUBROUTINE cal_z0_displa

END MODULE MOD_CanopyLayerProfile
