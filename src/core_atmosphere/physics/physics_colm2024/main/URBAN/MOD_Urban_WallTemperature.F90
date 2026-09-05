#include <define.h>

MODULE MOD_Urban_WallTemperature
!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!
!  The thickness of the wall (including the shady wall and the sunny
!  wall) is read from external data. Just like the soil, it is also
!  divided into 10 layers, with the same thickness set for each layer,
!  and its thermal parameters are also read from external data. Unlike
!  pervious/impervious surfaces, the wall does not consider water
!  accumulation or snow cover, so its thermal properties are completely
!  determined by its own materials. At the same time, it does not
!  consider water transfer, phase change processes, and latent heat
!  exchange.
!
!  Another difference is in the setting of heat exchange for the
!  innermost (bottom) layer. For soil and impervious surfaces, the lack
!  of heat exchange in the bottom layer is considered. However, for
!  walls, the heat exchange between the indoor wall surface air and the
!  innermost layer of the wall is considered. Apart from this, the other
!  aspects and the solution process are similar to the temperature
!  solution for the soil.
!
!  Created by Yongjiu Dai and Hua Yuan, 05/2020
!
!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   PUBLIC :: UrbanWallTem

CONTAINS


   SUBROUTINE UrbanWallTem (deltim,capr,cnfac,&
                            cv_wall,tk_wall,t_wall,dz_wall,z_wall,zi_wall,&
                            twall_inner,lwall,clwall,sabwall,fsenwall,cwalls,tkdz_wall)

!=======================================================================
!  Wall temperatures
!  o Boundary conditions:
!    F = Rnet - Hg - LEg (top),
!    For urban sunwall, shadewall, and wall columns, there is a non-zero
!    heat flux across the bottom "building inner surface" layer and the
!    equations are derived assuming a prescribed or adjusted internal
!    building temperature.  T = T_wall_inner (at the wall inner surface).
!
!  o Wall temperature is predicted from heat conduction in N wall layers
!    and up to 5 snow layers. The thermal conductivities at the
!    interfaces between two neighbor layers (j, j+1) are derived from an
!    assumption that the flux across the interface is equal to that from
!    the node j to the interface and the flux from the interface to the
!    node j+1. The equation is solved using the Crank-Nicholson method
!    and resulted in a tridiagonal system equation.
!
!  o no Phase change
!
!  Original author: Yongjiu Dai, 05/2020
!=======================================================================

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical
   USE MOD_Utils, only: tridia

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8), intent(in) :: deltim               !seconds in a time step [second]
   real(r8), intent(in) :: capr                 !tuning factor to turn first layer T into surface T
   real(r8), intent(in) :: cnfac                !Crank Nicholson factor between 0 and 1

   real(r8), intent(in) :: cv_wall(1:nl_wall)   !heat capacity of urban wall [J/m3/K]
   real(r8), intent(in) :: tk_wall(1:nl_wall)   !thermal conductivity of urban wall [W/m/K]

   real(r8), intent(in) :: dz_wall(1:nl_wall)   !layer thickness [m]
   real(r8), intent(in) :: z_wall (1:nl_wall)   !node depth [m]
   real(r8), intent(in) :: zi_wall(0:nl_wall)   !interface depth [m]

   real(r8), intent(in) :: twall_inner          !temperature at the wall inner surface [K]
   real(r8), intent(in) :: lwall                !atmospheric infrared (longwave) radiation [W/m2]
   real(r8), intent(in) :: clwall               !atmospheric infrared (longwave) radiation [W/m2]
   real(r8), intent(in) :: sabwall              !solar radiation absorbed by wall [W/m2]
   real(r8), intent(in) :: fsenwall             !sensible heat flux from wall [W/m2]
   real(r8), intent(in) :: cwalls               !deriv. of wall energy flux to wall temp [w/m2/k]

   real(r8), intent(inout) :: t_wall(1:nl_wall) !wall layers' temperature [K]
   real(r8), intent(inout) :: tkdz_wall         !inner wall heat flux [w/m2/k]

!-------------------------- Local Variables ----------------------------
   real(r8) wice_wall(1:nl_wall)  !ice lens [kg/m2]
   real(r8) wliq_wall(1:nl_wall)  !liquid water [kg/m2]

   real(r8) cv (1:nl_wall)        !heat capacity [J/(m2 K)]
   real(r8) thk(1:nl_wall)        !thermal conductivity of layer
   real(r8) tk (1:nl_wall)        !thermal conductivity [W/(m K)]

   real(r8) at (1:nl_wall)        !"a" vector for tridiagonal matrix
   real(r8) bt (1:nl_wall)        !"b" vector for tridiagonal matrix
   real(r8) ct (1:nl_wall)        !"c" vector for tridiagonal matrix
   real(r8) rt (1:nl_wall)        !"r" vector for tridiagonal solution

   real(r8) fn (1:nl_wall)        !heat diffusion through the layer interface [W/m2]
   real(r8) fn1(1:nl_wall)        !heat diffusion through the layer interface [W/m2]
   real(r8) fact(1:nl_wall)       !used in computing tridiagonal matrix
   real(r8) dzm                   !used in computing tridiagonal matrix
   real(r8) dzp                   !used in computing tridiagonal matrix

   real(r8) t_wall_bef(1:nl_wall) !wall/snow temperature before update
   real(r8) hs                    !net energy flux into the surface (w/m2)
   real(r8) dhsdt                 !d(hs)/dT

   integer i,j

!-----------------------------------------------------------------------

      wice_wall(1:) = 0.0  !ice lens [kg/m2]
      wliq_wall(1:) = 0.0  !liquid water [kg/m2]

      cv(1:) = cv_wall(1:)*dz_wall(1:)

      thk(1:) = tk_wall(1:)

      DO j = 1, nl_wall-1
         tk(j) = thk(j)*thk(j+1)*(z_wall(j+1)-z_wall(j)) &
               /(thk(j)*(z_wall(j+1)-zi_wall(j))+thk(j+1)*(zi_wall(j)-z_wall(j)))
      ENDDO
      tk(nl_wall) = thk(nl_wall)

! net ground heat flux into the wall surface and its temperature derivative
      hs = sabwall + lwall - fsenwall
      dhsdT = - cwalls + clwall

      t_wall_bef(1:) = t_wall(1:)

      j       = 1
      fact(j) = deltim / cv(j) * dz_wall(j) &
              / (0.5*(z_wall(j)-zi_wall(j-1)+capr*(z_wall(j+1)-zi_wall(j-1))))

      DO j = 1, nl_wall
         fact(j) = deltim/cv(j)
      ENDDO

      DO j = 1, nl_wall - 1
        fn(j) = tk(j)*(t_wall(j+1)-t_wall(j))/(z_wall(j+1)-z_wall(j))
      ENDDO

      j     =  nl_wall
      fn(j) = tk(j)*(twall_inner - cnfac*t_wall(j))/(zi_wall(j)-z_wall(j))
      tkdz_wall= tk(j)/(zi_wall(j)-z_wall(j))

! set up vector r and vectors a, b, c that define tridiagonal matrix
      j     = 1
      dzp   = z_wall(j+1)-z_wall(j)
      at(j) = 0.
      bt(j) = 1+(1.-cnfac)*fact(j)*tk(j)/dzp-fact(j)*dhsdT
      ct(j) =  -(1.-cnfac)*fact(j)*tk(j)/dzp
      rt(j) = t_wall(j) + fact(j)*( hs - dhsdT*t_wall(j) + cnfac*fn(j) )

      DO j = 2, nl_wall - 1
         dzm   = (z_wall(j)-z_wall(j-1))
         dzp   = (z_wall(j+1)-z_wall(j))
         at(j) =   - (1.-cnfac)*fact(j)* tk(j-1)/dzm
         bt(j) = 1.+ (1.-cnfac)*fact(j)*(tk(j)/dzp + tk(j-1)/dzm)
         ct(j) =   - (1.-cnfac)*fact(j)* tk(j)/dzp
         rt(j) = t_wall(j) + cnfac*fact(j)*( fn(j) - fn(j-1) )
      ENDDO

      j     = nl_wall
      dzm   = (z_wall(j)-z_wall(j-1))
      dzp   = (zi_wall(j)-z_wall(j))
      at(j) =   - (1.-cnfac)*fact(j)*tk(j-1)/dzm
      bt(j) = 1.+ (1.-cnfac)*fact(j)*(tk(j-1)/dzm+tk(j)/dzp)
      ct(j) = 0.
      rt(j) = t_wall(j) + fact(j)*(fn(j) - cnfac*fn(j-1))

! solve for t_wall
      i = size(at)
      CALL tridia (i ,at ,bt ,ct ,rt ,t_wall)

      j = nl_wall
      fn1(j) = tk(j)*(twall_inner - cnfac*t_wall(j))/(zi_wall(j)-z_wall(j))

   END SUBROUTINE UrbanWallTem

END MODULE MOD_Urban_WallTemperature
! ---------- EOP ------------
