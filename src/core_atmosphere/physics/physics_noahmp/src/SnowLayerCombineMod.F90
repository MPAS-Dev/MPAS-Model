module SnowLayerCombineMod

!!! Snowpack layer combination process
!!! Update snow ice, snow water, snow thickness, snow temperature

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowLayerWaterComboMod, only: SnowLayerWaterCombo

  implicit none

contains

  subroutine SnowLayerCombine(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: COMBINE
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: I,J,K,L           ! node indices
    integer                          :: NumSnowLayerOld   ! number of snow layer
    integer                          :: IndLayer          ! node index
    integer                          :: IndNeighbor       ! adjacent node selected for combination
    real(kind=kind_noahmp)           :: SnowIceTmp        ! total ice mass in snow
    real(kind=kind_noahmp)           :: SnowLiqTmp        ! total liquid water in snow
    real(kind=kind_noahmp)           :: SnowThickMin(3)   ! minimum thickness of each snow layer
    data SnowThickMin /0.025, 0.025, 0.1/                 ! MB: change limit
    !data SnowThickMin /0.045, 0.05, 0.2/

! --------------------------------------------------------------------
    associate(                                                                       &
              NumSnowLayerNeg        => noahmp%config%domain%NumSnowLayerNeg        ,& ! inout, actual number of snow layers (negative)
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! inout, thickness of snow/soil layers [m]
              SnowDepth              => noahmp%water%state%SnowDepth                ,& ! inout, snow depth [m]
              SnowWaterEquiv         => noahmp%water%state%SnowWaterEquiv           ,& ! inout, snow water equivalent [mm]
              SnowIce                => noahmp%water%state%SnowIce                  ,& ! inout, snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater             ,& ! inout, snow layer liquid water [mm]
              SoilLiqWater           => noahmp%water%state%SoilLiqWater             ,& ! inout, soil liquid moisture [m3/m3]
              SoilIce                => noahmp%water%state%SoilIce                  ,& ! inout, soil ice moisture [m3/m3]
              TemperatureSoilSnow    => noahmp%energy%state%TemperatureSoilSnow     ,& ! inout, snow and soil layer temperature [K]
              PondSfcThinSnwComb     => noahmp%water%state%PondSfcThinSnwComb       ,& ! out,   surface ponding [mm] from liquid in thin snow layer combination
              PondSfcThinSnwTrans    => noahmp%water%state%PondSfcThinSnwTrans       & ! out,   surface ponding [mm] from thin snow liquid during transition from multilayer to no layer
             )
! ----------------------------------------------------------------------

! check and combine small ice content layer
    NumSnowLayerOld = NumSnowLayerNeg

    do J = NumSnowLayerOld+1,0
       if ( SnowIce(J) <= 0.1 ) then
          if ( J /= 0 ) then
             SnowLiqWater(J+1)           = SnowLiqWater(J+1) + SnowLiqWater(J)
             SnowIce(J+1)                = SnowIce(J+1) + SnowIce(J)
             ThicknessSnowSoilLayer(J+1) = ThicknessSnowSoilLayer(J+1) + ThicknessSnowSoilLayer(J)
          else
             if ( NumSnowLayerNeg < -1 ) then    ! MB/KM: change to NumSnowLayerNeg
                SnowLiqWater(J-1)           = SnowLiqWater(J-1) + SnowLiqWater(J)
                SnowIce(J-1)                = SnowIce(J-1) + SnowIce(J)
                ThicknessSnowSoilLayer(J-1) = ThicknessSnowSoilLayer(J-1) + ThicknessSnowSoilLayer(J)
             else
                if ( SnowIce(J) >= 0.0 ) then
                   PondSfcThinSnwComb = SnowLiqWater(J)                ! NumSnowLayerNeg WILL GET SET TO ZERO BELOW; PondSfcThinSnwComb WILL GET 
                   SnowWaterEquiv     = SnowIce(J)                     ! ADDED TO PONDING FROM PHASECHANGE PONDING SHOULD BE
                   SnowDepth          = ThicknessSnowSoilLayer(J)      ! ZERO HERE BECAUSE IT WAS CALCULATED FOR THIN SNOW
                else  ! SnowIce OVER-SUBLIMATED EARLIER
                   PondSfcThinSnwComb = SnowLiqWater(J) + SnowIce(J)
                   if ( PondSfcThinSnwComb < 0.0 ) then                ! IF SnowIce AND SnowLiqWater SUBLIMATES REMOVE FROM SOIL
                      SoilIce(1) = SoilIce(1) + PondSfcThinSnwComb/(ThicknessSnowSoilLayer(1)*1000.0) ! negative SoilIce from oversublimation is adjusted below
                      PondSfcThinSnwComb = 0.0
                   endif
                   SnowWaterEquiv = 0.0
                   SnowDepth      = 0.0
                endif ! if(SnowIce(J) >= 0.0)
                SnowLiqWater(J)   = 0.0
                SnowIce(J)        = 0.0
                ThicknessSnowSoilLayer(J) = 0.0
             endif ! if(NumSnowLayerOld < -1)

             !SoilLiqWater(1) = SoilLiqWater(1) + SnowLiqWater(J)/(ThicknessSnowSoilLayer(1)*1000.0)
             !SoilIce(1)      = SoilIce(1) + SnowIce(J)/(ThicknessSnowSoilLayer(1)*1000.0)
          endif ! if(J /= 0)

          ! shift all elements above this down by one.
          if ( (J > NumSnowLayerNeg+1) .and. (NumSnowLayerNeg < -1) ) then
             do I = J, NumSnowLayerNeg+2, -1
                TemperatureSoilSnow(I)    = TemperatureSoilSnow(I-1)
                SnowLiqWater(I)           = SnowLiqWater(I-1)
                SnowIce(I)                = SnowIce(I-1)
                ThicknessSnowSoilLayer(I) = ThicknessSnowSoilLayer(I-1)
             enddo
          endif
          NumSnowLayerNeg = NumSnowLayerNeg + 1

       endif ! if(SnowIce(J) <= 0.1)
    enddo ! do J

! to conserve water in case of too large surface sublimation
    if ( SoilIce(1) < 0.0) then
       SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
       SoilIce(1)      = 0.0
    endif

    if ( NumSnowLayerNeg ==0 ) return   ! MB: get out if no longer multi-layer

    SnowWaterEquiv = 0.0
    SnowDepth      = 0.0
    SnowIceTmp     = 0.0
    SnowLiqTmp     = 0.0

    do J = NumSnowLayerNeg+1, 0
       SnowWaterEquiv = SnowWaterEquiv + SnowIce(J) + SnowLiqWater(J)
       SnowDepth      = SnowDepth + ThicknessSnowSoilLayer(J)
       SnowIceTmp     = SnowIceTmp + SnowIce(J)
       SnowLiqTmp     = SnowLiqTmp + SnowLiqWater(J)
    enddo

! check the snow depth - all snow gone, the liquid water assumes ponding on soil surface.
    !if ( (SnowDepth < 0.05) .and. (NumSnowLayerNeg < 0) ) then
    if ( (SnowDepth < 0.025) .and. (NumSnowLayerNeg < 0) ) then ! MB: change limit
       NumSnowLayerNeg     = 0
       SnowWaterEquiv      = SnowIceTmp
       PondSfcThinSnwTrans = SnowLiqTmp                ! LIMIT OF NumSnowLayerNeg < 0 MEANS INPUT PONDING
       if ( SnowWaterEquiv <= 0.0 ) SnowDepth = 0.0    ! SHOULD BE ZERO; SEE ABOVE
    endif

! check the snow depth - snow layers combined
    if ( NumSnowLayerNeg < -1 ) then
       NumSnowLayerOld = NumSnowLayerNeg
       IndLayer        = 1
       do I = NumSnowLayerOld+1, 0
          if ( ThicknessSnowSoilLayer(I) < SnowThickMin(IndLayer) ) then
             if ( I == NumSnowLayerNeg+1 ) then
                IndNeighbor = I + 1
             else if ( I == 0 ) then
                IndNeighbor = I - 1
             else
                IndNeighbor = I + 1
                if ( (ThicknessSnowSoilLayer(I-1)+ThicknessSnowSoilLayer(I)) < &
                     (ThicknessSnowSoilLayer(I+1)+ThicknessSnowSoilLayer(I)) ) IndNeighbor = I-1
             endif
             ! Node l and j are combined and stored as node j.
             if ( IndNeighbor > I ) then
                J = IndNeighbor
                L = I
             else
                J = I
                L = IndNeighbor
             endif

             ! update combined snow water & temperature
             call SnowLayerWaterCombo(ThicknessSnowSoilLayer(J), SnowLiqWater(J), SnowIce(J), TemperatureSoilSnow(J), &
                                      ThicknessSnowSoilLayer(L), SnowLiqWater(L), SnowIce(L), TemperatureSoilSnow(L) )

             ! Now shift all elements above this down one.
             if ( (J-1) > (NumSnowLayerNeg+1) ) then
                do K = J-1, NumSnowLayerNeg+2, -1
                   TemperatureSoilSnow(K)    = TemperatureSoilSnow(K-1)
                   SnowIce(K)                = SnowIce(K-1)
                   SnowLiqWater(K)           = SnowLiqWater(K-1)
                   ThicknessSnowSoilLayer(K) = ThicknessSnowSoilLayer(K-1)
                enddo
             endif
             ! Decrease the number of snow layers
             NumSnowLayerNeg = NumSnowLayerNeg + 1
             if ( NumSnowLayerNeg >= -1 ) Exit
          else 
             ! The layer thickness is greater than the prescribed minimum value
             IndLayer = IndLayer + 1
          endif
       enddo
    endif

    end associate

  end subroutine SnowLayerCombine

end module SnowLayerCombineMod
