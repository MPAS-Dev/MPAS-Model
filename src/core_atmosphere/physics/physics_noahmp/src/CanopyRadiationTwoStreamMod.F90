module CanopyRadiationTwoStreamMod

!!! Compute canopy radiative transfer using two-stream approximation of Dickinson (1983) Adv Geophysics
!!! Calculate fluxes absorbed by vegetation, reflected by vegetation, and transmitted through vegetation 
!!! for unit incoming direct or diffuse flux given an underlying ground with known albedo.
!!! Reference for the modified two-stream scheme Niu and Yang (2004), JGR

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine CanopyRadiationTwoStream(noahmp, IndSwBnd, IndSwDif)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: TWOSTREAM
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp
    integer          , intent(in   ) :: IndSwBnd                    ! solar radiation band index
    integer          , intent(in   ) :: IndSwDif                    ! 0=unit incoming direct; 1=unit incoming diffuse

! local variables
    real(kind=kind_noahmp)           :: ScatCoeffCan                ! total scattering coefficient for canopy
    real(kind=kind_noahmp)           :: ScatCoeffLeaf               ! scattering coefficient for leaves not covered by snow
    real(kind=kind_noahmp)           :: UpscatCoeffCanDif           ! upscatter parameter for diffuse radiation
    real(kind=kind_noahmp)           :: UpscatCoeffLeafDif          ! upscatter parameter for diffuse radiation for leaves
    real(kind=kind_noahmp)           :: UpscatCoeffCanDir           ! upscatter parameter for direct radiation
    real(kind=kind_noahmp)           :: UpscatCoeffLeafDir          ! upscatter parameter for direct radiation for leaves
    real(kind=kind_noahmp)           :: OpticDepthDir               ! optical depth of direct beam per unit leaf area
    real(kind=kind_noahmp)           :: OpticDepthDif               ! average diffuse optical depth per unit leaf area
    real(kind=kind_noahmp)           :: CosSolarZenithAngleTmp      ! cosine of solar zenith angle (0.001~1.0)
    real(kind=kind_noahmp)           :: SingleScatAlb               ! single scattering albedo
    real(kind=kind_noahmp)           :: LeafOrientIndex             ! leaf orientation index (-0.4~0.6)
    real(kind=kind_noahmp)           :: RadSwTransDir               ! transmitted direct solar radiation below canopy
    real(kind=kind_noahmp)           :: RadSwTransDif               ! transmitted diffuse solar radiation below canopy
    real(kind=kind_noahmp)           :: RadSwReflTot                ! total reflected flux by canopy and ground
    real(kind=kind_noahmp)           :: VegDensity                  ! vegetation density
    real(kind=kind_noahmp)           :: RadSwReflCan                ! reflected flux by canopy
    real(kind=kind_noahmp)           :: RadSwReflGrd                ! reflected flux by ground
    real(kind=kind_noahmp)           :: CrownDepth                  ! crown depth [m]
    real(kind=kind_noahmp)           :: CrownRadiusVert             ! vertical crown radius [m]
    real(kind=kind_noahmp)           :: SolarAngleTmp               ! solar angle conversion from SZA 
    real(kind=kind_noahmp)           :: FoliageDensity              ! foliage volume density (m-1)
    real(kind=kind_noahmp)           :: VegAreaIndTmp               ! temporary effective VAI
    real(kind=kind_noahmp)           :: Tmp0,Tmp1,Tmp2,Tmp3,Tmp4    ! temporary vars
    real(kind=kind_noahmp)           :: Tmp5,Tmp6,Tmp7,Tmp8,Tmp9    ! temporary vars
    real(kind=kind_noahmp)           :: P1,P2,P3,P4,S1,S2,U1,U2,U3  ! temporary vars
    real(kind=kind_noahmp)           :: B,C,D,D1,D2,F,H,H1,H2,H3    ! temporary vars
    real(kind=kind_noahmp)           :: H4,H5,H6,H7,H8,H9,H10       ! temporary vars 
    real(kind=kind_noahmp)           :: Phi1,Phi2,Sigma             ! temporary vars

! --------------------------------------------------------------------
    associate(                                                        &
              OptCanopyRadiationTransfer => noahmp%config%nmlist%OptCanopyRadiationTransfer ,& ! in,  options for canopy radiation transfer
              CosSolarZenithAngle        => noahmp%config%domain%CosSolarZenithAngle        ,& ! in,  cosine solar zenith angle
              CanopyWetFrac              => noahmp%water%state%CanopyWetFrac                ,& ! in,  wetted or snowed fraction of the canopy
              TreeCrownRadius            => noahmp%energy%param%TreeCrownRadius             ,& ! in,  tree crown radius [m]
              HeightCanopyTop            => noahmp%energy%param%HeightCanopyTop             ,& ! in,  top of canopy [m]
              HeightCanopyBot            => noahmp%energy%param%HeightCanopyBot             ,& ! in,  bottom of canopy [m]
              TreeDensity                => noahmp%energy%param%TreeDensity                 ,& ! in,  tree density [no. of trunks per m2]
              CanopyOrientIndex          => noahmp%energy%param%CanopyOrientIndex           ,& ! in,  leaf/stem orientation index
              ScatterCoeffSnow           => noahmp%energy%param%ScatterCoeffSnow            ,& ! in,  Scattering coefficient for snow
              UpscatterCoeffSnowDir      => noahmp%energy%param%UpscatterCoeffSnowDir       ,& ! in,  Upscattering parameters for snow for direct radiation
              UpscatterCoeffSnowDif      => noahmp%energy%param%UpscatterCoeffSnowDif       ,& ! in,  Upscattering parameters for snow for diffuse radiation
              VegAreaIndEff              => noahmp%energy%state%VegAreaIndEff               ,& ! in,  one-sided leaf+stem area index [m2/m2]
              TemperatureCanopy          => noahmp%energy%state%TemperatureCanopy           ,& ! in,  vegetation temperature [K]
              AlbedoGrdDir               => noahmp%energy%state%AlbedoGrdDir                ,& ! in,  ground albedo (direct beam: vis, nir)
              AlbedoGrdDif               => noahmp%energy%state%AlbedoGrdDif                ,& ! in,  ground albedo (diffuse: vis, nir)
              ReflectanceVeg             => noahmp%energy%state%ReflectanceVeg              ,& ! in,  leaf/stem reflectance weighted by LAI and SAI fraction
              TransmittanceVeg           => noahmp%energy%state%TransmittanceVeg            ,& ! in,  leaf/stem transmittance weighted by LAI and SAI fraction
              VegFrac                    => noahmp%energy%state%VegFrac                     ,& ! in,  greeness vegetation fraction
              AlbedoSfcDir               => noahmp%energy%state%AlbedoSfcDir                ,& ! out, surface albedo (direct)
              AlbedoSfcDif               => noahmp%energy%state%AlbedoSfcDif                ,& ! out, surface albedo (diffuse)
              VegAreaProjDir             => noahmp%energy%state%VegAreaProjDir              ,& ! out, projected leaf+stem area in solar direction
              GapBtwCanopy               => noahmp%energy%state%GapBtwCanopy                ,& ! out, between canopy gap fraction for beam
              GapInCanopy                => noahmp%energy%state%GapInCanopy                 ,& ! out, within canopy gap fraction for beam
              GapCanopyDif               => noahmp%energy%state%GapCanopyDif                ,& ! out, gap fraction for diffue light
              GapCanopyDir               => noahmp%energy%state%GapCanopyDir                ,& ! out, total gap fraction for beam (<=1-VegFrac)
              RadSwAbsVegDir             => noahmp%energy%flux%RadSwAbsVegDir               ,& ! out, flux abs by veg (per unit direct flux)
              RadSwAbsVegDif             => noahmp%energy%flux%RadSwAbsVegDif               ,& ! out, flux abs by veg (per unit diffuse flux)
              RadSwDirTranGrdDir         => noahmp%energy%flux%RadSwDirTranGrdDir           ,& ! out, downward direct flux below veg (per unit dir flux)
              RadSwDirTranGrdDif         => noahmp%energy%flux%RadSwDirTranGrdDif           ,& ! out, downward direct flux below veg per unit dif flux (=0)
              RadSwDifTranGrdDir         => noahmp%energy%flux%RadSwDifTranGrdDir           ,& ! out, downward diffuse flux below veg (per unit dir flux)
              RadSwDifTranGrdDif         => noahmp%energy%flux%RadSwDifTranGrdDif           ,& ! out, downward diffuse flux below veg (per unit dif flux)
              RadSwReflVegDir            => noahmp%energy%flux%RadSwReflVegDir              ,& ! out, flux reflected by veg layer (per unit direct flux)
              RadSwReflVegDif            => noahmp%energy%flux%RadSwReflVegDif              ,& ! out, flux reflected by veg layer (per unit diffuse flux)
              RadSwReflGrdDir            => noahmp%energy%flux%RadSwReflGrdDir              ,& ! out, flux reflected by ground (per unit direct flux)
              RadSwReflGrdDif            => noahmp%energy%flux%RadSwReflGrdDif               & ! out, flux reflected by ground (per unit diffuse flux)
             )
! ----------------------------------------------------------------------

    ! compute within and between gaps
    if ( VegAreaIndEff == 0.0 ) then
       GapCanopyDir = 1.0
       GapCanopyDif = 1.0
    else
       if ( OptCanopyRadiationTransfer == 1 ) then
          VegDensity      = -log(max(1.0-VegFrac, 0.01)) / (ConstPI*TreeCrownRadius**2)
          CrownDepth      = HeightCanopyTop - HeightCanopyBot
          CrownRadiusVert = 0.5 * CrownDepth
          SolarAngleTmp   = atan(CrownRadiusVert / TreeCrownRadius * tan(acos(max(0.01, CosSolarZenithAngle))))
         !GapBtwCanopy    = exp(TreeDensity * ConstPI * TreeCrownRadius**2 / cos(SolarAngleTmp))
          GapBtwCanopy    = exp(-VegDensity * ConstPI * TreeCrownRadius**2 / cos(SolarAngleTmp))
          FoliageDensity  = VegAreaIndEff / (1.33*ConstPI*TreeCrownRadius**3.0 * (CrownRadiusVert/TreeCrownRadius)*VegDensity)
          VegAreaIndTmp   = CrownDepth * FoliageDensity
          GapInCanopy     = (1.0 - GapBtwCanopy) * exp(-0.5*VegAreaIndTmp/CosSolarZenithAngle)
          GapCanopyDir    = min( 1.0-VegFrac, GapBtwCanopy+GapInCanopy )
          GapCanopyDif    = 0.05
       endif
       if ( OptCanopyRadiationTransfer == 2 ) then
          GapCanopyDir    = 0.0
          GapCanopyDif    = 0.0
       endif
       if ( OptCanopyRadiationTransfer == 3 ) then
          GapCanopyDir    = 1.0 - VegFrac
          GapCanopyDif    = 1.0 - VegFrac
       endif
    endif

    ! calculate two-stream parameters ScatCoeffCan, UpscatCoeffCanDir, UpscatCoeffCanDif, OpticDepthDif, VegAreaProjDir, OpticDepthDir.
    ! ScatCoeffCan, UpscatCoeffCanDir, UpscatCoeffCanDif are adjusted for snow. values for ScatCoeffCan*UpscatCoeffCanDir
    ! and ScatCoeffCan*UpscatCoeffCanDif are calculated and then divided by the new ScatCoeffCan
    ! because the product ScatCoeffCan*UpscatCoeffCanDif, ScatCoeffCan*UpscatCoeffCanDir is used in solution.
    ! also, the transmittances and reflectances are linear
    ! weights of leaf and stem values.

    CosSolarZenithAngleTmp  = max( 0.001, CosSolarZenithAngle )
    LeafOrientIndex         = min( max(CanopyOrientIndex, -0.4), 0.6 )
    if ( abs(LeafOrientIndex) <= 0.01 ) LeafOrientIndex = 0.01
    Phi1               = 0.5 - 0.633 * LeafOrientIndex - 0.330 * LeafOrientIndex * LeafOrientIndex
    Phi2               = 0.877 * (1.0 - 2.0 * Phi1)
    VegAreaProjDir     = Phi1 + Phi2 * CosSolarZenithAngleTmp
    OpticDepthDir      = VegAreaProjDir / CosSolarZenithAngleTmp
    OpticDepthDif      = (1.0 - Phi1/Phi2 * log( (Phi1+Phi2) / Phi1 )) / Phi2
    ScatCoeffLeaf      = ReflectanceVeg(IndSwBnd) + TransmittanceVeg(IndSwBnd)
    Tmp0               = VegAreaProjDir + Phi2 * CosSolarZenithAngleTmp
    Tmp1               = Phi1 * CosSolarZenithAngleTmp
    SingleScatAlb      = 0.5 * ScatCoeffLeaf * VegAreaProjDir / Tmp0 * (1.0 - Tmp1/Tmp0 * log((Tmp1+Tmp0)/Tmp1) )
    UpscatCoeffLeafDir = (1.0 + OpticDepthDif * OpticDepthDir) / &
                         (ScatCoeffLeaf * OpticDepthDif * OpticDepthDir) * SingleScatAlb
    UpscatCoeffLeafDif = 0.5 * (ReflectanceVeg(IndSwBnd) + TransmittanceVeg(IndSwBnd) + &
                         (ReflectanceVeg(IndSwBnd)-TransmittanceVeg(IndSwBnd))*((1.0+LeafOrientIndex)/2.0)**2)/ScatCoeffLeaf

    ! adjust omega, betad, and betai for intercepted snow
    if ( TemperatureCanopy > ConstFreezePoint ) then  ! no snow on leaf
       Tmp0 = ScatCoeffLeaf
       Tmp1 = UpscatCoeffLeafDir
       Tmp2 = UpscatCoeffLeafDif
    else
       Tmp0 = (1.0 - CanopyWetFrac) * ScatCoeffLeaf + CanopyWetFrac * ScatterCoeffSnow(IndSwBnd)
       Tmp1 = ((1.0 - CanopyWetFrac) * ScatCoeffLeaf * UpscatCoeffLeafDir + &
               CanopyWetFrac * ScatterCoeffSnow(IndSwBnd) * UpscatterCoeffSnowDir ) / Tmp0 ! direct
       Tmp2 = ((1.0 - CanopyWetFrac) * ScatCoeffLeaf * UpscatCoeffLeafDif + &
               CanopyWetFrac * ScatterCoeffSnow(IndSwBnd) * UpscatterCoeffSnowDif ) / Tmp0 ! diffuse
    endif
    ScatCoeffCan      = Tmp0
    UpscatCoeffCanDir = Tmp1
    UpscatCoeffCanDif = Tmp2

    ! absorbed, reflected, transmitted fluxes per unit incoming radiation
    B     = 1.0 - ScatCoeffCan + ScatCoeffCan * UpscatCoeffCanDif
    C     = ScatCoeffCan * UpscatCoeffCanDif
    Tmp0  = OpticDepthDif * OpticDepthDir
    D     = Tmp0 * ScatCoeffCan * UpscatCoeffCanDir
    F     = Tmp0 * ScatCoeffCan * (1.0 - UpscatCoeffCanDir)
    Tmp1  = B * B - C * C
    H     = sqrt(Tmp1) / OpticDepthDif
    Sigma = Tmp0 * Tmp0 - Tmp1
    if ( abs(Sigma) < 1.0e-6 ) Sigma = sign(1.0e-6, Sigma)
    P1    = B + OpticDepthDif * H
    P2    = B - OpticDepthDif * H
    P3    = B + Tmp0
    P4    = B - Tmp0
    S1    = exp( -H * VegAreaIndEff )
    S2    = exp( -OpticDepthDir * VegAreaIndEff )
    if ( IndSwDif == 0 ) then  ! direct
       U1 = B - C / AlbedoGrdDir(IndSwBnd)
       U2 = B - C * AlbedoGrdDir(IndSwBnd)
       U3 = F + C * AlbedoGrdDir(IndSwBnd)
    else                       ! diffuse
       U1 = B - C / AlbedoGrdDif(IndSwBnd)
       U2 = B - C * AlbedoGrdDif(IndSwBnd)
       U3 = F + C * AlbedoGrdDif(IndSwBnd)
    endif
    Tmp2  = U1 - OpticDepthDif * H
    Tmp3  = U1 + OpticDepthDif * H
    D1    = P1 * Tmp2 / S1 - P2 * Tmp3 * S1
    Tmp4  = U2 + OpticDepthDif * H
    Tmp5  = U2 - OpticDepthDif * H
    D2    = Tmp4 / S1 - Tmp5 * S1
    H1    = -D * P4 - C * F
    Tmp6  = D - H1 * P3 / Sigma
    Tmp7  = ( D - C - H1 / Sigma * (U1+Tmp0) ) * S2
    H2    = ( Tmp6 * Tmp2 / S1 - P2 * Tmp7 ) / D1
    H3    = - ( Tmp6 * Tmp3 * S1 - P1 * Tmp7 ) / D1
    H4    = -F * P3 - C * D
    Tmp8  = H4 / Sigma
    Tmp9  = ( U3 - Tmp8 * (U2-Tmp0) ) * S2
    H5    = - ( Tmp8 * Tmp4 / S1 + Tmp9 ) / D2
    H6    = ( Tmp8 * Tmp5 * S1 + Tmp9 ) / D2
    H7    = (C * Tmp2) / (D1 * S1)
    H8    = (-C * Tmp3 * S1) / D1
    H9    = Tmp4 / (D2 * S1)
    H10   = (-Tmp5 * S1) / D2

    ! downward direct and diffuse fluxes below vegetation Niu and Yang (2004), JGR.
    if ( IndSwDif == 0 ) then  ! direct
       RadSwTransDir = S2 * (1.0 - GapCanopyDir) + GapCanopyDir
       RadSwTransDif = (H4 * S2 / Sigma + H5 * S1 + H6 / S1) * (1.0 - GapCanopyDir)
    else                       ! diffuse
       RadSwTransDir = 0.0
       RadSwTransDif = (H9 * S1 + H10 / S1) * (1.0 - GapCanopyDif) + GapCanopyDif
    endif
    if ( IndSwDif == 0 ) then  ! direct
       RadSwDirTranGrdDir(IndSwBnd) = RadSwTransDir
       RadSwDifTranGrdDir(IndSwBnd) = RadSwTransDif
    else                       ! diffuse
       RadSwDirTranGrdDif(IndSwBnd) = RadSwTransDir
       RadSwDifTranGrdDif(IndSwBnd) = RadSwTransDif
    endif

    ! flux reflected by the surface (veg. and ground)
    if ( IndSwDif == 0 ) then ! direct
       RadSwReflTot = (H1 / Sigma + H2 + H3) * (1.0 - GapCanopyDir) + AlbedoGrdDir(IndSwBnd) * GapCanopyDir
       RadSwReflCan = (H1 / Sigma + H2 + H3) * (1.0 - GapCanopyDir)
       RadSwReflGrd = AlbedoGrdDir(IndSwBnd) * GapCanopyDir
    else                      ! diffuse
       RadSwReflTot = (H7 + H8) * (1.0 - GapCanopyDif) + AlbedoGrdDif(IndSwBnd) * GapCanopyDif
       RadSwReflCan = (H7 + H8) * (1.0 - GapCanopyDif) + AlbedoGrdDif(IndSwBnd) * GapCanopyDif
       RadSwReflGrd = 0
    endif
    if ( IndSwDif == 0 ) then ! direct
       AlbedoSfcDir(IndSwBnd)    = RadSwReflTot
       RadSwReflVegDir(IndSwBnd) = RadSwReflCan
       RadSwReflGrdDir(IndSwBnd) = RadSwReflGrd
    else                      ! diffuse
       AlbedoSfcDif(IndSwBnd)    = RadSwReflTot
       RadSwReflVegDif(IndSwBnd) = RadSwReflCan
       RadSwReflGrdDif(IndSwBnd) = RadSwReflGrd
    endif

    ! flux absorbed by vegetation
    if ( IndSwDif == 0 ) then ! direct
       RadSwAbsVegDir(IndSwBnd) = 1.0 - AlbedoSfcDir(IndSwBnd) - (1.0 - AlbedoGrdDir(IndSwBnd))*RadSwDirTranGrdDir(IndSwBnd) - &
                                  (1.0 - AlbedoGrdDif(IndSwBnd))*RadSwDifTranGrdDir(IndSwBnd)
    else                      ! diffuse
       RadSwAbsVegDif(IndSwBnd) = 1.0 - AlbedoSfcDif(IndSwBnd) - (1.0 - AlbedoGrdDir(IndSwBnd))*RadSwDirTranGrdDif(IndSwBnd) - &
                                  (1.0 - AlbedoGrdDif(IndSwBnd))*RadSwDifTranGrdDif(IndSwBnd)
    endif

    end associate

  end subroutine CanopyRadiationTwoStream

end module CanopyRadiationTwoStreamMod
