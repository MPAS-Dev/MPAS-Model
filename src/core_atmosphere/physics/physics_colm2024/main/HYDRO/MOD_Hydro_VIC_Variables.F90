MODULE MOD_Hydro_VIC_Variables
   USE MOD_Precision
   IMPLICIT NONE

   ! /***** Define the number of layers used in VIC *****/
   integer, parameter :: Nlayer = 3           !/**< Number of soil moisture layers in model */
   integer            :: Nfrost = 1           !/**< Number of frost subareas in model */

   ! /***** Define maximum array sizes for model source code *****/
   integer, parameter :: MAX_LAYERS = 3       !/**< maximum number of soil moisture layers */
   integer, parameter :: MAX_FROST_AREAS = 3  !/**< maximum number of frost sub-areas */
   integer, parameter :: MAX_ZWTVMOIST = 11   !/**< maximum number of points in water table vs moisture curve for each soil layer; should include points at lower and upper boundaries of the layer */

   ! /***** colm layers to vic layers *****/
   integer, parameter, dimension(:) :: colm2vic_lay(Nlayer) = [3, 6, 10]   !/**< colm layers to vic layers */

   ! /******************************************************************************
   ! * @brief   This structure stores all soil variables for each layer in the
   ! *          soil column.
   ! *****************************************************************************/
   type layer_data_struct
      real(r8) :: ice(MAX_FROST_AREAS) ! /**< ice content of the frozen sublayer (mm) */
      real(r8) :: moist                ! /**< moisture content of the unfrozen sublayer (mm) */
      real(r8) :: evap                 ! /**< evapotranspiration from soil layer (mm) */
      real(r8) :: zwt                  ! /**< water table position relative to soil surface within the layer (cm) */
   END type layer_data_struct

   ! /******************************************************************************
   ! * @brief   This structure stores soil variables for the complete soil column
   ! *          for each grid cell.
   ! *****************************************************************************/
   type cell_data_struct
      real(r8) :: asat                       ! /**< saturated area fraction */
      real(r8) :: baseflow                   ! /**< baseflow from current cell (mm/TS) */
      real(r8) :: runoff                     ! /**< runoff from current cell (mm/TS) */
      type(layer_data_struct) :: layer(MAX_LAYERS)    ! /**< structure containing soil variables for each layer (see above) */
      !!! for zwt calcaulation, not used
      real(r8) :: zwt                       ! /**< average water table position [cm] - using lowest unsaturated layer */
      real(r8) :: zwt_lumped                ! /**< average water table position [cm] - lumping all layers' moisture together */
   END type cell_data_struct

   ! /******************************************************************************
   ! * @brief   This structure stores the soil parameters for a grid cell.
   ! *****************************************************************************/
   type soil_con_struct
      real(r8) :: frost_fract(MAX_FROST_AREAS) ! /**< spatially distributed frost coverage fractions */
      real(r8) :: max_moist(MAX_LAYERS)        ! /**< Maximum moisture content (mm) per layer */
      real(r8) :: resid_moist(MAX_LAYERS)      ! /**< Residual moisture content of soil layer (mm) */
      real(r8) :: Ksat(MAX_LAYERS)             ! /**< Saturated hydraulic conductivity (mm/day) */
      real(r8) :: expt(MAX_LAYERS)             ! /**< Layer-specific exponent n (=3+2/lambda) in Campbell's equation for hydraulic conductivity, HBH 5.6 */
      !!!! to be calibrated
      real(r8) :: b_infilt                     ! /**< Infiltration parameter */
      real(r8) :: Ds                           ! /**< Fraction of maximum subsurface flow rate */
      real(r8) :: Ws                           ! /**< Fraction of maximum soil moisture */
      real(r8) :: Dsmax                        ! /**< Maximum subsurface flow rate (mm/day) */
      real(r8) :: c                            ! /**< Exponent in ARNO baseflow scheme */
      real(r8) :: depth(MAX_LAYERS)            ! /**< Thickness of each soil moisture layer (m) */
      !!! for zwt calculation, not used
      ! Bubbling pressure, HBH 5.15 (cm)
      real(r8) :: bubble(MAX_LAYERS)
      ! Zwt values in the zwt-v-moist curve for each layer.
      real(r8) :: zwtvmoist_zwt(MAX_LAYERS + 2, MAX_ZWTVMOIST)
      ! Moist values in the zwt-v-moist curve for each layer.
      real(r8) :: zwtvmoist_moist(MAX_LAYERS + 2, MAX_ZWTVMOIST)
   END type soil_con_struct

CONTAINS


   SUBROUTINE vic_para(porsl, theta_r, hksati, bsw, wice_soisno, wliq_soisno, fevpg, rootflux, &
                        b_infilt, Dsmax, Ds, Ws, c, &
                        soil_con, cell)

      USE MOD_Precision
      USE MOD_Vars_Global
      IMPLICIT NONE

      type(soil_con_struct) , intent(inout) :: soil_con
      type(cell_data_struct), intent(inout) :: cell

      real(r8), intent(in) :: porsl(1:nl_soil), theta_r(1:nl_soil), hksati(1:nl_soil), bsw(1:nl_soil)
      real(r8), intent(in) :: wice_soisno(1:nl_soil), wliq_soisno(1:nl_soil)
      real(r8), intent(in) :: fevpg
      real(r8), intent(in) :: rootflux(1:nl_soil)

      real(r8), intent(in) :: b_infilt, Dsmax, Ds, Ws, c
      real(r8) :: soil_tmp(Nlayer), ice_tmp(Nlayer)
      integer  :: lb, lp, k, ilay

      real(r8) :: dltime !int(DEF_simulation_time%timestep)
      !-----------------------END Variable List-------------------------------

      dltime = DEF_simulation_time%timestep

      CALL CoLM2VIC(dz_soi, soil_tmp)
      soil_con%depth = soil_tmp

      CALL CoLM2VIC_weight(porsl, soil_tmp)
      ! convert - to mm
      soil_con%max_moist = soil_tmp*soil_con%depth*1000

      CALL CoLM2VIC_weight(theta_r, soil_tmp)
      ! convert - to mm
      soil_con%resid_moist = soil_tmp*soil_con%depth*1000

      CALL CoLM2VIC_weight(hksati, soil_tmp)
      ! convert mm/s to mm/day
      soil_con%Ksat = soil_tmp*86400

      CALL CoLM2VIC_weight(bsw, soil_tmp)
      ! 2*lambda+3
      soil_con%expt = soil_tmp*2+3

      soil_con%b_infilt = b_infilt
      soil_con%Dsmax    = Dsmax
      soil_con%Ds       = Ds
      soil_con%Ws       = Ws
      soil_con%c        = c

      soil_con%frost_fract = 1
      IF (sum(wice_soisno)>0) THEN
         Nfrost = 3
         DO k = 1, Nfrost
            IF (Nfrost == 1) THEN
               soil_con%frost_fract(k) = 1.0
            ELSEIF (Nfrost == 2) THEN
               soil_con%frost_fract(k) = 0.5
            ELSE
               soil_con%frost_fract(k) = 1.0 / real(Nfrost - 1, kind=8)
               IF (k == 1 .or. k == Nfrost) THEN
                   soil_con%frost_fract(k) = soil_con%frost_fract(k) / 2.0
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      CALL CoLM2VIC(wliq_soisno, soil_tmp)
      DO ilay = 1, Nlayer
         ! mm
         cell%layer(ilay)%moist = soil_tmp(ilay)
      ENDDO

      DO ilay=1, Nlayer
         cell%layer(ilay)%ice(:) = 0
      ENDDO

      IF (sum(wice_soisno)>0) THEN
         DO ilay = 1, Nlayer
            lp = colm2vic_lay(ilay)
            IF (ilay==1) THEN
               lb = 1
            ELSE
               lb = colm2vic_lay(ilay-1)+1
            ENDIF
            CALL VIC_IceLay(lb, lp, wice_soisno(lb:lp), ice_tmp)
            cell%layer(ilay)%ice(:) = ice_tmp
         ENDDO
      ! ELSE
      !    DO ilay = 1, Nlayer
      !       cell%layer(ilay)%ice(:) = 0
      !    ENDDO
      ENDIF

      CALL CoLM2VIC(rootflux, soil_tmp)
      ! mm/s*dltime to convert to  mm
      DO ilay = 1, Nlayer
         cell%layer(ilay)%evap = soil_tmp(ilay)*dltime
      ENDDO
      cell%layer(1)%evap = cell%layer(1)%evap + fevpg*dltime

   END SUBROUTINE vic_para


   SUBROUTINE VIC_IceLay(lb, lp, colm_ice, vic_ice)

   IMPLICIT NONE
   !-----------------------Arguments---------------------------------------
   integer     , intent(in ) :: lb
   integer     , intent(in ) :: lp
   real(kind=8), intent(in ) :: colm_ice(lb:lp)
   real(kind=8), intent(out) :: vic_ice(3)
   !-----------------------Local variables---------------------------------
   integer      :: idx, colm_lay
   real(kind=8) :: totalSum
   real(kind=8) :: multiplier
   real(kind=8) :: ice_tmp(lp-lb+1)
   integer      :: vic_lay=3
   !-----------------------END Variable List-------------------------------

      colm_lay = lp - lb + 1
      ice_tmp  = colm_ice
      totalSum = sum(ice_tmp)

      IF (colm_lay == 1) THEN
         vic_ice = totalSum / vic_lay
      ELSEIF (colm_lay == 2) THEN
         vic_ice(1) = ice_tmp(1) * 2.0 / vic_lay
         vic_ice(3) = ice_tmp(2) * 2.0 / vic_lay
      ELSEIF (colm_lay == 3) THEN
         vic_ice    = ice_tmp
      ELSE
         DO idx = 1, min(int((colm_lay-1)/vic_lay), vic_lay)
            multiplier = merge(1.0, 0.0, colm_lay > idx*vic_lay)
            vic_ice(1) = vic_ice(1) + ice_tmp(idx) * multiplier
            vic_ice(3) = vic_ice(3) + ice_tmp(colm_lay-idx+1) * multiplier
         ENDDO
         multiplier = merge((colm_lay-idx*vic_lay)/vic_lay, 0, colm_lay <= (idx+1)*vic_lay)
         vic_ice(1) = vic_ice(1) + ice_tmp(idx+1) * multiplier
         vic_ice(3) = vic_ice(3) + ice_tmp(colm_lay-idx) * multiplier
      ENDIF
      vic_ice(2) = totalSum - vic_ice(1) - vic_ice(3)

   END SUBROUTINE VIC_Icelay


   SUBROUTINE CoLM2VIC(colm_water, vic_water)

   USE MOD_Vars_Global
   IMPLICIT NONE
   !-----------------------Arguments---------------------------------------
   real, intent(in ) :: colm_water(1:nl_soil)
   real, intent(out) :: vic_water(Nlayer)
   !-----------------------Local variables---------------------------------
   integer :: i_colm, i_vic
   !-----------------------END Variable List-------------------------------

      DO i_vic = 1, Nlayer
         vic_water(i_vic) = 0
         IF (i_vic == 1) THEN
            DO i_colm = 1, colm2vic_lay(i_vic)
               vic_water(i_vic) = vic_water(i_vic) + colm_water(i_colm)
            ENDDO
         ELSE
            DO i_colm = colm2vic_lay(i_vic-1)+1, colm2vic_lay(i_vic)
               vic_water(i_vic) = vic_water(i_vic) + colm_water(i_colm)
            ENDDO
         ENDIF
      ENDDO

   END SUBROUTINE CoLM2VIC


   SUBROUTINE CoLM2VIC_weight(colm_water, vic_water)

   USE MOD_Vars_Global
   IMPLICIT NONE
   !-----------------------Arguments---------------------------------------
   real, intent(in ) :: colm_water(1:nl_soil)
   real, intent(out) :: vic_water(Nlayer)
   !-----------------------Local variables---------------------------------
   integer :: i_colm, i_vic
   !-----------------------END Variable List-------------------------------

      DO i_vic = 1, Nlayer
         vic_water(i_vic) = 0
         IF (i_vic == 1) THEN
            DO i_colm = 1, colm2vic_lay(i_vic)
               vic_water(i_vic) = vic_water(i_vic) + colm_water(i_colm)*dz_soi(i_colm)
            ENDDO
            vic_water(i_vic) = vic_water(i_vic)/sum(dz_soi(1:colm2vic_lay(i_vic)))
         ELSE
            DO i_colm = colm2vic_lay(i_vic-1)+1, colm2vic_lay(i_vic)
               vic_water(i_vic) = vic_water(i_vic) + colm_water(i_colm)*dz_soi(i_colm)
            ENDDO
            vic_water(i_vic) = vic_water(i_vic)/sum(dz_soi(colm2vic_lay(i_vic-1)+1:colm2vic_lay(i_vic)))
         ENDIF
      ENDDO

   END SUBROUTINE CoLM2VIC_weight


   SUBROUTINE VIC2CoLM(colm_water, vic_water)

   USE MOD_Vars_Global
   IMPLICIT NONE
   !-----------------------Arguments---------------------------------------
   real, intent(in   ) :: vic_water(Nlayer)
   real, intent(inout) :: colm_water(1:nl_soil)
   !-----------------------Local variables---------------------------------
   integer :: i_colm, i_vic
   !-----------------------END Variable List-------------------------------

      DO i_vic = 1, Nlayer
         IF (i_vic == 1) THEN
            DO i_colm = 1, colm2vic_lay(i_vic)
               colm_water(i_colm) = vic_water(i_vic)*(dz_soi(i_colm)/sum(dz_soi(1:colm2vic_lay(i_vic))))
            ENDDO
         ELSE
            DO i_colm = colm2vic_lay(i_vic-1)+1, colm2vic_lay(i_vic)
               colm_water(i_colm) = vic_water(i_vic)*(dz_soi(i_colm)/sum(dz_soi(colm2vic_lay(i_vic-1)+1:colm2vic_lay(i_vic))))
            ENDDO
         ENDIF
      ENDDO

   END SUBROUTINE VIC2CoLM


END MODULE MOD_Hydro_VIC_Variables
