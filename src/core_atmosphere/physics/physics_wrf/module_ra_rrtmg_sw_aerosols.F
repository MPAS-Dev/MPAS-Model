!=================================================================================================================
!module_ra_rrtmg_sw_aerosols includes subroutine calc_aerosol_rrtmg_sw. subroutine calc_aerosol_rrtmg_sw is called
!from subroutine radiation_sw_from_MPAS in mpas_atmphys_driver_radiation_sw.F. calc_aerosol_rrtmg_sw calculates
!the optical properties (aerosol optical depth,asymmetry factor,and single-scattering albedo) of "water-friendly"
!and "ice-friendly" aerosols from the Thompson cloud microphysics scheme. calc_aerosol_rrtmg_sw was copied from
!from WRF-4.0.2 (see module_radiation_driver.F).
!Laura D. Fowler (laura@ucar.edu) / 2024-05-16.

 module module_ra_rrtmg_sw_aerosols
 use mpas_log
 use mpas_atmphys_functions,only: rslf
 use mpas_atmphys_utilities, only: physics_error_fatal,physics_message
#define FATAL_ERROR(M) call physics_error_fatal(M)
#define WRITE_MESSAGE(M) call physics_message(M)

 implicit none
 private
 public:: calc_aerosol_rrtmg_sw


 contains


!=================================================================================================================
!--------------------------------------------------------------
!       INDICES CONVENTION
!--------------------------------------------------------------       
!       kms:kme define the range for full-level indices
!       kts:kte define the range for half-level indices
!       
!       kms=1 is the first full level at surface
!       kts=1 is the first half level at surface
!       
!       kme is the last full level at toa
!       kte is the last half level at toa
!       
!       There is one more full level than half levels.
!       Therefore, kme=kte+1. I checked it in one of my
!       simulations:
!       
!         namelist.input:
!              s_vert=1  e_vert=28        
!         code:
!              kms= 1    kts= 1
!              kms=28    kte=27
!       
!       In the vertical dimension there is no tiling for
!       parallelization as in the horizontal dimensions.
!       For i-dim and j-dim, the t-indices define the
!       range of indices over which each tile runs.
!--------------------------------------------------------------
!
!  namelist options:
!    aer_aod550_opt = [1,2] : 
!        1 = input constant value for AOD at 550 nm from namelist. 
!            In this case, the value is read from aer_aod550_val; 
!        2 = input value from auxiliary input 15. It is a time-varying 2D grid in netcdf wrf-compatible 
!            format. The default operation is aer_aod550_opt=1 and aer_aod550_val=0.12
!    aer_angexp_opt = [1,2,3] : 
!        1 = input constant value for Angstrom exponent from namelist. In this case, the value is read 
!            from aer_angexp_val; 
!        2 = input value from auxiliary input 15, as in aer_aod550_opt; 
!        3 = Angstrom exponent value estimated from the aerosol type defined in aer_type, and modulated 
!            with the RH in WRF. Default operation is aer_angexp_opt = 1, and aer_angexp_val=1.3.
!    aer_ssa_opt and aer_asy_opt are similar to aer_angexp_opt.
!
!    aer_type = [1,2,3] : 1 for rural, 2 is urban and 3 is maritime.
!--------------------------------------------------------------

subroutine calc_aerosol_rrtmg_sw(ht,dz8w,p,t3d,qv3d,aer_type,                               &
                                 aer_aod550_opt, aer_angexp_opt, aer_ssa_opt, aer_asy_opt,  &
                                 aer_aod550_val, aer_angexp_val, aer_ssa_val, aer_asy_val,  &
                                 aod5502d, angexp2d, aerssa2d, aerasy2d,                    &
                                 ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte,           &
                                 tauaer, ssaaer, asyaer, aod5503d                           )

    ! constants
    integer, parameter :: N_BANDS=14
    ! local index variables
    integer :: i,j,k,nb

    real :: lower_wvl(N_BANDS),upper_wvl(N_BANDS)
    data (lower_wvl(i),i=1,N_BANDS) /3.077,2.500,2.150,1.942,1.626,1.299,1.242,0.7782,0.6250,0.4415,0.3448,0.2632,0.2000,3.846/
    data (upper_wvl(i),i=1,N_BANDS) /3.846,3.077,2.500,2.150,1.942,1.626,1.299,1.2420,0.7782,0.6250,0.4415,0.3448,0.2632,12.195/

    ! I/O variables
    integer, intent(in) :: ims,ime,jms,jme,kms,kme, &
                           its,ite,jts,jte,kts,kte

    real, dimension(ims:ime, kms:kme, jms:jme), intent(in) :: p,     & ! pressure (Pa)
                                                              t3d,   & ! temperature (K)
                                                              dz8w,  & ! dz between full levels (m)
                                                              qv3d     ! water vapor mixing ratio (kg/kg)
!-- MPAS modifications: aer_type is a function of the land-sea mask, and set to 1 over land (or rural classification in WRF),
!   and set to 0 over oceans (or maritime classification in WRF):
!   integer, intent(in) :: aer_type
    integer, dimension(ims:ime,jms:jme), intent(in):: aer_type
    character(len=256):: wrf_err_message
!-- end MPAS modifications..
    integer, intent(in) :: aer_aod550_opt, aer_angexp_opt, aer_ssa_opt, aer_asy_opt
    real, intent(in)    :: aer_aod550_val, aer_angexp_val, aer_ssa_val, aer_asy_val

    real, dimension(ims:ime, jms:jme), intent(in)    :: ht
    real, dimension(ims:ime, jms:jme), optional, intent(inout) :: aod5502d, angexp2d, aerssa2d, aerasy2d
    real, dimension(ims:ime, kms:kme, jms:jme, 1:N_BANDS), intent(inout) :: tauaer, ssaaer, asyaer

    real, dimension(ims:ime, kms:kme, jms:jme), optional, intent(in) :: aod5503d   ! trude

    ! local variables
    real :: angexp_val,aod_rate,x,xy,xx
    real, dimension(ims:ime, jms:jme, 1:N_BANDS) :: aod550spc
    real, dimension(ims:ime, kms:kme, jms:jme, 1:N_BANDS) :: aod550spc3d    !  trude
    real, dimension(ims:ime, kms:kme, jms:jme)   :: rh  ! relative humidity

    call calc_relative_humidity(p,t3d,qv3d,                 &
                                ims,ime,jms,jme,kms,kme,    &
                                its,ite,jts,jte,kts,kte,    &
                                rh                          )

    aer_aod550_opt_select: select case(aer_aod550_opt)
       !case(0)
       ! reserved for climatology
       case(1)
          if (aer_aod550_val .lt. 0) then
             write(wrf_err_message,'("aer_aod550_val must be positive. Negative value ",F7.4," found")') aer_aod550_val
             FATAL_ERROR(trim(wrf_err_message))
          end if
          write( wrf_err_message, '("aer_aod550_opt=",I1,": AOD@550 nm fixed to value ",F6.3)') aer_aod550_opt,aer_aod550_val
          WRITE_MESSAGE(trim(wrf_err_message))
          do j=jts,jte
             do i=its,ite
                aod5502d(i,j)=aer_aod550_val
             end do
          end do

       case(2) 
          if (.not.(present(aod5502d))) then
             write(wrf_err_message,*) 'Expected gridded total AOD@550 nm, but it is not in the radiation driver'
             FATAL_ERROR(trim(wrf_err_message))
          end if
          if (minval(aod5502d) .lt. 0) then
             FATAL_ERROR('AOD@550 must be positive. Negative value(s) found in auxinput')
          end if
!         call mpas_log_write('--- aer_aod550_opt = $i: AOD@550 nm read from auxinput min = $r  max = $r', &
!                             intArgs=(/aer_aod550_opt/),realArgs=(/minval(aod5502d(its:ite,jts:jte)), &
!                             maxval(aod5502d(its:ite,jts:jte))/))
       case default
          write(wrf_err_message,*) 'Expected aer_aod550_opt=[1,2]. Got',aer_aod550_opt
          FATAL_ERROR(trim(wrf_err_message))
    end select aer_aod550_opt_select


    ! here, the 3d aod550 is calculated according to the aer_angexp_opt case
    aer_angexp_opt_select: select case(aer_angexp_opt)
       !case(0)
       ! reserved for climatology
       case(1)
          if (aer_angexp_val .lt. -0.3) then
             write(wrf_err_message,'("WARNING: aer_angexp_val limited to -0.3. Illegal value ",F7.4," found")') aer_angexp_val
             WRITE_MESSAGE(trim(wrf_err_message))
          end if
          if (aer_angexp_val .gt. 2.5) then
             write(wrf_err_message,'("WARNING: aer_angexp_val limited to 2.5. Illegal value ",F7.4," found")') aer_angexp_val
             WRITE_MESSAGE(trim(wrf_err_message))
          end if
          write( wrf_err_message , '("aer_angexp_opt=",I1,": Aerosol Angstrom exponent fixed to value ",F6.3)') &
                                      aer_angexp_opt,aer_angexp_val
          WRITE_MESSAGE(trim(wrf_err_message))
          angexp_val=min(2.5,max(-0.3,aer_angexp_val))
          do nb=1,N_BANDS
             if ((angexp_val .lt. 0.999) .or. (angexp_val .gt. 1.001)) then
                aod_rate=((0.55**angexp_val)*(upper_wvl(nb)**(1.-angexp_val)- &
                          lower_wvl(nb)**(1.-angexp_val)))/((1.-angexp_val)*(upper_wvl(nb)-lower_wvl(nb)))
             else
                aod_rate=(0.55/(upper_wvl(nb)-lower_wvl(nb)))*log(upper_wvl(nb)/lower_wvl(nb))
             end if
             do j=jts,jte
                do i=its,ite
                   aod550spc(i,j,nb)=aod5502d(i,j)*aod_rate
                end do
             end do
          end do
          do j=jts,jte
             do i=its,ite
                angexp2d(i,j)=angexp_val
             end do
          end do
       case(2)
          if (.not.(present(angexp2d))) then
             write(wrf_err_message,*) 'Expected gridded aerosol Angstrom exponent, but it is not in the radiation driver'
             FATAL_ERROR(trim(wrf_err_message))
          end if
          write( wrf_err_message, '("aer_angexp_opt=",I1,": Angstrom exponent read from auxinput (min=",F6.3," max=",F6.3,")")') &
                                  aer_angexp_opt,minval(angexp2d),maxval(angexp2d)
          WRITE_MESSAGE(trim(wrf_err_message))
          do j=jts,jte
             do i=its,ite
                angexp_val=min(2.5,max(-0.3,angexp2d(i,j)))
                do nb=1,N_BANDS
                   if ((angexp_val .lt. 0.999) .or. (angexp_val .gt. 1.001)) then
                      aod_rate=((0.55**angexp_val)*(upper_wvl(nb)**(1.-angexp_val)- &
                                lower_wvl(nb)**(1.-angexp_val)))/((1.-angexp_val)*(upper_wvl(nb)-lower_wvl(nb)))
                   else
                      aod_rate=(0.55/(upper_wvl(nb)-lower_wvl(nb)))*log(upper_wvl(nb)/lower_wvl(nb))
                   end if
                   aod550spc(i,j,nb)=aod5502d(i,j)*aod_rate
                end do
             end do
          end do

       case(3)
          ! spectral disaggregation based on a prescribed aerosol type and relative humidity
!         call mpas_log_write('--- aer_angexp_opt = $i: angstrom exponent calculated from RH and aer_type $i', &
!                             intArgs=(/aer_angexp_opt,aer_type/))
          call calc_spectral_aod_rrtmg_sw(ims,ime,jms,jme,kms,kme,      &
                                          its,ite,jts,jte,kts,kte,      &
                                          rh,aer_type,aod5502d,         &
                                          aod550spc,                    &
                                          aod5503d, aod550spc3d)         ! trude

!-- MPAS modifications: we do not need the variable angexp2d outside of subroutine calc_aerosol_rrtmg_sw. Since it is
!   declared as an optional variable, we simply test if it is present or not (Laura D. Fowler/2019-01-13):
          if(present(angexp2d)) then
          do j=jts,jte
            do i=its,ite
              angexp2d(i,j) = 0.0
            enddo
          enddo

          if (present(aod5503d)) then
            do j=jts,jte
             do k=kts,kte
              do i=its,ite
                xy=0
                xx=0
                do nb=8,N_BANDS-3  ! bands between 0.4 and  1.0 um
                   ! the slope of a linear regression with intercept=0 is m=E(xy)/E(x^2), where y=m*x
                   x=log(0.5*(lower_wvl(nb)+upper_wvl(nb))/0.55)
                   xy=xy+x*log(aod550spc3d(i,k,j,nb)/aod5503d(i,k,j))
                   xx=xx+x*x
                end do
                angexp2d(i,j) = angexp2d(i,j) - (xy/(N_BANDS-3-8+1))/(xx/(N_BANDS-3-8+1))
              enddo
             enddo
            enddo
          else

          ! added July, 16th, 2013: angexp2d is in the wrfout when aer_angexp_opt=3. It is the average
          ! value in the spectral bands between 0.4 and 1. um
          do j=jts,jte
             do i=its,ite
                xy=0
                xx=0
                do nb=8,N_BANDS-3  ! bands between 0.4 and  1.0 um
                   ! the slope of a linear regression with intercept=0 is m=E(xy)/E(x^2), where y=m*x
                   x=log(0.5*(lower_wvl(nb)+upper_wvl(nb))/0.55)
                   xy=xy+x*log(aod550spc(i,j,nb)/aod5502d(i,j))
                   xx=xx+x*x
                end do
                angexp2d(i,j)=-(xy/(N_BANDS-3-8+1))/(xx/(N_BANDS-3-8+1))
             end do
          end do
          endif
       endif ! end MPAS modifications.

       case default
          write(wrf_err_message,*) 'Expected aer_angexp_opt=[1,2,3]. Got',aer_angexp_opt
          FATAL_ERROR(trim(wrf_err_message))
    end select aer_angexp_opt_select

!..If 3D AOD (at 550nm) was provided explicitly, then no need to assume a
!.. vertical distribution, just use what was provided.  (Trude)

      if (present(aod5503d)) then
         do nb=1,N_BANDS
          do j=jts,jte
           do k=kts,kte
            do i=its,ite
               tauaer(i,k,j,nb) = aod550spc3d(i,k,j,nb)
            enddo
           enddo
          enddo
         enddo
      else
         ! exponental -vertical- profile
         call aod_profiler(ht,dz8w,aod550spc,n_bands,ims,ime,jms,jme,kms,kme, &
                      its,ite,jts,jte,kts,kte,tauaer                     )
      endif

    aer_ssa_opt_select: select case(aer_ssa_opt)
       !case(0)
       ! reserved for climatology
       case(1)
          if ((aer_ssa_val .lt. 0) .or. (aer_ssa_val .gt. 1)) then
             write(wrf_err_message,'("aer_ssa_val must be within [0,1]. Illegal value ",F7.4," found")') aer_ssa_val
             FATAL_ERROR(trim(wrf_err_message))
          end if
          write( wrf_err_message, &
                '("aer_ssa_opt=",I1,": single-scattering albedo fixed to value ",F6.3)') aer_ssa_opt,aer_ssa_val
          WRITE_MESSAGE(trim(wrf_err_message))
          do j=jts,jte
             do i=its,ite
                do k=kts,kte
                   do nb=1,N_BANDS
                      ! no spectral disaggregation
                      ssaaer(i,k,j,nb)=aer_ssa_val
                   end do
                end do
             end do
          end do
          do j=jts,jte
             do i=its,ite
                aerssa2d(i,j)=aer_ssa_val
             end do
          end do

       case(2)
          if (.not.(present(aerssa2d))) then
             write(wrf_err_message,*) 'Expected gridded aerosol single-scattering albedo, but it is not in the radiation driver'
             FATAL_ERROR(trim(wrf_err_message))
          end if
          if ((minval(aerssa2d) .lt. 0) .or. (maxval(aerssa2d) .gt. 1)) then
             write(wrf_err_message,*) 'Aerosol single-scattering albedo must be within [0,1]. ' // &
                                      'Out of bounds value(s) found in auxinput'
             FATAL_ERROR(trim(wrf_err_message))
          end if
          write( wrf_err_message, '("aer_ssa_opt=",I1,": single-scattering albedo from auxinput (min=",F6.3," max=",F6.3,")")') &
                                  aer_ssa_opt,minval(aerssa2d),maxval(aerssa2d)
          WRITE_MESSAGE(trim(wrf_err_message))
          do j=jts,jte
             do i=its,ite
                do k=kts,kte
                   do nb=1,N_BANDS
                      ! no spectral disaggregation
                      ssaaer(i,k,j,nb)=aerssa2d(i,j)
                   end do
                end do
             end do
          end do

       case(3)
          ! spectral disaggregation based on a prescribed aerosol type and relative humidity
!         call mpas_log_write('--- aer_ssa_opt    = $i: single-scattering albedo calculated from RH and aer_type $i', &
!                             intArgs=(/aer_ssa_opt,aer_type/))
          call calc_spectral_ssa_rrtmg_sw(ims,ime,jms,jme,kms,kme,  &
                                          its,ite,jts,jte,kts,kte,  &
                                          rh,aer_type,ssaaer        )
!-- MPAS modifications: we do not need the variable aerssa2d outside of subroutine calc_aerosol_rrtmg_sw. Since it is
!   declared as an optional variable, we simply test if it is present or not (Laura D. Fowler/2018=04-09):
          if(present(aerssa2d)) then
          ! added July, 16th, 2013: aerssa2d is in the wrfout when aer_ssa_opt=3. It is the average
          ! value in the spectral bands between 0.4 and 1. um
          do j=jts,jte
             do i=its,ite
                aerssa2d(i,j)=0
             end do
          end do
          do j=jts,jte
             do i=its,ite
                do nb=8,N_BANDS-3  ! bands between 0.4 and 1.0 um
                   aerssa2d(i,j)=aerssa2d(i,j)+ssaaer(i,kts,j,nb)
                end do
                aerssa2d(i,j)=aerssa2d(i,j)/(N_BANDS-3-8+1)
             end do
          end do
          endif ! end MPAS modifications.

       case default
          write(wrf_err_message,*) 'Expected aer_ssa_opt=[1,2,3]. Got',aer_ssa_opt
          FATAL_ERROR(trim(wrf_err_message))
    end select aer_ssa_opt_select

    aer_asy_opt_select: select case(aer_asy_opt)
       !case(0)
       ! reserved for climatology
       case(1)
          if ((aer_asy_val .lt. 0) .or. (aer_asy_val .gt. 1)) then
             write(wrf_err_message,'("aer_asy_val must be withing [-1,1]. Illegal value ",F7.4," found")') aer_asy_val
             FATAL_ERROR(trim(wrf_err_message))
          end if
          write( wrf_err_message , '("aer_asy_opt=",I1,": asymmetry parameter fixed to value ",F6.3)') aer_asy_opt,aer_asy_val
          WRITE_MESSAGE(trim(wrf_err_message))
          do j=jts,jte
             do i=its,ite
                do k=kts,kte
                   do nb=1,N_BANDS
                      asyaer(i,k,j,nb)=aer_asy_val
                   end do
                end do
             end do
          end do
          do j=jts,jte
             do i=its,ite
                aerasy2d(i,j)=aer_asy_val
             end do
          end do

       case(2)
          if (.not.(present(aerasy2d))) then
             write(wrf_err_message,*) 'Expected gridded aerosol asymmetry parameter, but it is not in the radiation driver'
             FATAL_ERROR(trim(wrf_err_message))
          end if
          if ((minval(aerasy2d) .lt. -1) .or. (maxval(aerasy2d) .gt. 1)) then
             FATAL_ERROR('Aerosol asymmetry parameter must be within [-1,1]. Out of bounds value(s) found in auxinput')
          end if
          write( wrf_err_message, '("aer_asy_opt=",I1,": asymmetry parameter read from auxinput (min=",F6.3," max=",F6.3,")")') &
                                  aer_asy_opt,minval(aerasy2d),maxval(aerasy2d)
          WRITE_MESSAGE(trim(wrf_err_message))
          do j=jts,jte
             do i=its,ite
                do k=kts,kte
                   do nb=1,N_BANDS
                      asyaer(i,k,j,nb)=aerasy2d(i,j)
                   end do
                end do
             end do
          end do

       case(3)
          ! spectral disaggregation based on a prescribed aerosol type and relative humidity
!         call mpas_log_write('--- aer_asy_opt    = $i: asymmetry parameter calculated from RH and aer_type $i', &
!                             intArgs=(/aer_asy_opt,aer_type/))
          call calc_spectral_asy_rrtmg_sw(ims,ime,jms,jme,kms,kme,  &
                                          its,ite,jts,jte,kts,kte,  &
                                          rh,aer_type,asyaer        )
!-- MPAS modifications: we do not need the variable aerasy2d outside of subroutine calc_aerosol_rrtmg_sw. Since it is
!   declared as an optional variable, we simply test if it is present or not (Laura D. Fowler/2018=04-09):
          if(present(aerasy2d)) then
          ! added July, 16th, 2013: aerasy2d is in the wrfout when aer_asy_opt=3. It is the average
          ! value in the spectral bands between 0.4 and 1. um
          do j=jts,jte
             do i=its,ite
                aerasy2d(i,j)=0
             end do
          end do
          do j=jts,jte
             do i=its,ite
                do nb=8,N_BANDS-3  ! bands between 0.4 and 1.0 um
                   aerasy2d(i,j)=aerasy2d(i,j)+asyaer(i,kts,j,nb)
                end do
                aerasy2d(i,j)=aerasy2d(i,j)/(N_BANDS-3-8+1)
             end do
          end do
          endif ! end MPAS modifications.

       case default
          write(wrf_err_message,*) 'Expected aer_asy_opt=[1,2,3]. Got',aer_asy_opt
          FATAL_ERROR(trim(wrf_err_message))
    end select aer_asy_opt_select

end subroutine calc_aerosol_rrtmg_sw

subroutine calc_spectral_aod_rrtmg_sw(ims,ime,jms,jme,kms,kme,          &
                                      its,ite,jts,jte,kts,kte,          &
                                      rh,aer_type,aod550,               &
                                      tauaer,                           &
                                      aod550_3d, tauaer3d)               ! trude

   implicit none
   
   ! constants
   integer, parameter :: N_AER_TYPES=3
   integer, parameter :: N_RH=8
   integer, parameter :: N_BANDS=14
   integer, parameter :: N_INT_POINTS=4

   ! I/O variables
   integer, intent(in) :: ims,ime,jms,jme,kms,kme, &
                          its,ite,jts,jte,kts,kte
!- MPAS modifications: aer_type is a function of the land-sea mask, and set to 1 over land (or rural classification in WRF),
!  and set to 0 over oceans (or maritime classification in WRF):
!  integer, intent(in) :: aer_type
   integer:: aer_t
   integer, dimension(ims:ime,jms:jme), intent(in):: aer_type
!- end MPAS modifications (Laura D. Fowler/2018=04-09).

   real, dimension(ims:ime, kms:kme, jms:jme),   intent(in) :: rh        ! relative humidity
   real, dimension(ims:ime, jms:jme),            intent(in) :: aod550    ! Total AOD at 550 nm at surface
   real, dimension(ims:ime, jms:jme, 1:N_BANDS), intent(inout) :: tauaer ! Total spectral aerosol optical depth at surface

   ! ++ Trude
   real, dimension(ims:ime, kms:kme, jms:jme), optional, intent(in) :: aod550_3d ! 3D  AOD at 550 nm 
   real, dimension(ims:ime, kms:kme, jms:jme, 1:N_BANDS), optional, intent(inout) :: tauaer3d ! 
   ! -- Trude

   ! local variables
   integer :: i,j,k,ib,imax,imin,ii,jj,kk
   real    :: rhs(N_RH),lj
   real    :: raod_lut(N_AER_TYPES,N_BANDS,N_RH)

   ! relative humidity steps
   data (rhs(i),i=1,8) /0.,50.,70.,80.,90.,95.,98.,99./

   ! aer_type = 1 : rural (SF79)
   data (raod_lut(1,ib,1),ib=1,N_BANDS) /0.0735,0.0997,0.1281,0.1529,0.1882,0.2512,0.3010,0.4550,0.7159,1.0357, &
                                         1.3582,1.6760,2.2523,0.0582/
   data (raod_lut(1,ib,2),ib=1,N_BANDS) /0.0741,0.1004,0.1289,0.1537,0.1891,0.2522,0.3021,0.4560,0.7166,1.0351, &
                                         1.3547,1.6687,2.2371,0.0587/
   data (raod_lut(1,ib,3),ib=1,N_BANDS) /0.0752,0.1017,0.1304,0.1554,0.1909,0.2542,0.3042,0.4580,0.7179,1.0342, &
                                         1.3485,1.6559,2.2102,0.0596/
   data (raod_lut(1,ib,4),ib=1,N_BANDS) /0.0766,0.1034,0.1323,0.1575,0.1932,0.2567,0.3068,0.4605,0.7196,1.0332, &
                                         1.3411,1.6407,2.1785,0.0608/
   data (raod_lut(1,ib,5),ib=1,N_BANDS) /0.0807,0.1083,0.1379,0.1635,0.1998,0.2639,0.3143,0.4677,0.7244,1.0305, &
                                         1.3227,1.6031,2.1006,0.0644/
   data (raod_lut(1,ib,6),ib=1,N_BANDS) /0.0884,0.1174,0.1482,0.1746,0.2118,0.2769,0.3277,0.4805,0.7328,1.0272, &
                                         1.2977,1.5525,1.9976,0.0712/
   data (raod_lut(1,ib,7),ib=1,N_BANDS) /0.1072,0.1391,0.1724,0.2006,0.2396,0.3066,0.3581,0.5087,0.7510,1.0231, &
                                         1.2622,1.4818,1.8565,0.0878/
   data (raod_lut(1,ib,8),ib=1,N_BANDS) /0.1286,0.1635,0.1991,0.2288,0.2693,0.3377,0.3895,0.5372,0.7686,1.0213, &
                                         1.2407,1.4394,1.7739,0.1072/

   ! aer_type = 2 : urban (SF79)
   data (raod_lut(2,ib,1),ib=1,N_BANDS) /0.1244,0.1587,0.1939,0.2233,0.2635,0.3317,0.3835,0.5318,0.7653,1.0344, &
                                         1.3155,1.5885,2.0706,0.1033/
   data (raod_lut(2,ib,2),ib=1,N_BANDS) /0.1159,0.1491,0.1834,0.2122,0.2518,0.3195,0.3712,0.5207,0.7585,1.0331, &
                                         1.3130,1.5833,2.0601,0.0956/
   data (raod_lut(2,ib,3),ib=1,N_BANDS) /0.1093,0.1416,0.1752,0.2035,0.2427,0.3099,0.3615,0.5118,0.7529,1.0316, &
                                         1.3083,1.5739,2.0408,0.0898/
   data (raod_lut(2,ib,4),ib=1,N_BANDS) /0.1062,0.1381,0.1712,0.1993,0.2382,0.3052,0.3567,0.5074,0.7501,1.0302, &
                                         1.3025,1.5620,2.0168,0.0870/
   data (raod_lut(2,ib,5),ib=1,N_BANDS) /0.1045,0.1361,0.1690,0.1970,0.2357,0.3025,0.3540,0.5049,0.7486,1.0271, &
                                         1.2864,1.5297,1.9518,0.0854/
   data (raod_lut(2,ib,6),ib=1,N_BANDS) /0.1065,0.1384,0.1716,0.1997,0.2386,0.3056,0.3571,0.5078,0.7504,1.0227, &
                                         1.2603,1.4780,1.8492,0.0872/
   data (raod_lut(2,ib,7),ib=1,N_BANDS) /0.1147,0.1478,0.1820,0.2107,0.2503,0.3179,0.3696,0.5192,0.7575,1.0146, &
                                         1.2116,1.3830,1.6658,0.0946/
   data (raod_lut(2,ib,8),ib=1,N_BANDS) /0.1247,0.1590,0.1943,0.2237,0.2639,0.3322,0.3840,0.5322,0.7656,1.0082, &
                                         1.1719,1.3075,1.5252,0.1036/

   ! aer_type = 3 : maritime (SF79)
   data (raod_lut(3,ib,1),ib=1,N_BANDS) /0.3053,0.3507,0.3932,0.4261,0.4681,0.5334,0.5797,0.6962,0.8583,1.0187, &
                                         1.1705,1.3049,1.5205,0.2748/
   data (raod_lut(3,ib,2),ib=1,N_BANDS) /0.3566,0.4023,0.4443,0.4765,0.5170,0.5792,0.6227,0.7298,0.8756,1.0162, &
                                         1.1472,1.2614,1.4415,0.3256/
   data (raod_lut(3,ib,3),ib=1,N_BANDS) /0.4359,0.4803,0.5203,0.5505,0.5879,0.6441,0.6828,0.7756,0.8985,1.0135, &
                                         1.1198,1.2109,1.3518,0.4051/
   data (raod_lut(3,ib,4),ib=1,N_BANDS) /0.5128,0.5544,0.5913,0.6187,0.6523,0.7020,0.7358,0.8149,0.9174,1.0115, &
                                         1.0995,1.1740,1.2875,0.4835/
   data (raod_lut(3,ib,5),ib=1,N_BANDS) /0.6479,0.6816,0.7108,0.7320,0.7575,0.7946,0.8193,0.8752,0.9455,1.0092, &
                                         1.0728,1.1263,1.2061,0.6236/
   data (raod_lut(3,ib,6),ib=1,N_BANDS) /0.7582,0.7831,0.8043,0.8196,0.8377,0.8636,0.8806,0.9184,0.9649,1.0080, &
                                         1.0564,1.0973,1.1576,0.7399/
   data (raod_lut(3,ib,7),ib=1,N_BANDS) /0.8482,0.8647,0.8785,0.8884,0.9000,0.9164,0.9272,0.9506,0.9789,1.0072, &
                                         1.0454,1.0780,1.1256,0.8360/
   data (raod_lut(3,ib,8),ib=1,N_BANDS) /0.8836,0.8965,0.9073,0.9149,0.9239,0.9365,0.9448,0.9626,0.9841,1.0069, &
                                         1.0415,1.0712,1.1145,0.8741/

!   ++ Trude    ;  if 3D AOD, disaggreaget at all levels.
   if (present(aod550_3d)) then
      do j=jts,jte
         do i=its,ite
            !-- initialization of aerosol type:
            aer_t = aer_type(i,j)
            !   common part of the Lagrange's interpolator
            !   only depends on the relative humidity value
            do kk = kts,kte
               ii=1
               do while ( (ii.le.N_RH) .and. (rh(i,kk,j).gt.rhs(ii)) )
                  ii=ii+1
               end do
               imin=max(1,ii-N_INT_POINTS/2-1)
               imax=min(N_RH,ii+N_INT_POINTS/2)

               do ib=1,N_BANDS
                  tauaer3d(i,kk,j,ib)=0.
                  do jj=imin,imax
                     lj=1.
                     do k=imin,imax
                        if (k.ne.jj) lj=lj*(rh(i,kk,j)-rhs(k))/(rhs(jj)-rhs(k))
                     end do
                     tauaer3d(i,kk,j,ib)=tauaer3d(i,kk,j,ib)+lj*raod_lut(aer_t,ib,jj)*aod550_3d(i,kk,j)
                  end do
               end do
            end do
         end do
      end do
    else
! -- Trude

   do j=jts,jte
      do i=its,ite
         !-- initialization of aerosol type:
         aer_t = aer_type(i,j)
         ! common part of the Lagrange's interpolator
         !   only depends on the relative humidity value
         ii=1
         do while ( (ii.le.N_RH) .and. (rh(i,kts,j).gt.rhs(ii)) )
            ii=ii+1
         end do
         imin=max(1,ii-N_INT_POINTS/2-1)
         imax=min(N_RH,ii+N_INT_POINTS/2)

         do ib=1,N_BANDS
            tauaer(i,j,ib)=0.
            do jj=imin,imax
               lj=1.
               do k=imin,imax
                  if (k.ne.jj) lj=lj*(rh(i,kts,j)-rhs(k))/(rhs(jj)-rhs(k))
               end do
               tauaer(i,j,ib)=tauaer(i,j,ib)+lj*raod_lut(aer_t,ib,jj)*aod550(i,j)
            end do
         end do
      end do
   end do
   endif

end subroutine calc_spectral_aod_rrtmg_sw

subroutine calc_spectral_ssa_rrtmg_sw(ims,ime,jms,jme,kms,kme,  &
                                      its,ite,jts,jte,kts,kte,  &
                                      rh,aer_type,              &
                                      ssaaer                    )
   implicit none
   
   ! constants
   integer, parameter :: N_AER_TYPES=3
   integer, parameter :: N_RH=8
   integer, parameter :: N_BANDS=14
   integer, parameter :: N_INT_POINTS=4

   ! I/O variables
   integer, intent(in) :: ims,ime,jms,jme,kms,kme, &
                          its,ite,jts,jte,kts,kte
!- MPAS modifications: aer_type is a function of the land-sea mask, and set to 1 over land (or rural classification in WRF),
!  and set to 0 over oceans (or maritime classification in WRF):
!  integer, intent(in) :: aer_type
   integer:: aer_t
   integer, dimension(ims:ime,jms:jme), intent(in):: aer_type
!- end MPAS modifications (Laura D. Fowler/2018=04-09).
   real, dimension(ims:ime, kms:kme, jms:jme),            intent(in)    :: rh     ! surface relative humidity
   real, dimension(ims:ime, kms:kme, jms:jme, 1:N_BANDS), intent(inout) :: ssaaer ! aerosol single-scattering albedo at surface

   ! local variables
   integer :: i,j,k,kk,ib,imax,imin,ii,jj
   real    :: rhs(N_RH),lj
   real    :: ssa_lut(N_AER_TYPES,N_BANDS,N_RH)

   ! relative humidity steps
   data (rhs(i),i=1,8) /0.,50.,70.,80.,90.,95.,98.,99./

   ! aer_type = 1 : rural (SF79)
   data (ssa_lut(1,ib,1),ib=1,N_BANDS) /0.8730,0.6695,0.8530,0.8601,0.8365,0.7949,0.8113,0.8810,0.9305,0.9436, &
                                        0.9532,0.9395,0.8007,0.8634/
   data (ssa_lut(1,ib,2),ib=1,N_BANDS) /0.8428,0.6395,0.8571,0.8645,0.8408,0.8007,0.8167,0.8845,0.9326,0.9454, &
                                        0.9545,0.9416,0.8070,0.8589/
   data (ssa_lut(1,ib,3),ib=1,N_BANDS) /0.8000,0.6025,0.8668,0.8740,0.8503,0.8140,0.8309,0.8943,0.9370,0.9489, &
                                        0.9577,0.9451,0.8146,0.8548/
   data (ssa_lut(1,ib,4),ib=1,N_BANDS) /0.7298,0.5666,0.9030,0.9049,0.8863,0.8591,0.8701,0.9178,0.9524,0.9612, &
                                        0.9677,0.9576,0.8476,0.8578/
   data (ssa_lut(1,ib,5),ib=1,N_BANDS) /0.7010,0.5606,0.9312,0.9288,0.9183,0.9031,0.9112,0.9439,0.9677,0.9733, &
                                        0.9772,0.9699,0.8829,0.8590/
   data (ssa_lut(1,ib,6),ib=1,N_BANDS) /0.6933,0.5620,0.9465,0.9393,0.9346,0.9290,0.9332,0.9549,0.9738,0.9782, &
                                        0.9813,0.9750,0.8980,0.8594/
   data (ssa_lut(1,ib,7),ib=1,N_BANDS) /0.6842,0.5843,0.9597,0.9488,0.9462,0.9470,0.9518,0.9679,0.9808,0.9839, &
                                        0.9864,0.9794,0.9113,0.8648/
   data (ssa_lut(1,ib,8),ib=1,N_BANDS) /0.6786,0.5897,0.9658,0.9522,0.9530,0.9610,0.9651,0.9757,0.9852,0.9871, &
                                        0.9883,0.9835,0.9236,0.8618/

   ! aer_type = 2: urban (SF79)
   data (ssa_lut(2,ib,1),ib=1,N_BANDS) /0.4063,0.3663,0.4093,0.4205,0.4487,0.4912,0.5184,0.5743,0.6233,0.6392, &
                                        0.6442,0.6408,0.6105,0.4094/
   data (ssa_lut(2,ib,2),ib=1,N_BANDS) /0.4113,0.3654,0.4215,0.4330,0.4604,0.5022,0.5293,0.5848,0.6336,0.6493, &
                                        0.6542,0.6507,0.6205,0.4196/
   data (ssa_lut(2,ib,3),ib=1,N_BANDS) /0.4500,0.3781,0.4924,0.5050,0.5265,0.5713,0.6048,0.6274,0.6912,0.7714, &
                                        0.7308,0.7027,0.6772,0.4820/
   data (ssa_lut(2,ib,4),ib=1,N_BANDS) /0.5075,0.4139,0.5994,0.6127,0.6350,0.6669,0.6888,0.7333,0.7704,0.7809, &
                                        0.7821,0.7762,0.7454,0.5709/
   data (ssa_lut(2,ib,5),ib=1,N_BANDS) /0.5596,0.4570,0.7009,0.7118,0.7317,0.7583,0.7757,0.8093,0.8361,0.8422, &
                                        0.8406,0.8337,0.8036,0.6525/
   data (ssa_lut(2,ib,6),ib=1,N_BANDS) /0.6008,0.4971,0.7845,0.7906,0.8075,0.8290,0.8418,0.8649,0.8824,0.8849, &
                                        0.8815,0.8739,0.8455,0.7179/
   data (ssa_lut(2,ib,7),ib=1,N_BANDS) /0.6401,0.5407,0.8681,0.8664,0.8796,0.8968,0.9043,0.9159,0.9244,0.9234, &
                                        0.9182,0.9105,0.8849,0.7796/
   data (ssa_lut(2,ib,8),ib=1,N_BANDS) /0.6567,0.5618,0.9073,0.9077,0.9182,0.9279,0.9325,0.9398,0.9440,0.9413, &
                                        0.9355,0.9278,0.9039,0.8040/

   ! aer_type = 3 : maritime (SF79)
   data (ssa_lut(3,ib,1),ib=1,N_BANDS) /0.9697,0.9183,0.9749,0.9820,0.9780,0.9712,0.9708,0.9778,0.9831,0.9827, &
                                        0.9826,0.9723,0.8763,0.9716/
   data (ssa_lut(3,ib,2),ib=1,N_BANDS) /0.9070,0.8491,0.9730,0.9816,0.9804,0.9742,0.9738,0.9802,0.9847,0.9841, &
                                        0.9838,0.9744,0.8836,0.9546/
   data (ssa_lut(3,ib,3),ib=1,N_BANDS) /0.8378,0.7761,0.9797,0.9827,0.9829,0.9814,0.9812,0.9852,0.9882,0.9875, &
                                        0.9871,0.9791,0.9006,0.9348/
   data (ssa_lut(3,ib,4),ib=1,N_BANDS) /0.7866,0.7249,0.9890,0.9822,0.9856,0.9917,0.9924,0.9932,0.9943,0.9938, &
                                        0.9933,0.9887,0.9393,0.9204/
   data (ssa_lut(3,ib,5),ib=1,N_BANDS) /0.7761,0.7164,0.9959,0.9822,0.9834,0.9941,0.9955,0.9952,0.9960,0.9956, &
                                        0.9951,0.9922,0.9538,0.9152/
   data (ssa_lut(3,ib,6),ib=1,N_BANDS) /0.7671,0.7114,0.9902,0.9786,0.9838,0.9954,0.9970,0.9965,0.9971,0.9968, &
                                        0.9964,0.9943,0.9644,0.9158/
   data (ssa_lut(3,ib,7),ib=1,N_BANDS) /0.7551,0.7060,0.9890,0.9743,0.9807,0.9966,0.9989,0.9978,0.9982,0.9980, &
                                        0.9978,0.9964,0.9757,0.9122/
   data (ssa_lut(3,ib,8),ib=1,N_BANDS) /0.7439,0.7000,0.9870,0.9695,0.9769,0.9970,1.0000,0.9984,0.9988,0.9986, &
                                        0.9984,0.9975,0.9825,0.9076/

   do j=jts,jte
      do i=its,ite
         !-- initialization of aerosol type:
         aer_t = aer_type(i,j)
         do k=kts,kte
            ! common part of the Lagrange's interpolator
            !   only depends on the relative humidity value
            ii=1
            do while ( (ii.le.N_RH) .and. (rh(i,k,j).gt.rhs(ii)) )
               ii=ii+1
            end do
            imin=max(1,ii-N_INT_POINTS/2-1)
            imax=min(N_RH,ii+N_INT_POINTS/2)

            do ib=1,N_BANDS
               ssaaer(i,k,j,ib)=0.
               do jj=imin,imax
                  lj=1.
                  do kk=imin,imax
                     if (kk.ne.jj) lj=lj*(rh(i,k,j)-rhs(kk))/(rhs(jj)-rhs(kk))
                  end do
                  ssaaer(i,k,j,ib)=ssaaer(i,k,j,ib)+lj*ssa_lut(aer_t,ib,jj)
               end do
            end do
         end do
      end do
   end do
end subroutine calc_spectral_ssa_rrtmg_sw

subroutine calc_spectral_asy_rrtmg_sw(ims,ime,jms,jme,kms,kme,  &
                                      its,ite,jts,jte,kts,kte,  &
                                      rh,aer_type,              &
                                      asyaer                    )
   implicit none
   
   ! constants
   integer, parameter :: N_AER_TYPES=3
   integer, parameter :: N_RH=8
   integer, parameter :: N_BANDS=14
   integer, parameter :: N_INT_POINTS=4

   ! I/O variables
   integer, intent(in) :: ims,ime,jms,jme,kms,kme, &
                          its,ite,jts,jte,kts,kte
!- MPAS modifications: aer_type is a function of the land-sea mask, and set to 1 over land (or rural classification in WRF),
!  and set to 0 over oceans (or maritime classification in WRF):
!  integer, intent(in) :: aer_type
   integer:: aer_t
   integer, dimension(ims:ime,jms:jme), intent(in):: aer_type
!- end MPAS modifications (Laura D. Fowler/2018=04-09).
   real, dimension(ims:ime, kms:kme, jms:jme),            intent(in)    :: rh ! surface relative humidity
   real, dimension(ims:ime, kms:kme, jms:jme, 1:N_BANDS), intent(inout) :: asyaer ! aerosol asymmetry parameter at surface

   ! local variables
   integer :: i,j,k,kk,ib,imax,imin,ii,jj
   real    :: rhs(N_RH),lj
   real    :: asy_lut(N_AER_TYPES,N_BANDS,N_RH)

   ! relative humidity steps
   data (rhs(i),i=1,8) /0.,50.,70.,80.,90.,95.,98.,99./

   ! aer_type = 1 : rural (SF79)
   data (asy_lut(1,ib,1),ib=1,N_BANDS) /0.7444,0.7711,0.7306,0.7103,0.6693,0.6267,0.6169,0.6207,0.6341,0.6497, &
                                        0.6630,0.6748,0.7208,0.7419/
   data (asy_lut(1,ib,2),ib=1,N_BANDS) /0.7444,0.7747,0.7314,0.7110,0.6711,0.6301,0.6210,0.6251,0.6392,0.6551, &
                                        0.6680,0.6799,0.7244,0.7436/
   data (asy_lut(1,ib,3),ib=1,N_BANDS) /0.7438,0.7845,0.7341,0.7137,0.6760,0.6381,0.6298,0.6350,0.6497,0.6657, &
                                        0.6790,0.6896,0.7300,0.7477/
   data (asy_lut(1,ib,4),ib=1,N_BANDS) /0.7336,0.7934,0.7425,0.7217,0.6925,0.6665,0.6616,0.6693,0.6857,0.7016, &
                                        0.7139,0.7218,0.7495,0.7574/
   data (asy_lut(1,ib,5),ib=1,N_BANDS) /0.7111,0.7865,0.7384,0.7198,0.6995,0.6864,0.6864,0.6987,0.7176,0.7326, &
                                        0.7427,0.7489,0.7644,0.7547/
   data (asy_lut(1,ib,6),ib=1,N_BANDS) /0.7009,0.7828,0.7366,0.7196,0.7034,0.6958,0.6979,0.7118,0.7310,0.7452, &
                                        0.7542,0.7593,0.7692,0.7522/
   data (asy_lut(1,ib,7),ib=1,N_BANDS) /0.7226,0.8127,0.7621,0.7434,0.7271,0.7231,0.7248,0.7351,0.7506,0.7622, &
                                        0.7688,0.7719,0.7756,0.7706/
   data (asy_lut(1,ib,8),ib=1,N_BANDS) /0.7296,0.8219,0.7651,0.7513,0.7404,0.7369,0.7386,0.7485,0.7626,0.7724, &
                                        0.7771,0.7789,0.7790,0.7760/

   ! aer_type = 2: urban (SF79)
   data (asy_lut(2,ib,1),ib=1,N_BANDS) /0.7399,0.7372,0.7110,0.6916,0.6582,0.6230,0.6147,0.6214,0.6412,0.6655, &
                                        0.6910,0.7124,0.7538,0.7395/
   data (asy_lut(2,ib,2),ib=1,N_BANDS) /0.7400,0.7419,0.7146,0.6952,0.6626,0.6287,0.6209,0.6280,0.6481,0.6723, &
                                        0.6974,0.7180,0.7575,0.7432/
   data (asy_lut(2,ib,3),ib=1,N_BANDS) /0.7363,0.7614,0.7303,0.7100,0.6815,0.6550,0.6498,0.6590,0.6802,0.7032, &
                                        0.7255,0.7430,0.7735,0.7580/
   data (asy_lut(2,ib,4),ib=1,N_BANDS) /0.7180,0.7701,0.7358,0.7163,0.6952,0.6807,0.6801,0.6935,0.7160,0.7370, &
                                        0.7553,0.7681,0.7862,0.7623/
   data (asy_lut(2,ib,5),ib=1,N_BANDS) /0.7013,0.7733,0.7374,0.7203,0.7057,0.7006,0.7035,0.7192,0.7415,0.7596, &
                                        0.7739,0.7827,0.7906,0.7596/
   data (asy_lut(2,ib,6),ib=1,N_BANDS) /0.6922,0.7773,0.7404,0.7264,0.7170,0.7179,0.7228,0.7389,0.7595,0.7746, &
                                        0.7851,0.7909,0.7918,0.7562/
   data (asy_lut(2,ib,7),ib=1,N_BANDS) /0.6928,0.7875,0.7491,0.7393,0.7345,0.7397,0.7455,0.7602,0.7773,0.7883, &
                                        0.7944,0.7970,0.7912,0.7555/
   data (asy_lut(2,ib,8),ib=1,N_BANDS) /0.7021,0.7989,0.7590,0.7512,0.7613,0.7746,0.7718,0.7727,0.7867,0.7953, &
                                        0.7988,0.7994,0.7906,0.7600/

   ! aer_type = 3 : maritime (SF79)
   data (asy_lut(3,ib,1),ib=1,N_BANDS) /0.6620,0.7011,0.7111,0.7068,0.6990,0.6918,0.6883,0.6827,0.6768,0.6773, &
                                        0.6863,0.6940,0.7245,0.6719/
   data (asy_lut(3,ib,2),ib=1,N_BANDS) /0.6880,0.7394,0.7297,0.7240,0.7162,0.7083,0.7038,0.6957,0.6908,0.6917, &
                                        0.6952,0.7035,0.7356,0.6977/
   data (asy_lut(3,ib,3),ib=1,N_BANDS) /0.7266,0.7970,0.7666,0.7593,0.7505,0.7427,0.7391,0.7293,0.7214,0.7210, &
                                        0.7212,0.7265,0.7519,0.7340/
   data (asy_lut(3,ib,4),ib=1,N_BANDS) /0.7683,0.8608,0.8120,0.8030,0.7826,0.7679,0.7713,0.7760,0.7723,0.7716, &
                                        0.7726,0.7767,0.7884,0.7768/
   data (asy_lut(3,ib,5),ib=1,N_BANDS) /0.7776,0.8727,0.8182,0.8083,0.7985,0.7939,0.7953,0.7913,0.7846,0.7870, &
                                        0.7899,0.7918,0.7969,0.7870/
   data (asy_lut(3,ib,6),ib=1,N_BANDS) /0.7878,0.8839,0.8231,0.8130,0.8050,0.7977,0.7945,0.7932,0.7955,0.7992, &
                                        0.8025,0.8035,0.8055,0.7956/
   data (asy_lut(3,ib,7),ib=1,N_BANDS) /0.8005,0.8957,0.8273,0.8179,0.8105,0.8035,0.8010,0.8030,0.8081,0.8108, &
                                        0.8143,0.8174,0.8174,0.8042/
   data (asy_lut(3,ib,8),ib=1,N_BANDS) /0.8104,0.9034,0.8294,0.8212,0.8144,0.8087,0.8077,0.8118,0.8175,0.8202, &
                                        0.8239,0.8265,0.8246,0.8095/

   do j=jts,jte
      do i=its,ite
         !-- initialization of aerosol type:
         aer_t = aer_type(i,j)
         do k=kts,kte
            ! common part of the Lagrange's interpolator
            !   only depends on the relative humidity value
            ii=1
            do while ( (ii.le.N_RH) .and. (rh(i,k,j).gt.rhs(ii)) )
               ii=ii+1
            end do
            imin=max(1,ii-N_INT_POINTS/2-1)
            imax=min(N_RH,ii+N_INT_POINTS/2)

            do ib=1,N_BANDS
               asyaer(i,k,j,ib)=0.
               do jj=imin,imax
                  lj=1.
                  do kk=imin,imax
                     if (kk.ne.jj) lj=lj*(rh(i,k,j)-rhs(kk))/(rhs(jj)-rhs(kk))
                  end do
                  asyaer(i,k,j,ib)=asyaer(i,k,j,ib)+lj*asy_lut(aer_t,ib,jj)
               end do
            end do
         end do
      end do
   end do
end subroutine calc_spectral_asy_rrtmg_sw

subroutine aod_profiler(ht,dz8w,taod550,n_bands,   &
                        ims,ime,jms,jme,kms,kme,   &
                        its,ite,jts,jte,kts,kte,   &
                        aod550                     &
                                                   )
   implicit none
   
   ! constants
   real, parameter :: scale_height=2500. ! meters

   ! I/O variables
   integer, intent(in) :: n_bands
   integer, intent(in) :: ims,ime,jms,jme,kms,kme, &
                          its,ite,jts,jte,kts,kte
   real, dimension( ims:ime, jms:jme),            intent(in) :: ht
   real, dimension( ims:ime, kms:kme, jms:jme ),  intent(in) :: dz8w
   real, dimension( ims:ime, jms:jme, 1:n_bands), intent(in) :: taod550
   real, dimension( ims:ime, kms:kme, jms:jme, 1:n_bands ), intent(inout) :: aod550

   ! local variables
   real, dimension(its:ite,kts:kte) :: z2d,aod5502d
   real, dimension(its:ite)         :: htoa
   real :: aod_scale
   real :: aod_acum
   integer :: i,j,k,nb

   ! input variables from driver are defined such as kms is sfc and
   ! kme is toa. Equivalently, kts is sfc and kte is toa
   do j=jts,jte
      ! heigth profile
      !   kts=surface, kte=toa
      do i=its,ite
         z2d(i,kts)=ht(i,j)+0.5*dz8w(i,kts,j)
         do k=kts+1,kte
            z2d(i,k)=z2d(i,k-1)+0.5*(dz8w(i,k-1,j)+dz8w(i,k,j))
         end do
         htoa(i)=z2d(i,kte)+0.5*dz8w(i,kte,j)
      end do

      do nb=1,n_bands
         ! AOD exponential profile
         do i=its,ite
            aod_scale=taod550(i,j,nb)/(scale_height*(exp(-ht(i,j)/scale_height)-exp(-htoa(i)/scale_height)))
            do k=kts,kte
               aod550(i,k,j,nb)=aod_scale*dz8w(i,k,j)*exp(-z2d(i,k)/scale_height)
            end do
         end do 
      end do ! nb-loop
   end do ! j-loop
end subroutine aod_profiler

subroutine calc_relative_humidity(p,t3d,qv3d,                 &
                                  ims,ime,jms,jme,kms,kme,    &
                                  its,ite,jts,jte,kts,kte,    &
                                  rh                          )
   implicit none
   
   ! I/O variables
   integer, intent(in) :: ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
   ! Naming convention: 8~at => p8w reads as "p-at-w" (w=full levels)
   real, dimension(ims:ime, kms:kme, jms:jme), intent(in)    :: p,    & ! pressure (Pa)
                                                                t3d,  & ! temperature (K)
                                                                qv3d    ! water vapor mixing ratio (kg/kg)
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout) :: rh      ! relative humidity at surface

   ! local variables
   real    :: tc,rv,es,e
   integer :: i,j,k

   do j=jts,jte
      do i=its,ite
         do k=kts,kte                               ! only calculations at surface level
            tc=t3d(i,k,j)-273.15                    ! temperature (C)
            rv=max(0.,qv3d(i,k,j))                  ! water vapor mixing ration (kg kg-1)
            es=6.112*exp((17.6*tc)/(tc+243.5))      ! saturation vapor pressure, hPa, Bolton (1980)
            e =0.01*rv*p(i,k,j)/(rv+0.62197)        ! vapor pressure, hPa, (ECMWF handouts, page 6, Atmosph. Thermdyn.)
                                                    ! rv=eps * e/(p-e) -> e=p * rv/(rv+eps), eps=0.62197
            rh(i,k,j)=min(99.,max(0.,100.*e/es))    ! relative humidity (%)
         end do
      end do
   end do

end subroutine calc_relative_humidity

!=================================================================================================================
 end module module_ra_rrtmg_sw_aerosols
!=================================================================================================================
