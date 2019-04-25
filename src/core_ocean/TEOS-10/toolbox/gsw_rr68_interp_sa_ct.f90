!==========================================================================
pure subroutine gsw_rr68_interp_sa_ct (sa, ct, p, p_i, sa_i, ct_i)
!==========================================================================
!
!  Interpolate Absolute Salinity and Conservative Temperature values to
!  arbitrary pressures using the Reiniger and Ross (1968) interpolation
!  scheme.
!  Note that this interpolation scheme requires at least four observed
!  bottles on the cast.
!
!  SA   =  Absolute Salinity                                  [ g/kg ]
!  CT   =  Conservative Temperature (ITS-90)                 [ deg C ]
!  p    =  sea pressure                                       [ dbar ]
!           ( i.e. absolute pressure - 10.1325 dbar )
!  p_i  =  pressures to interpolate to.
!
!  SA_i = interpolated SA values at pressures p_i.
!  CT_i = interpolated CT values at pressures p_i.
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_util_interp1q_int

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa(:), ct(:), p(:), p_i(:)
real (r8), intent(out) :: sa_i(:), ct_i(:)

integer :: i, j, mp, mp_i, nshallow, ncentral, ndeep

integer, allocatable :: ip(:), ip_i(:)
integer, allocatable :: ip_ishallow(:), ip_icentral(:), ip_ideep(:)
logical, allocatable :: shallow(:), central(:), deep(:)
real (r8), allocatable :: ip_shallow(:), ip_central(:), ip_deep(:)
real (r8), allocatable :: dp(:)

character (*), parameter :: func_name = "gsw_rr68_interp_sa_ct"

mp = size(p)
if (mp .lt. 4) then  ! need at least four bottles to perform this interpolation
    sa_i = gsw_error_code(1,func_name)
    ct_i = sa_i
    return
end if

allocate(dp(mp-1))
dp = p(2:mp) - p(1:mp-1)
if (any(dp .le. 0.0_r8)) then  ! pressure must be monotonic
    sa_i = gsw_error_code(2,func_name)
    ct_i = sa_i
    return
end if

mp_i = size(p_i)

allocate(shallow(mp_i),central(mp_i),deep(mp_i))
shallow = (p_i >= p(1)    .and. p_i <= p(2)   ); nshallow=count(shallow)
central = (p_i >= p(2)    .and. p_i <= p(mp-1)); ncentral=count(central)
deep    = (p_i >= p(mp-1) .and. p_i <= p(mp)  ); ndeep=count(deep)

if ((nshallow .eq. 0) .or. (ncentral .eq. 0) .or. (ndeep .eq. 0)) then
    sa_i = gsw_error_code(3,func_name)
    ct_i = sa_i
    return
end if

allocate(ip(mp),ip_i(mp_i))
ip = (/ (i, i=1,mp) /)
ip_i = (/ (i, i=1,mp_i) /)

allocate(ip_shallow(nshallow),ip_central(ncentral),ip_deep(ndeep))
allocate(ip_ishallow(nshallow),ip_icentral(ncentral),ip_ideep(ndeep))

! Calculate the 2 outer extrapolated values and the inner interpolated values

ip_icentral = pack(ip_i,central)
ip_central = gsw_util_interp1q_int(p,ip,p_i(ip_icentral))
call rr68_interp_section(0,ip_central,ip_icentral,sa_i,ct_i)

ip_ishallow = pack(ip_i,shallow)
ip_shallow = gsw_util_interp1q_int(p,ip,p_i(ip_ishallow))
call rr68_interp_section(-1,ip_shallow,ip_ishallow,sa_i,ct_i)

ip_ideep = pack(ip_i,deep)
ip_deep = gsw_util_interp1q_int(p,ip,p_i(ip_ideep))
call rr68_interp_section(+1,ip_deep,ip_ideep,sa_i,ct_i)

! Insert any observed bottles that are at the required interpolated pressures
do i = 1, mp_i
   do j = 1, mp
      if (p(j) .eq. p_i(i)) then
         sa_i(i) = sa(j)
         ct_i(i) = ct(j)
      end if
   end do
end do

return

contains

    pure subroutine rr68_interp_section (sectnum, ip_sect, ip_isect, sa_i, ct_i)

    use gsw_mod_toolbox, only : gsw_linear_interp_sa_ct

    implicit none

    integer, intent(in) :: sectnum, ip_isect(:)
    real (r8), intent(in) :: ip_sect(:)
    real (r8), intent(inout) :: sa_i(:), ct_i(:)

    integer nsect
    real (r8) :: m

    integer, allocatable :: ip_1(:), ip_2(:), ip_3(:), ip_4(:)
    real (r8), allocatable :: ct_12(:), ct_13(:), ct_23(:), ct_34(:), ctp1(:)
    real (r8), allocatable :: ctp2(:), ct_ref(:), ctref_denom(:)
    real (r8), allocatable :: ct_ref_minus_ctp1(:), ct_ref_minus_ctp2(:)
    real (r8), allocatable :: ctref_num(:)
    real (r8), allocatable :: gamma1_23(:), gamma1_24(:), gamma2_31(:)
    real (r8), allocatable :: gamma2_34(:), gamma2_41(:), gamma3_12(:)
    real (r8), allocatable :: gamma3_42(:), gamma4_12(:), gamma4_23(:)
    real (r8), allocatable :: sa_12(:), sa_13(:), sa_23(:), sa_34(:), sap1(:)
    real (r8), allocatable :: sap2(:), sa_ref(:), saref_denom(:)
    real (r8), allocatable :: sa_ref_minus_sap1(:), sa_ref_minus_sap2(:)
    real (r8), allocatable :: saref_num(:)

    nsect = size(ip_sect)
    allocate(ip_1(nsect),ip_2(nsect),ip_3(nsect),ip_4(nsect))

    allocate(sa_12(nsect),sa_13(nsect),sa_23(nsect),sa_34(nsect))
    allocate(sa_ref(nsect),saref_num(nsect),saref_denom(nsect))
    allocate(sap1(nsect),sap2(nsect),sa_ref_minus_sap1(nsect))
    allocate(sa_ref_minus_sap2(nsect))

    allocate(ct_12(nsect),ct_13(nsect),ct_23(nsect),ct_34(nsect))
    allocate(ct_ref(nsect),ctref_num(nsect),ctref_denom(nsect))
    allocate(ctp1(nsect),ctp2(nsect),ct_ref_minus_ctp1(nsect))
    allocate(ct_ref_minus_ctp2(nsect))

    allocate(gamma1_23(nsect),gamma2_31(nsect),gamma3_12(nsect))
    if (sectnum .eq. 0) then
        allocate(gamma2_34(nsect),gamma3_42(nsect),gamma4_23(nsect))
    else
        allocate(gamma1_24(nsect),gamma2_41(nsect),gamma4_12(nsect))
    end if

    if (sectnum .lt. 0) then       !  shallow
        ip_1 = floor(ip_sect)
        ip_2 = ceiling(ip_sect)
        where (ip_1 .eq. ip_2) ip_2 = ip_1 + 1
        ip_3 = ip_2 + 1
        ip_4 = ip_3 + 1
    else if (sectnum .eq. 0) then  !  central
        ip_2 = floor(ip_sect)
        ip_3 = ceiling(ip_sect)
        where (ip_2 .eq. ip_3) ip_2 = ip_3 - 1
        ip_1 = ip_2 - 1
        where (ip_1 .lt. 1)
            ip_1 = 1
            ip_2 = 2
            ip_3 = 3
        end where
        ip_4 = ip_3 + 1
    else if (sectnum .gt. 0) then  !  deep
        ip_1 = ceiling(ip_sect)
        ip_2 = floor(ip_sect)
        where (ip_1 .eq. ip_2) ip_2 = ip_1 - 1
        ip_3 = ip_2 - 1
        ip_4 = ip_3 - 1
    end if

    !eqn (3d)
    sa_34 = sa(ip_3) + ((sa(ip_4) - sa(ip_3))*(p_i(ip_isect) - p(ip_3))/ &
        (p(ip_4) - p(ip_3)))
    ct_34 = ct(ip_3) + ((ct(ip_4) - ct(ip_3))*(p_i(ip_isect) - p(ip_3))/ &
        (p(ip_4) - p(ip_3)))

    ! Construct the Reiniger & Ross reference curve equation.
    ! m = the power variable
    m = 1.7_r8

    if (sectnum .eq. 0) then

        sa_12 = sa(ip_1) + ((sa(ip_2) - sa(ip_1))*(p_i(ip_isect) - p(ip_1))/ &
            (p(ip_2) - p(ip_1)))
        ct_12 = ct(ip_1) + ((ct(ip_2) - ct(ip_1))*(p_i(ip_isect) - p(ip_1))/ &
            (p(ip_2) - p(ip_1)))

        call gsw_linear_interp_sa_ct(sa,ct,p,p_i(ip_isect),sa_23,ct_23)

        ! eqn (3a)
        saref_num = (abs(sa_23-sa_34)**m)*sa_12 + (abs(sa_12-sa_23)**m)*sa_34 
        ctref_num = (abs(ct_23-ct_34)**m)*ct_12 + (abs(ct_12-ct_23)**m)*ct_34

        saref_denom = abs(sa_23-sa_34)**m + abs(sa_12-sa_23)**m 
        ctref_denom = abs(ct_23-ct_34)**m + abs(ct_12-ct_23)**m 

        where (saref_denom .eq. 0.0_r8)
            sa_23 = sa_23 + 1.0e-6_r8
            saref_num = (abs(sa_23-sa_34)**m)*sa_12+(abs(sa_12-sa_23)**m)*sa_34
            saref_denom = abs(sa_23-sa_34)**m + abs(sa_12-sa_23)**m 
        end where
        where (ctref_denom .eq. 0.0_r8)
            ct_23 = ct_23 + 1.0e-6_r8
            ctref_num = (abs(ct_23-ct_34)**m)*ct_12+(abs(ct_12-ct_23)**m)*ct_34
            ctref_denom = abs(ct_23-ct_34)**m + abs(ct_12-ct_23)**m 
        end where

        sa_ref = 0.5_r8*(sa_23 + (saref_num/saref_denom))
        ct_ref = 0.5_r8*(ct_23 + (ctref_num/ctref_denom))

    else

        call gsw_linear_interp_sa_ct(sa,ct,p,p_i(ip_isect),sa_12,ct_12)

        sa_13 = sa(ip_1) + ((sa(ip_3) - sa(ip_1))*(p_i(ip_isect) - p(ip_1))/&
            (p(ip_3) - p(ip_1)))
        ct_13 = ct(ip_1) + ((ct(ip_3) - ct(ip_1))*(p_i(ip_isect) - p(ip_1))/&
            (p(ip_3) - p(ip_1)))

        sa_23 = sa(ip_2) + ((sa(ip_3) - sa(ip_2))*(p_i(ip_isect) - p(ip_2))/ &
            (p(ip_3) - p(ip_2)))
        ct_23 = ct(ip_2) + ((ct(ip_3) - ct(ip_2))*(p_i(ip_isect) - p(ip_2))/ &
            (p(ip_3) - p(ip_2)))

        !eqn (3a')
        saref_num = (abs(sa_12-sa_23)**m)*sa_34 + (abs(sa_12-sa_13)**m)*sa_23 
        ctref_num = (abs(ct_12-ct_23)**m)*ct_34 + (abs(ct_12-ct_13)**m)*ct_23

        saref_denom = abs(sa_12-sa_23)**m + abs(sa_12-sa_13)**m 
        ctref_denom = abs(ct_12-ct_23)**m + abs(ct_12-ct_13)**m 

        where (saref_denom .eq. 0.0_r8)
            sa_23 = sa_23 + 1.0e-6_r8
            saref_num = (abs(sa_12-sa_23)**m)*sa_34+(abs(sa_12-sa_13)**m)*sa_23
            saref_denom = abs(sa_12-sa_23)**m + abs(sa_12-sa_13)**m 
        end where
        where (ctref_denom .eq. 0.0_r8)
            ct_23 = ct_23 + 1.0e-6_r8
            ctref_num = (abs(ct_12-ct_23)**m)*ct_34+(abs(ct_12-ct_13)**m)*ct_23
            ctref_denom = abs(ct_12-ct_23)**m + abs(ct_12-ct_13)**m 
        end where

        sa_ref = 0.5_r8*(sa_12 + (saref_num/saref_denom))
        ct_ref = 0.5_r8*(ct_12 + (ctref_num/ctref_denom))

    end if

    !eqn (3c)
    gamma1_23 = ((p_i(ip_isect) - p(ip_2))*(p_i(ip_isect) - p(ip_3)))/ &
        ((p(ip_1) - p(ip_2))*(p(ip_1) - p(ip_3)))
    gamma2_31 = ((p_i(ip_isect) - p(ip_3))*(p_i(ip_isect) - p(ip_1)))/ &
        ((p(ip_2) - p(ip_3))*(p(ip_2) - p(ip_1)))
    gamma3_12 = ((p_i(ip_isect) - p(ip_1))*(p_i(ip_isect) - p(ip_2)))/ &
        ((p(ip_3) - p(ip_1))*(p(ip_3) - p(ip_2)))

    if (sectnum .eq. 0) then 
        gamma2_34 = ((p_i(ip_isect) - p(ip_3))*(p_i(ip_isect) - p(ip_4)))/ &
            ((p(ip_2) - p(ip_3))*(p(ip_2) - p(ip_4)))
        gamma3_42 = ((p_i(ip_isect) - p(ip_4))*(p_i(ip_isect) - p(ip_2)))/ &
            ((p(ip_3) - p(ip_4))*(p(ip_3) - p(ip_2)))
        gamma4_23 = ((p_i(ip_isect) - p(ip_2))*(p_i(ip_isect) - p(ip_3)))/ &
            ((p(ip_4) - p(ip_2))*(p(ip_4) - p(ip_3)))
    else
        gamma1_24 = ((p_i(ip_isect) - p(ip_2))*(p_i(ip_isect) - p(ip_4)))/ &
            ((p(ip_1) - p(ip_2))*(p(ip_1) - p(ip_4)))
        gamma2_41 = ((p_i(ip_isect) - p(ip_4))*(p_i(ip_isect) - p(ip_1)))/ &
            ((p(ip_2) - p(ip_4))*(p(ip_2) - p(ip_1)))
        gamma4_12 = ((p_i(ip_isect) - p(ip_1))*(p_i(ip_isect) - p(ip_2)))/ &
            ((p(ip_4) - p(ip_1))*(p(ip_4) - p(ip_2)))
    end if

    !eqn (3b/3b')
    sap1 = gamma1_23*sa(ip_1) + gamma2_31*sa(ip_2) + gamma3_12*sa(ip_3)
    ctp1 = gamma1_23*ct(ip_1) + gamma2_31*ct(ip_2) + gamma3_12*ct(ip_3)
    if (sectnum .eq. 0) then
        sap2 = gamma2_34*sa(ip_2) + gamma3_42*sa(ip_3) + gamma4_23*sa(ip_4)
        ctp2 = gamma2_34*ct(ip_2) + gamma3_42*ct(ip_3) + gamma4_23*ct(ip_4)
    else
        sap2 = gamma1_24*sa(ip_1) + gamma2_41*sa(ip_2) + gamma4_12*sa(ip_4)
        ctp2 = gamma1_24*ct(ip_1) + gamma2_41*ct(ip_2) + gamma4_12*ct(ip_4)
    end if

    !eqn (3)
    sa_ref_minus_sap1 = abs(sa_ref - sap1)
    sa_ref_minus_sap2 = abs(sa_ref - sap2)
    where (sa_ref_minus_sap1 .eq. 0.0_r8 .and. sa_ref_minus_sap2 .eq. 0.0_r8)
        sa_ref = sa_ref + 1.0e-6_r8
        sa_ref_minus_sap1 = abs(sa_ref - sap1)
        sa_ref_minus_sap2 = abs(sa_ref - sap2)
    end where

    ct_ref_minus_ctp1 = abs(ct_ref - ctp1)
    ct_ref_minus_ctp2 = abs(ct_ref - ctp2)
    where (ct_ref_minus_ctp1 .eq. 0.0_r8 .and. ct_ref_minus_ctp2 .eq. 0.0_r8)
        ct_ref = ct_ref + 1.0e-6_r8
        ct_ref_minus_ctp1 = abs(ct_ref - ctp1)
        ct_ref_minus_ctp2 = abs(ct_ref - ctp2)
    end where

    sa_i(ip_isect) = (sa_ref_minus_sap1*sap2 + sa_ref_minus_sap2*sap1) / &
                        (sa_ref_minus_sap1 + sa_ref_minus_sap2)
    ct_i(ip_isect) = (ct_ref_minus_ctp1*ctp2 + ct_ref_minus_ctp2*ctp1) / &
                        (ct_ref_minus_ctp1 + ct_ref_minus_ctp2)
    return
    end subroutine rr68_interp_section

end subroutine

!--------------------------------------------------------------------------
