! IBM libmassv compatibility library
! 
#define R4KIND selected_real_kind(6)
#define R8KIND selected_real_kind(12)

#ifndef NATIVE_MASSV
      subroutine vdiv(z,x,y,n)
      real(kind=R8KIND) x(*),y(*),z(*)
      integer n
      integer j
      do 10 j=1,n
      z(j)=x(j)/y(j)
   10 continue
      return
      end

      subroutine vsdiv(z,x,y,n)
      real(kind=R4KIND) x(*),y(*),z(*)
      integer n
      integer j
      do 10 j=1,n
      z(j)=x(j)/y(j)
   10 continue
      return
      end

      subroutine vexp(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=exp(x(j))
   10 continue
      return
      end

      subroutine vsexp(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=exp(x(j))
   10 continue
      return
      end

      subroutine vlog(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=log(x(j))
   10 continue
      return
      end

      subroutine vslog(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=log(x(j))
   10 continue
      return
      end

      subroutine vrec(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=1.d0/x(j)
   10 continue
      return
      end

      subroutine vsrec(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=1.e0/x(j)
   10 continue
      return
      end

      subroutine vrsqrt(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=1.d0/sqrt(x(j))
   10 continue
      return
      end

      subroutine vsrsqrt(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=1.e0/sqrt(x(j))
   10 continue
      return
      end

      subroutine vsincos(x,y,z,n)
      real(kind=R8KIND) x(*),y(*),z(*)
      integer n
      integer j
      do 10 j=1,n
      x(j)=sin(z(j))
      y(j)=cos(z(j))
   10 continue
      return
      end

      subroutine vssincos(x,y,z,n)
      real(kind=R4KIND) x(*),y(*),z(*)
      integer n
      integer j
      do 10 j=1,n
      x(j)=sin(z(j))
      y(j)=cos(z(j))
   10 continue
      return
      end

      subroutine vsqrt(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=sqrt(x(j))
   10 continue
      return
      end

      subroutine vssqrt(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=sqrt(x(j))
   10 continue
      return
      end

      subroutine vtan(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=tan(x(j))
   10 continue
      return
      end

      subroutine vstan(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=tan(x(j))
   10 continue
      return
      end

      subroutine vatan2(z,y,x,n)
      real(kind=R8KIND) x(*),y(*),z(*)
      integer n
      integer j
      do 10 j=1,n
      z(j)=atan2(y(j),x(j))
   10 continue
      return
      end

      subroutine vsatan2(z,y,x,n)
      real(kind=R4KIND) x(*),y(*),z(*)
      integer n
      integer j
      do 10 j=1,n
      z(j)=atan2(y(j),x(j))
   10 continue
      return
      end

      subroutine vasin(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=asin(x(j))
   10 continue
      return
      end

      subroutine vsin(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=sin(x(j))
   10 continue
      return
      end

      subroutine vssin(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=sin(x(j))
   10 continue
      return
      end

      subroutine vacos(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=acos(x(j))
   10 continue
      return
      end

      subroutine vcos(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=cos(x(j))
   10 continue
      return
      end

      subroutine vscos(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=cos(x(j))
   10 continue
      return
      end

      subroutine vcosisin(y,x,n)
      complex(kind=R8KIND) y(*)
      real(kind=R8KIND) x(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=cmplx(cos(x(j)),sin(x(j)),kind=R8KIND)
   10 continue
      return
      end

      subroutine vscosisin(y,x,n)
      complex(kind=R4KIND) y(*)
      real(kind=R4KIND) x(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)= cmplx(cos(x(j)),sin(x(j)),kind=R4KIND)
   10 continue
      return
      end

      subroutine vdint(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
!     y(j)=dint(x(j))
      y(j)=int(x(j))
   10 continue
      return
      end

      subroutine vdnint(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
!     y(j)=dnint(x(j))
      y(j)=nint(x(j))
   10 continue
      return
      end

      subroutine vlog10(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=log10(x(j))
   10 continue
      return
      end

!      subroutine vlog1p(y,x,n)
!      real(kind=R8KIND) x(*),y(*)
!      interface
!        real(kind=R8KIND) function log1p(%val(x))
!          real(kind=R8KIND) x
!        end function log1p
!      end interface
!      do 10 j=1,n
!      y(j)=log1p(x(j))
!   10 continue
!      return
!      end

      subroutine vcosh(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=cosh(x(j))
   10 continue
      return
      end

      subroutine vsinh(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=sinh(x(j))
   10 continue
      return
      end

      subroutine vtanh(y,x,n)
      real(kind=R8KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=tanh(x(j))
   10 continue
      return
      end

!      subroutine vexpm1(y,x,n)
!      real(kind=R8KIND) x(*),y(*)
!      interface
!        real(kind=R8KIND) function expm1(%val(x))
!          real(kind=R8KIND) x
!        end function expm1
!      end interface 
!      do 10 j=1,n
!      y(j)=expm1(x(j))
!   10 continue
!      return
!      end


      subroutine vsasin(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=asin(x(j))
   10 continue
      return
      end

      subroutine vsacos(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
#if defined (G95)
! no reason why g95 should fail - oh well, we don't use this routine anyways
      y(j)=asin( sqrt(1-x(j)*x(j)) )
#else
      y(j)=acos(x(j))
#endif
   10 continue
      return
      end

      subroutine vscosh(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=cosh(x(j))
   10 continue
      return
      end

!      subroutine vsexpm1(y,x,n)
!      real(kind=R4KIND) x(*),y(*)
!      interface
!        real(kind=R8KIND) function expm1(%val(x))
!          real(kind=R8KIND) x
!        end function expm1
!      end interface
!      do 10 j=1,n
!      y(j)=expm1(real(x(j),8))
!   10 continue
!      return
!      end

      subroutine vslog10(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=log10(x(j))
   10 continue
      return
      end

!      subroutine vslog1p(y,x,n)
!      real(kind=R4KIND) x(*),y(*)
!      interface
!        real(kind=R8KIND) function log1p(%val(x))
!          real(kind=R8KIND) x
!        end function log1p
!      end interface
!      do 10 j=1,n
!      y(j)=log1p(real(x(j),8))
!   10 continue
!      return
!      end


      subroutine vssinh(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=sinh(x(j))
   10 continue
      return
      end

      subroutine vstanh(y,x,n)
      real(kind=R4KIND) x(*),y(*)
      integer n
      integer j
      do 10 j=1,n
      y(j)=tanh(x(j))
   10 continue
      return
      end
#endif

      subroutine vspow(z,y,x,n)
      real(kind=R4KIND) x(*),y(*),z(*)
      integer n
      integer j
      do 10 j=1,n
      z(j)=y(j)**x(j)
   10 continue
      return
      end

      subroutine vpow(z,y,x,n)
      real(kind=R8KIND) x(*),y(*),z(*)
      integer n
      integer j
      do 10 j=1,n
      z(j)=y(j)**x(j)
   10 continue
      return
      end

