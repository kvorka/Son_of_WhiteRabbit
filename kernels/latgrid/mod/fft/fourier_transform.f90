module fourier_transform
  !! Original work: fxpack (ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING, Keiichi Ishioka)
  use math
  implicit none
  
  type, public :: T_fft
    integer                     :: n
    integer,        allocatable :: it(:)
    real(kind=dbl), allocatable :: t(:)
    
    contains
    
    procedure, public, pass :: init_sub => fft_init_sub
    procedure, public, pass :: fft_c2r_sub
    procedure, public, pass :: fft_r2c_sub
    procedure, public, pass :: deallocate_sub => fft_deallocate_sub
    
  end type T_fft
  
  integer, parameter :: imm = -2e4
  
  interface
    module subroutine fft_init_sub(this, n)
      class(T_fft), intent(inout) :: this
      integer,      intent(in)    :: n
    end subroutine fft_init_sub
    
    module subroutine fft_deallocate_sub(this)
      class(T_fft), intent(inout) :: this
    end subroutine fft_deallocate_sub
    
    module subroutine fft_r2c_sub(this, m, x)
      class(T_fft),      intent(in)    :: this
      integer,           intent(in)    :: m
      real(kind=dbl),    intent(inout) :: x(4*m*ndbl,2,0:this%n/2-1)
    end subroutine fft_r2c_sub
    
    module subroutine fft_c2r_sub(this, m, x)
      class(T_fft),   intent(in)    :: this
      integer,        intent(in)    :: m
      real(kind=dbl), intent(inout) :: x(4*m*ndbl,2,0:this%n/2-1)
    end subroutine fft_c2r_sub
    
    module subroutine fxzini(n, it, t)
      integer,        intent(in)  :: n
      integer,        intent(out) :: it(n)
      real(kind=dbl), intent(out) :: t(2,0:n-1)
    end subroutine fxzini
    
    module subroutine fxzshf(n, it, m, x)
      integer,        intent(in)    :: n, m, it(*)
      real(kind=dbl), intent(inout) :: x(8*m*ndbl,0:n/2-1)
    end subroutine fxzshf
    
    module subroutine fxztal(n, it, t, m, x)
      integer,        intent(in)    :: n, m, it(2)
      real(kind=dbl), intent(in)    :: t(0:*)
      real(kind=dbl), intent(inout) :: x(*)
    end subroutine fxztal
    
    module subroutine fxcpy(m, arr_from, arr_to) bind(C, name="fxcpy_c")
      integer, value, intent(in)  :: m
      real(kind=dbl), intent(in)  :: arr_from(*)
      real(kind=dbl), intent(out) :: arr_to(*)
    end subroutine fxcpy
    
    module subroutine fxaddsub(m, arr1, arr2) bind(C, name="fxaddsub_c")
      integer, value, intent(in)    :: m
      real(kind=dbl), intent(inout) :: arr1(*), arr2(*)
    end subroutine fxaddsub
    
    module subroutine fxrsc(m, fac, arr) bind(C, name="fxrsc_c")
      integer,        value, intent(in)    :: m
      real(kind=dbl), value, intent(in)    :: fac
      real(kind=dbl),        intent(inout) :: arr(*)
    end subroutine fxrsc
    
    module subroutine fxc2r(m, t, x11, x12, x21, x22) bind(C, name="fxc2r_c")
      integer, value, intent(in)    :: m
      real(kind=dbl), intent(in)    :: t(*)
      real(kind=dbl), intent(inout) :: x11(*), x12(*), x21(*), x22(*)
    end subroutine fxc2r
    
    module subroutine fxr2c(m, t, x11, x12, x21, x22) bind(C, name="fxr2c_c")
      integer, value, intent(in)    :: m
      real(kind=dbl), intent(in)    :: t(*)
      real(kind=dbl), intent(inout) :: x11(*), x12(*), x21(*), x22(*)
    end subroutine fxr2c
    
    module subroutine fxzm2a(m, k, l, x, t) bind(C, name="fxzm2a_c")
      integer, value, intent(in)    :: m, k, l
      real(kind=dbl), intent(in)    :: t(*)
      real(kind=dbl), intent(inout) :: x(*)
    end subroutine fxzm2a
    
    module subroutine fxzm2b(m, l, x) bind(C, name="fxzm2b_c")
      integer, value, intent(in)    :: m, l
      real(kind=dbl), intent(inout) :: x(*)
    end subroutine fxzm2b
    
    module subroutine fxzm3a(m, k, l, x, t) bind(C, name="fxzm3a_c")
      integer, value, intent(in)    :: m, k, l
      real(kind=dbl), intent(in)    :: t(*)
      real(kind=dbl), intent(inout) :: x(*)
    end subroutine fxzm3a
    
    module subroutine fxzm3b(m, l, x) bind(C, name="fxzm3b_c")
      integer, value, intent(in)    :: m, l
      real(kind=dbl), intent(inout) :: x(*)
    end subroutine fxzm3b
    
    module subroutine fxzm4a(m, k, l, x, t) bind(C, name="fxzm4a_c")
      integer, value, intent(in)    :: m, k, l
      real(kind=dbl), intent(in)    :: t(*)
      real(kind=dbl), intent(inout) :: x(*)
    end subroutine fxzm4a
    
    module subroutine fxzm4b(m, l, x) bind(C, name="fxzm4b_c")
      integer, value, intent(in)    :: m, l
      real(kind=dbl), intent(inout) :: x(*)
    end subroutine fxzm4b
    
    module subroutine fxzm5a(m, k, l, x, t) bind(C, name="fxzm5a_c")
      integer, value, intent(in)    :: m, k, l
      real(kind=dbl), intent(in)    :: t(*)
      real(kind=dbl), intent(inout) :: x(*)
    end subroutine fxzm5a
    
    module subroutine fxzm5b(m, l, x) bind(C, name="fxzm5b_c")
      integer, value, intent(in)    :: m, l
      real(kind=dbl), intent(inout) :: x(*)
    end subroutine fxzm5b
  end interface
  
end module fourier_transform
