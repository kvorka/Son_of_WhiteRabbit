module math
  use calloc
  implicit none; public
  
  real(kind=dbl), parameter :: deps = 1.0d-15
  real(kind=qbl), parameter :: qeps = 1.0d-28
  
  real(kind=dbl), parameter :: zero  = 0._dbl
  real(kind=qbl), parameter :: qzero = 0._qbl
  real(kind=dbl), parameter :: one   = 1._dbl
  real(kind=qbl), parameter :: qone  = 1._qbl
  
  real(kind=dbl), parameter :: sq2_1 = 1 / sqrt(2._dbl)
  real(kind=dbl), parameter :: pi    = acos(-one)
  real(kind=qbl), parameter :: qpi   = acos(-qone)
  real(kind=dbl), parameter :: s4pi  = sqrt(4*pi)
  
  complex(kind=dbl), parameter :: cunit = cmplx(zero, one , kind=dbl)
  complex(kind=dbl), parameter :: czero = cmplx(zero, zero, kind=dbl)
  complex(kind=dbl), parameter :: cs4pi = cmplx(s4pi, zero, kind=dbl)
  
  interface
    module elemental function int2str_fn(n) result(str)
      integer,          intent(in) :: n
      character(len=10)            :: str
    end function int2str_fn
    
    module elemental real(kind=dbl) function i2r_fn(ix)
      integer, intent(in) :: ix
    end function i2r_fn
    
    module elemental real(kind=dbl) function q2r_fn(qx)
      real(kind=qbl), intent(in) :: qx
    end function q2r_fn
    
    module elemental real(kind=dbl) function c2r_fn(cx)
      complex(kind=dbl), intent(in) :: cx
    end function c2r_fn
    
    module elemental complex(kind=dbl) function r2c_fn(x)
      real(kind=dbl), intent(in) :: x
    end function r2c_fn
    
    module subroutine trans_carray_sub(n, length, arr_from, arr_to)
      integer,           intent(in)  :: n, length
      complex(kind=dbl), intent(in)  :: arr_from(n,length)
      complex(kind=dbl), intent(out) :: arr_to(length,n)
    end subroutine trans_carray_sub
    
    module subroutine alloc_aligned_sub( n, c_arr, f_arr )
      integer,                 intent(in)  :: n
      type(c_ptr),             intent(out) :: c_arr
      real(kind=dbl), pointer, intent(out) :: f_arr(:)
    end subroutine alloc_aligned_sub
    
    module subroutine free_aligned_sub( c_arr, f_arr )
      type(c_ptr),             intent(inout) :: c_arr
      real(kind=dbl), pointer, intent(inout) :: f_arr(:)
    end subroutine free_aligned_sub
    
#if defined ( kernelC )
    module subroutine zero_carray_sub(length, arr) bind(C, name="zero_carray_c")
      integer, value,    intent(in)  :: length
      complex(kind=dbl), intent(out) :: arr(*)
    end subroutine zero_carray_sub
    
    module subroutine copy_carray_sub(length, arr_from, arr_to) bind(C, name="copy_carray_c")
      integer, value,    intent(in)  :: length
      complex(kind=dbl), intent(in)  :: arr_from(*)
      complex(kind=dbl), intent(out) :: arr_to(*)
    end subroutine copy_carray_sub
    
    module subroutine copy1_carray_sub(length, fac, arr) bind(C, name="copy1_carray_c")
      integer, value,    intent(in)    :: length
      real(kind=dbl),    intent(in)    :: fac
      complex(kind=dbl), intent(inout) :: arr(*)
    end subroutine copy1_carray_sub
    
    module subroutine copy2_carray_sub(length, fac, arr_from, arr_to) bind(C, name="copy2_carray_c")
      integer, value,    intent(in)  :: length
      real(kind=dbl),    intent(in)  :: fac
      complex(kind=dbl), intent(in)  :: arr_from(*)
      complex(kind=dbl), intent(out) :: arr_to(*)
    end subroutine copy2_carray_sub
    
    module subroutine copy3_carray_sub(length, fac, arr_from, arr_to) bind(C, name="copy3_carray_c")
      integer, value,    intent(in)    :: length
      real(kind=dbl),    intent(in)    :: fac
      complex(kind=dbl), intent(in)    :: arr_from(*)
      complex(kind=dbl), intent(inout) :: arr_to(*)
    end subroutine copy3_carray_sub
#else
    module subroutine zero_carray_sub(length, arr)
      integer,           intent(in)  :: length
      complex(kind=dbl), intent(out) :: arr(length)
    end subroutine zero_carray_sub
    
    module subroutine copy_carray_sub(length, arr_from, arr_to)
      integer,           intent(in)  :: length
      complex(kind=dbl), intent(in)  :: arr_from(length)
      complex(kind=dbl), intent(out) :: arr_to(length)
    end subroutine copy_carray_sub
    
    module subroutine copy1_carray_sub(length, fac, arr)
      integer,           intent(in)    :: length
      real(kind=dbl),    intent(in)    :: fac
      complex(kind=dbl), intent(inout) :: arr(length)
    end subroutine copy1_carray_sub
    
    module subroutine copy2_carray_sub(length, fac, arr_from, arr_to)
      integer,           intent(in)  :: length
      real(kind=dbl),    intent(in)  :: fac
      complex(kind=dbl), intent(in)  :: arr_from(length)
      complex(kind=dbl), intent(out) :: arr_to(length)
    end subroutine copy2_carray_sub
    
    module subroutine copy3_carray_sub(length, fac, arr_from, arr_to)
      integer,           intent(in)    :: length
      real(kind=dbl),    intent(in)    :: fac
      complex(kind=dbl), intent(in)    :: arr_from(length)
      complex(kind=dbl), intent(inout) :: arr_to(length)
    end subroutine copy3_carray_sub
#endif
  end interface
  
end module math
