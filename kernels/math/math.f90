module math
  use iso_c_binding
  use iso_fortran_env, only : real128
  implicit none; public
  
  integer, parameter :: dbl = c_double
  integer, parameter :: qbl = real128
  
  real(kind=dbl),    parameter :: zero  = 0._dbl
  real(kind=qbl),    parameter :: qzero = 0._qbl
  real(kind=dbl),    parameter :: one   = 1._dbl
  real(kind=qbl),    parameter :: qone  = 1._qbl
  real(kind=dbl),    parameter :: pi    = acos(-one)
  real(kind=qbl),    parameter :: qpi   = acos(-qone)
  real(kind=dbl),    parameter :: s4pi  = sqrt(4*pi)
  complex(kind=dbl), parameter :: czero = cmplx(zero, zero, kind=dbl)
  complex(kind=dbl), parameter :: cunit = cmplx(zero, one, kind=dbl)
  
#if defined (mem64)
  integer, parameter :: alig = 64  !! memory alignement: AVX512
  integer, parameter :: ndbl = 8   !! number of doubles in one reg. AVX512
#elif defined (mem32)
  integer, parameter :: alig = 32  !! memory alignement: AVX
  integer, parameter :: ndbl = 4   !! number of doubles in one reg. AVX
#endif
  
  interface
    module type(c_ptr) function fortmalloc(alignmt, n) bind(C, name='aligned_alloc')
      integer, value :: alignmt, n
    end function fortmalloc
    
    module subroutine fortfree(ptr) bind(C, name="free")
      type(c_ptr), value :: ptr
    end subroutine fortfree
    
    module elemental function int2str_fn(n) result(str)
      integer,          intent(in) :: n
      character(len=10)            :: str
    end function int2str_fn
    
    module subroutine alloc_aligned_sub( n, c_arr, f_arr )
      integer,                 intent(in)  :: n
      type(c_ptr),             intent(out) :: c_arr
      real(kind=dbl), pointer, intent(out) :: f_arr(:)
    end subroutine alloc_aligned_sub
    
    module subroutine free_aligned_sub( c_arr, f_arr )
      type(c_ptr),             intent(inout) :: c_arr
      real(kind=dbl), pointer, intent(inout) :: f_arr(:)
    end subroutine free_aligned_sub
    
    module subroutine curl_ptp_j_sub( length, fac1, fac2, fac3, fac4, fac5, fac6, darr1, darr2, darr3, arr1, arr2, arr3, &
                                    & curl1, curl2, curl3)
      integer,           intent(in)  :: length
      real(kind=dbl),    intent(in)  :: fac1, fac2, fac3, fac4, fac5, fac6
      complex(kind=dbl), intent(in)  :: darr1(length), darr2(length), darr3(length), arr1(length), arr2(length), arr3(length)
      complex(kind=dbl), intent(out) :: curl1(length), curl2(length), curl3(length)
    end subroutine curl_ptp_j_sub
    
    module subroutine eee2xyz_sub(n, sumPTP, cxyz)
      integer,           intent(in)  :: n
      complex(kind=dbl), intent(in)  :: sumPTP(n,3)
      complex(kind=dbl), intent(out) :: cxyz(3,n)
    end subroutine eee2xyz_sub
    
    module subroutine xy2ee_sub(length, cx, cy)
      integer, value,    intent(in)    :: length
      complex(kind=dbl), intent(inout) :: cx(length), cy(length)
    end subroutine xy2ee_sub
    
    module subroutine trans_4_carray_sub(length, arr_from, arr_to)
      integer,           intent(in)  :: length
      complex(kind=dbl), intent(in)  :: arr_from(4,length)
      complex(kind=dbl), intent(out) :: arr_to(length,4)
    end subroutine trans_4_carray_sub
    
    module subroutine trshf_3_carray_sub(length, v1, v2, v3, ca)
      integer,           intent(in)  :: length
      complex(kind=dbl), intent(in)  :: v1(length,3), v2(length,3), v3(length,3)
      complex(kind=dbl), intent(out) :: ca(9,length)
    end subroutine trshf_3_carray_sub
    
    module subroutine grad_pp_j_sub(length, fac1, fac2, fac3, fac4, darr, arr, grad1, grad3) bind(C, name="grad_pp_j_c")
      integer,        value, intent(in)  :: length
      real(kind=dbl), value, intent(in)  :: fac1, fac2, fac3, fac4
      complex(kind=dbl),     intent(in)  :: darr(*), arr(*)
      complex(kind=dbl),     intent(out) :: grad1(*), grad3(*)
    end subroutine grad_pp_j_sub
    
    module subroutine zero_rarray_sub(istart, length, arr) bind(C, name="zero_rarray_c")
      integer, value, intent(in)  :: istart, length
      real(kind=dbl), intent(out) :: arr(*)
    end subroutine zero_rarray_sub
    
    module subroutine copy_rarray_sub(istart, length, arr_from, arr_to) bind(C, name="copy_rarray_c")
      integer, value, intent(in)  :: istart, length
      real(kind=dbl), intent(in)  :: arr_from(*)
      real(kind=dbl), intent(out) :: arr_to(*)
    end subroutine copy_rarray_sub
    
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
      integer,        value, intent(in)    :: length
      real(kind=dbl), value, intent(in)    :: fac
      complex(kind=dbl),     intent(inout) :: arr(*)
    end subroutine copy1_carray_sub
    
    module subroutine copy2_carray_sub(length, fac, arr_from, arr_to) bind(C, name="copy2_carray_c")
      integer,        value, intent(in)  :: length
      real(kind=dbl), value, intent(in)  :: fac
      complex(kind=dbl),     intent(in)  :: arr_from(*)
      complex(kind=dbl),     intent(out) :: arr_to(*)
    end subroutine copy2_carray_sub
    
    module subroutine copy3_carray_sub(length, fac, arr_from, arr_to) bind(C, name="copy3_carray_c")
      integer,        value, intent(in)    :: length
      real(kind=dbl), value, intent(in)    :: fac
      complex(kind=dbl),     intent(in)    :: arr_from(*)
      complex(kind=dbl),     intent(inout) :: arr_to(*)
    end subroutine copy3_carray_sub
    
    module subroutine cadj3_carray_sub(length, fac, arr_from, arr_to) bind(C, name="cadj3_carray_c")
      integer,        value, intent(in)    :: length
      real(kind=dbl), value, intent(in)    :: fac
      complex(kind=dbl),     intent(in)    :: arr_from(*)
      complex(kind=dbl),     intent(inout) :: arr_to(*) 
    end subroutine cadj3_carray_sub
    
    module subroutine copy4_carray_sub(length, fac1, fac2, fac3, arr1, arr2, arr_to) bind(C, name="copy4_carray_c")
      integer,        value, intent(in)    :: length
      real(kind=dbl), value, intent(in)    :: fac1, fac2, fac3
      complex(kind=dbl),     intent(in)    :: arr1(*), arr2(*)
      complex(kind=dbl),     intent(inout) :: arr_to(*)
    end subroutine copy4_carray_sub
    
    module subroutine copy5_carray_sub(length, fac1, fac2, fac3, fac4, arr1, arr2, arr3, arr_to) bind(C, name="copy5_carray_c")
      integer,        value, intent(in)    :: length
      real(kind=dbl), value, intent(in)    :: fac1, fac2, fac3, fac4
      complex(kind=dbl),     intent(in)    :: arr1(*), arr2(*), arr3(*)
      complex(kind=dbl),     intent(inout) :: arr_to(*)
    end subroutine copy5_carray_sub
    
    module subroutine swap_carray_sub(length, arr1, arr2) bind(C, name="swap_carray_c")
      integer, value,    intent(in)    :: length
      complex(kind=dbl), intent(inout) :: arr1(*), arr2(*)
    end subroutine swap_carray_sub
    
    module subroutine gcopy_sub(n, arr_from, arr_to) bind(C, name="gcopy_c")
      integer, value, intent(in)  :: n
      real(kind=dbl), intent(in)  :: arr_from(*)
      real(kind=dbl), intent(out) :: arr_to(*)
    end subroutine gcopy_sub
  end interface
  
end module math
