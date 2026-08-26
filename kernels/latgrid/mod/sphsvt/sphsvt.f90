module sphsvt
  use sph
  implicit none
  
  type, public :: T_sphsvt
    integer :: jmax, jmax1, jmax2, jms, jms1
    
    contains
    
    procedure :: init_sub => init_sphsvt_sub
    procedure :: scal2scal_mj_to_jm_sub, vec2scal_jm_to_mj_sub, scal2vec_mj_to_jm_sub
    
  end type T_sphsvt
  
  interface
    module subroutine init_sphsvt_sub(this, jmax)
      class(T_sphsvt), intent(inout) :: this
      integer,         intent(in)    :: jmax
    end subroutine init_sphsvt_sub
    
    module subroutine vec2scal_jm_to_mj_sub(this, nca, ca, cc)
      class(T_sphsvt),   intent(in)  :: this
      integer,           intent(in)  :: nca
      complex(kind=dbl), intent(in)  :: ca(nca,3,*)
      complex(kind=dbl), intent(out) :: cc(3*nca,*)
    end subroutine vec2scal_jm_to_mj_sub
    
    module subroutine scal2scal_mj_to_jm_sub(this, cr, ncr, crpadding, cjm)
      class(T_sphsvt),   intent(in)    :: this
      integer,           intent(in)    :: ncr, crpadding
      complex(kind=dbl), intent(in)    :: cr(ncr,*)
      complex(kind=dbl), intent(inout) :: cjm(*)
    end subroutine scal2scal_mj_to_jm_sub
    
    module subroutine scal2vec_mj_to_jm_sub(this, cr, ncr, crpadding, cjm1, cjm2, cjm3)
      class(T_sphsvt),   intent(in)    :: this
      integer,           intent(in)    :: ncr, crpadding
      complex(kind=dbl), intent(inout) :: cr(ncr,*)
      complex(kind=dbl), intent(inout) :: cjm1(*), cjm2(*), cjm3(*)
    end subroutine scal2vec_mj_to_jm_sub
  end interface
  
  interface
    module real(kind=dbl) function cleb1_fn(j1, m1, j2, m2, j, m)
      integer, intent(in) :: j1, m1, j2, m2, j, m
    end function cleb1_fn

#if defined ( kernelC )
    module subroutine cadj3_carray_sub(n, fac, arr_from, arr_to) bind(C, name="cadj3_carray_c")
      integer, value,    intent(in)    :: n
      real(kind=dbl),    intent(in)    :: fac
      complex(kind=dbl), intent(in)    :: arr_from(*)
      complex(kind=dbl), intent(inout) :: arr_to(*) 
    end subroutine cadj3_carray_sub
    
    module subroutine eee2xyz_sub(n, sumPTP, cc) bind(C, name="eee2xyz_c")
      integer, value,    intent(in)  :: n
      complex(kind=dbl), intent(in)  :: sumPTP(*)
      complex(kind=dbl), intent(out) :: cc(*)
    end subroutine eee2xyz_sub
    
    module subroutine copy_vcvv_vcvxv_sub(n, v, q, curlv, ca) bind(C, name="copy_vcvv_vcvxv_c")
      integer, value,    intent(in)  :: n
      complex(kind=dbl), intent(in)  :: v(*), q(*), curlv(*)
      complex(kind=dbl), intent(out) :: ca(*)
    end subroutine copy_vcvv_vcvxv_sub
#else
    module subroutine cadj3_carray_sub(n, fac, arr_from, arr_to)
      integer,           intent(in)    :: n
      real(kind=dbl),    intent(in)    :: fac
      complex(kind=dbl), intent(in)    :: arr_from(n)
      complex(kind=dbl), intent(inout) :: arr_to(n) 
    end subroutine cadj3_carray_sub
    
    module subroutine eee2xyz_sub(n, sumPTP, cc)
      integer,           intent(in)  :: n
      complex(kind=dbl), intent(in)  :: sumPTP(n,3)
      complex(kind=dbl), intent(out) :: cc(3,n)
    end subroutine eee2xyz_sub
    
    module subroutine copy_vcvv_vcvxv_sub(n, v, q, curlv, ca)
      integer,           intent(in)  :: n
      complex(kind=dbl), intent(in)  :: v(n,3), q(n,3), curlv(n,3)
      complex(kind=dbl), intent(out) :: ca(3,3,n)
    end subroutine copy_vcvv_vcvxv_sub
#endif
  end interface
  
end module sphsvt