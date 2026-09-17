module sphsvt
  use sph
  implicit none
  
  type, public :: T_sphsvt
    integer :: jmax, jmax1, jms, jms1
    
    contains
    
    procedure :: init_sub => init_sphsvt_sub
    procedure :: alloc_work_rxd_sub
    procedure :: scal2scal_mj_to_jm_sub, vec2scal_jm_to_mj_sub, scal2vec_mj_to_jm_sub
    
  end type T_sphsvt
  
  interface
    module subroutine init_sphsvt_sub(this, jmax)
      class(T_sphsvt), intent(inout) :: this
      integer,         intent(in)    :: jmax
    end subroutine init_sphsvt_sub
    
    module subroutine alloc_work_rxd_sub(this, n, work)
      class(T_sphsvt),                intent(in) :: this
      integer,                        intent(in) :: n
      complex(kind=dbl), allocatable, intent(out) :: work(:)
    end subroutine alloc_work_rxd_sub
    
    module subroutine vec2scal_jm_to_mj_sub(this, nca, ca, cc)
      class(T_sphsvt),   intent(in)  :: this
      integer,           intent(in)  :: nca
      complex(kind=dbl), intent(in)  :: ca(nca,3,*)
      complex(kind=dbl), intent(out) :: cc(3*nca,*)
    end subroutine vec2scal_jm_to_mj_sub
    
    module subroutine scal2scal_mj_to_jm_sub(this, cr, cjm)
      class(T_sphsvt),   intent(in)    :: this
      complex(kind=dbl), intent(in)    :: cr(this%jms1)
      complex(kind=dbl), intent(inout) :: cjm(this%jms)
    end subroutine scal2scal_mj_to_jm_sub
    
    module subroutine scal2vec_mj_to_jm_sub(this, cr, cjm1, cjm2, cjm3)
      class(T_sphsvt),   intent(in)    :: this
      complex(kind=dbl), intent(inout) :: cr(this%jms1,3)
      complex(kind=dbl), intent(inout) :: cjm1(this%jms), cjm2(this%jms), cjm3(this%jms)
    end subroutine scal2vec_mj_to_jm_sub
    
    module real(kind=dbl) function cleb1_fn(j1, m1, j2, m2, j, m)
      integer, intent(in) :: j1, m1, j2, m2, j, m
    end function cleb1_fn
    
    module subroutine ee2xy_sub(n, cx, cy) bind(C, name="ee2xy_c")
      integer, value,    intent(in)    :: n
      complex(kind=dbl), intent(inout) :: cx(*), cy(*)
    end subroutine ee2xy_sub
    
    module subroutine xy2ee_sub(n, cx, cy) bind(C, name="xy2ee_c")
      integer, value,    intent(in)    :: n
      complex(kind=dbl), intent(inout) :: cx(*), cy(*)
    end subroutine xy2ee_sub
    
    module subroutine trans_4_carray_sub(length, arr_from, arr_to) bind(C, name="trans_4_carray_c")
      integer, value,    intent(in)  :: length
      complex(kind=dbl), intent(in)  :: arr_from(*)
      complex(kind=dbl), intent(out) :: arr_to(*)
    end subroutine trans_4_carray_sub
    
    module subroutine trshf_3_3_carray_sub(length, arr_from, arr_to) bind(C, name="trshf_3_3_carray_c")
      integer, value,    intent(in)  :: length
      complex(kind=dbl), intent(in)  :: arr_from(*)
      complex(kind=dbl), intent(out) :: arr_to(*)
    end subroutine trshf_3_3_carray_sub
    
    module subroutine trshf_3x3_9_carray_sub(length, v1, v2, v3, ca) bind(C, name="trshf_3x3_9_carray_c")
      integer, value,    intent(in)  :: length
      complex(kind=dbl), intent(in)  :: v1(*), v2(*), v3(*)
      complex(kind=dbl), intent(out) :: ca(*)
    end subroutine trshf_3x3_9_carray_sub
  end interface
  
end module sphsvt