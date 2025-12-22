module sphsvt
  use cleb
  use sphbase
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
    
    module subroutine vec2scal_jm_to_mj_sub(this, v1, v2, v3, cc)
      class(T_sphsvt),   intent(in)    :: this
      complex(kind=dbl), intent(in)    :: v1(this%jms,3), v2(this%jms,3), v3(this%jms,3)
      complex(kind=dbl), intent(inout) :: cc(9,*)
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
  
end module sphsvt