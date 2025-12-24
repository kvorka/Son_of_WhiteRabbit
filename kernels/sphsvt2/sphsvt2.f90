module sphsvt2
  use math
  use sph
  implicit none
  include '../../shtns/include/shtns.f03'
  
  type, public :: T_sphsvt2
    integer :: jmax, jms
    
    contains
    
    procedure :: init_sub        => init_sphsvt2_sub
    procedure :: jm_to_shtns_sub => jm_to_shtns_sphsvt2_sub
    procedure :: shtns_to_jm_sub => shtns_to_jm_sphsvt2_sub
    procedure :: jml_to_qst_sub  => jml_to_qst_sphsvt2_sub
    procedure :: qst_to_jml_sub  => qst_to_jml_sphsvt2_sub
    
  end type T_sphsvt2
  
  interface
    module subroutine init_sphsvt2_sub(this, jmax)
      class(T_sphsvt2), intent(inout) :: this
      integer,          intent(in)    :: jmax
    end subroutine init_sphsvt2_sub
    
    module subroutine jm_to_shtns_sphsvt2_sub(this, shtns_c, arr)
      class(T_sphsvt2),  intent(in)    :: this
      type(c_ptr),       intent(in)    :: shtns_c
      complex(kind=dbl), intent(inout) :: arr(this%jms)
    end subroutine jm_to_shtns_sphsvt2_sub
    
    module subroutine shtns_to_jm_sphsvt2_sub(this, shtns_c, arr)
      class(T_sphsvt2),  intent(in)    :: this
      type(c_ptr),       intent(in)    :: shtns_c
      complex(kind=dbl), intent(inout) :: arr(this%jms)
    end subroutine shtns_to_jm_sphsvt2_sub
    
    module subroutine jml_to_qst_sphsvt2_sub(this, pol1, torr, pol2)
      class(T_sphsvt2),  intent(in)    :: this
      complex(kind=dbl), intent(inout) :: pol1(this%jms), torr(this%jms), pol2(this%jms)
    end subroutine jml_to_qst_sphsvt2_sub
    
    module subroutine qst_to_jml_sphsvt2_sub(this, pol1, torr, pol2)
      class(T_sphsvt2),  intent(in)    :: this
      complex(kind=dbl), intent(inout) :: pol1(this%jms), torr(this%jms), pol2(this%jms)
    end subroutine qst_to_jml_sphsvt2_sub
  end interface
  
end module sphsvt2