module sphsvt2
  use math
  use sph
  implicit none
  
  type, public :: T_sphsvt2
    integer :: jmax, jms
    
    contains
    
    procedure :: init_sub    => init_sphsvt2_sub
    procedure :: jml2qst_sub => jml2qst_sphsvt2_sub
    
  end type T_sphsvt2
  
  interface
    module subroutine init_sphsvt2_sub(this, jmax)
      class(T_sphsvt2), intent(inout) :: this
      integer,          intent(in)    :: jmax
    end subroutine init_sphsvt2_sub
    
    module subroutine jml2qst_sphsvt2_sub(this, vec)
      class(T_sphsvt2),  intent(in)    :: this
      complex(kind=dbl), intent(inout) :: vec(this%jms,3)
    end subroutine jml2qst_sphsvt2_sub
  end interface
  
end module sphsvt2