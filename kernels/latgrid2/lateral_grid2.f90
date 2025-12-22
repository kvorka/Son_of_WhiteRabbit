module lateral_grid2
  use math
  use sph
  use sphsvt2
  implicit none
  include 'shtns.f03'
  
  type, public :: T_lateralGrid2
    integer,     public :: nL, nF, jmax, jms
    type(c_ptr), public :: shtns_c
    
    contains
    
    procedure, pass :: init_sub     => init_shtns_sub
    procedure, pass :: q2grid_sub   => q2grid_shtns_sub
    procedure, pass :: qst2grid_sub => qst2grid_shtns_sub
    
  end type T_lateralGrid2
  
  interface
    module subroutine init_shtns_sub(this, jmax)
      class(T_lateralGrid2), intent(inout) :: this
      integer,               intent(in)    :: jmax
    end subroutine init_shtns_sub
    
    module subroutine q2grid_sub(this, scal, grid)
      class(T_lateralGrid2), intent(in)    :: this
      complex(kind=dbl),     intent(inout) :: scal(this%jms)
      real(kind=dbl),        intent(out)   :: grid(this%nL * this%nF)
    end subroutine q2grid_sub
    
    module subroutine qst2grid_shtns_sub(this, vec, grid)
      class(T_lateralGrid2), intent(in)    :: this
      complex(kind=dbl),     intent(inout) :: vec(this%jms,3)
      real(kind=dbl),        intent(out)   :: grid(this%nL * this%nF,3)
    end subroutine qst2grid_shtns_sub
  end interface
  
end module lateral_grid2