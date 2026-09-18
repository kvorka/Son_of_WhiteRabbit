module ocean
  use ocean_parms
  use physicalobject
  implicit none
  
  type, extends(T_physicalObject), public :: T_ocean
    
    contains
    
    procedure, public, pass :: init_sub        => init_ocean_sub
    procedure, public, pass :: time_scheme_sub => time_scheme_ocean_sub
    procedure, public, pass :: write_state_sub => write_state_ocean_sub
    procedure, public, pass :: deallocate_sub  => deallocate_ocean_sub
    
  end type T_ocean
  
  interface
    module subroutine init_ocean_sub(this, speed)
      class(T_ocean), intent(inout) :: this
      logical,        intent(in)    :: speed
    end subroutine init_ocean_sub
    
    module subroutine time_scheme_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine time_scheme_ocean_sub
    
    module subroutine write_state_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine write_state_ocean_sub
    
    module subroutine deallocate_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine deallocate_ocean_sub
  end interface
  
end module ocean