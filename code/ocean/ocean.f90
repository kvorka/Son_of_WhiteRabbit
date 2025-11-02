module ocean
  use physicalobject
  use ocean_constants
  use omp_lib
  implicit none
  
  type, extends(T_physicalObject), public :: T_ocean
    complex(kind=dbl), allocatable :: nsph1(:,:), nsph2(:,:), ntorr(:,:), ntemp(:,:)
    
    contains
    
    procedure, public, pass :: init_sub        => init_ocean_sub
    procedure, public, pass :: deallocate_sub  => deallocate_ocean_sub
    
    procedure, public, pass :: init_state_sub     => init_state_ocean_sub
    procedure, public, pass :: init_temp_bbnd_sub => init_temp_bbnd_ocean_sub
    procedure, public, pass :: fullnl_sub         => fullnl2_ocean_sub
    procedure, public, pass :: time_scheme_sub    => time_scheme2_ocean_sub
    procedure, public, pass :: iter_sub           => iter_ocean_sub
    procedure, public, pass :: speed_sub          => speed_ocean_sub
    procedure, public, pass :: vypis_ocean_sub    => vypis_ocean_sub
    
  end type T_ocean
  
  interface
    module subroutine init_ocean_sub(this, speed)
      class(T_ocean), intent(inout)          :: this
      logical,        intent(in),   optional :: speed
    end subroutine init_ocean_sub
    
    module subroutine deallocate_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine deallocate_ocean_sub
    
    module subroutine init_temp_bbnd_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine init_temp_bbnd_ocean_sub
    
    module subroutine init_state_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine init_state_ocean_sub
    
    module subroutine time_scheme_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine time_scheme_ocean_sub
    
    module subroutine time_scheme2_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine time_scheme2_ocean_sub
    
    module subroutine iter_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine iter_ocean_sub
    
    module subroutine speed_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine speed_ocean_sub
    
    module subroutine vypis_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine vypis_ocean_sub
    
    module subroutine fullnl_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine fullnl_ocean_sub
    
    module subroutine fullnl2_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine fullnl2_ocean_sub
  end interface
  
end module ocean