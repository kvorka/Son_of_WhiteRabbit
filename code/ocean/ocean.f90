module ocean
  use physicalobject
  use ocean_constants
  implicit none
  
  type, extends(T_physicalObject), public :: T_ocean
    
    contains
    
    procedure, public, pass :: init_sub        => init_ocean_sub
    procedure, public, pass :: deallocate_sub  => deallocate_ocean_sub
    
    procedure, public, pass :: vgradT_vcurlv_sub => vgradT_vcurlv_ocean_sub
    procedure, public, pass :: time_scheme_sub   => time_scheme_ocean_sub
    procedure, public, pass :: vypis_ocean_sub   => vypis_ocean_sub
    
  end type T_ocean
  
  interface
    module subroutine init_ocean_sub(this, speed)
      class(T_ocean), intent(inout) :: this
      logical,        intent(in)    :: speed
    end subroutine init_ocean_sub
    
    module subroutine deallocate_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine deallocate_ocean_sub
    
    module subroutine time_scheme_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine time_scheme_ocean_sub
    
    module subroutine vypis_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine vypis_ocean_sub
    
    module subroutine vgradT_vcurlv_ocean_sub(this, q, curlv, v, ntemp, nsph1, ntorr, nsph2)
      class(T_ocean),    intent(inout) :: this
      complex(kind=dbl), intent(in)    :: curlv(*), q(*), v(*)
      complex(kind=dbl), intent(out)   :: ntemp(*), nsph1(*), ntorr(*), nsph2(*)
    end subroutine vgradT_vcurlv_ocean_sub
  end interface
  
  interface
    module subroutine init_temp_bbnd_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine init_temp_bbnd_ocean_sub
    
    module subroutine init_state_ocean_sub(this)
      class(T_ocean), intent(inout) :: this
    end subroutine init_state_ocean_sub
    
    module subroutine grid_op_vgradT_vcurlv_sub(nfour, grid, gtmp)
      integer,        intent(in)    :: nfour
      real(kind=dbl), intent(inout) :: grid(ndbl,4,0:*)
      real(kind=dbl), intent(out)   :: gtmp(ndbl,4,0:*)
    end subroutine grid_op_vgradT_vcurlv_sub
  end interface
  
end module ocean