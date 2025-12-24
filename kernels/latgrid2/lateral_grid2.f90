module lateral_grid2
  use math
  use sph
  use sphsvt2
  implicit none
  
  type, public :: T_lateralGrid2
    integer         :: nL, nF, jmax, jms
    type(c_ptr)     :: shtns_c
    type(T_sphsvt2) :: rxd
    
    contains
    
    procedure, pass :: init_sub        => init_shtns_sub
    procedure, pass :: alloc_grid_sub  => alloc_grid_shtns_sub
    procedure, pass :: jm_to_grid_sub  => jm_to_grid_shtns_sub
    procedure, pass :: grid_to_jm_sub  => grid_to_jm_shtns_sub
    procedure, pass :: jml_to_grid_sub => jml_to_grid_shtns_sub
    procedure, pass :: grid_to_jml_sub => grid_to_jml_shtns_sub
    
  end type T_lateralGrid2
  
  interface
    module subroutine init_shtns_sub(this, jmax)
      class(T_lateralGrid2), intent(inout) :: this
      integer,               intent(in)    :: jmax
    end subroutine init_shtns_sub
    
    module subroutine alloc_grid_shtns_sub(this, grid_c, grid)
      class(T_lateralGrid2),   intent(in) :: this
      type(c_ptr),             intent(out) :: grid_c
      real(kind=dbl), pointer, intent(out) :: grid(:)
    end subroutine alloc_grid_shtns_sub
    
    module subroutine jm_to_grid_shtns_sub(this, scal, grid)
      class(T_lateralGrid2), intent(in)    :: this
      complex(kind=dbl),     intent(inout) :: scal(*)
      real(kind=dbl),        intent(out)   :: grid(*)
    end subroutine jm_to_grid_shtns_sub
    
    module subroutine grid_to_jm_shtns_sub(this, grid, scal)
      class(T_lateralGrid2), intent(in)    :: this
      complex(kind=dbl),     intent(out)   :: scal(*)
      real(kind=dbl),        intent(inout) :: grid(*)
    end subroutine grid_to_jm_shtns_sub
    
    module subroutine jml_to_grid_shtns_sub(this, pol1, torr, pol2, rgrid, tgrid, pgrid)
      class(T_lateralGrid2), intent(in)    :: this
      complex(kind=dbl),     intent(inout) :: pol1(*), torr(*), pol2(*)
      real(kind=dbl),        intent(out)   :: rgrid(*), tgrid(*), pgrid(*)
    end subroutine jml_to_grid_shtns_sub
    
    module subroutine grid_to_jml_shtns_sub(this, rgrid, tgrid, pgrid, pol1, torr, pol2)
      class(T_lateralGrid2), intent(in)    :: this
      complex(kind=dbl),     intent(out)   :: pol1(*), torr(*), pol2(*)
      real(kind=dbl),        intent(inout) :: rgrid(*), tgrid(*), pgrid(*)
    end subroutine grid_to_jml_shtns_sub
  end interface
  
end module lateral_grid2