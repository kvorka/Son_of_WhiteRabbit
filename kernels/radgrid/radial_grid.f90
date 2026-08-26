module radial_grid
  use math
  implicit none
  
  type, public :: T_radialGrid
    integer                     :: nd
    real(kind=dbl)              :: volume
    real(kind=dbl), allocatable :: r(:), rr(:)
    
    contains
    
    procedure, pass :: init_sub       => init_grid_sub
    procedure, pass :: deallocate_sub => deallocate_grid_sub
    
    procedure, pass :: d, c, dd, cc, drr, interpolation_sub, intV_fn
    
  end type T_radialGrid
  
  interface
    module subroutine init_grid_sub(this, nr, rd, ru)
      class(T_radialGrid), intent(inout) :: this
      integer,             intent(in)    :: nr
      real(kind=dbl),      intent(in)    :: rd, ru
    end subroutine init_grid_sub
    
    module subroutine deallocate_grid_sub(this)
      class(T_radialGrid), intent(inout) :: this
    end subroutine deallocate_grid_sub
    
    module real(kind=dbl) function d(this, i, p)
      class(T_radialGrid), intent(in) :: this
      integer,             intent(in) :: i, p
    end function d
    
    module real(kind=dbl) function c(this, i, p)
      class(T_radialGrid), intent(in) :: this
      integer,             intent(in) :: i, p
    end function c
    
    module real(kind=dbl) function dd(this, i, p)
      class(T_radialGrid), intent(in) :: this
      integer,             intent(in) :: i, p
    end function dd
    
    module real(kind=dbl) function cc(this, i, p)
      class(T_radialGrid), intent(in) :: this
      integer,             intent(in) :: i, p
    end function cc
    
    module real(kind=dbl) function drr(this, i, p)
      class(T_radialGrid), intent(in) :: this
      integer,             intent(in) :: i, p
    end function drr
    
    module function intV_fn(this, field) result(intV)
      class(T_radialGrid), intent(in) :: this
      real(kind=dbl),      intent(in) :: field(:)
      real(kind=dbl)                  :: intV
    end function intV_fn
    
    module subroutine interpolation_sub(this, jmdim, ir, field, nrdim1, jmdim1, rr1, field1)
      class(T_radialGrid), intent(in)  :: this
      integer,             intent(in)  :: ir, jmdim, nrdim1, jmdim1
      real(kind=dbl),      intent(in)  :: rr1(nrdim1)
      complex(kind=dbl),   intent(in)  :: field1(jmdim1,nrdim1)
      complex(kind=dbl),   intent(out) :: field(jmdim)
    end subroutine interpolation_sub
  end interface
  
end module radial_grid