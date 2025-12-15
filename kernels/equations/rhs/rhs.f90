module rhs
  use spharray
  implicit none
  
  type, public :: T_rhs
    integer                        :: nd, jmax
    type(T_spharray), allocatable :: temp(:), torr(:), sph1(:), sph2(:)
    
    contains
    
    procedure :: init_sub       => init_rhs_sub
    procedure :: deallocate_sub => deallocate_rhs_sub
    
    procedure :: init_rtemp_sub
    procedure :: init_rtorr_sub
    procedure :: init_rmech_sub
    
  end type T_rhs
  
  interface
    module subroutine init_rhs_sub(this, nd, jmax)
      class(T_rhs), intent(inout) :: this
      integer,      intent(in)    :: nd, jmax
    end subroutine init_rhs_sub
    
    module subroutine deallocate_rhs_sub(this)
      class(T_rhs), intent(inout) :: this
    end subroutine deallocate_rhs_sub
    
    module subroutine init_rtemp_sub(this)
      class(T_rhs), intent(inout) :: this
    end subroutine init_rtemp_sub
    
    module subroutine init_rtorr_sub(this)
      class(T_rhs), intent(inout) :: this
    end subroutine init_rtorr_sub
    
    module subroutine init_rmech_sub(this)
      class(T_rhs), intent(inout) :: this
    end subroutine init_rmech_sub
  end interface
  
end module rhs