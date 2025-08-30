module matrices
  use matrix
  implicit none
  
  type, public :: T_matrices
    class(T_matrix), allocatable :: temp(:), torr(:), mech(:)
    integer                      :: nd, jmax
    
    contains
    
    procedure :: init_sub       => init_matrices_sub
    procedure :: deallocate_sub => deallocate_matrices_sub
    
    procedure :: init_mtemp_sub
    procedure :: init_mtorr_sub
    procedure :: init_mmech_sub
    
  end type T_matrices
  
  interface
    module pure subroutine init_matrices_sub(this, nd, jmax)
      class(T_matrices), intent(inout) :: this
      integer,           intent(in)    :: nd, jmax
    end subroutine init_matrices_sub
    
    module subroutine deallocate_matrices_sub(this)
      class(T_matrices), intent(inout) :: this
    end subroutine deallocate_matrices_sub

    module pure subroutine init_mtemp_sub(this)
      class(T_matrices), intent(inout) :: this
    end subroutine init_mtemp_sub
    
    module pure subroutine init_mtorr_sub(this)
      class(T_matrices), intent(inout) :: this
    end subroutine init_mtorr_sub
    
    module pure subroutine init_mmech_sub(this)
      class(T_matrices), intent(inout) :: this
    end subroutine init_mmech_sub
  end interface
  
end module matrices