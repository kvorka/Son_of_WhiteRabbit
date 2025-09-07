module solution
  use math
  implicit none
  
  type, public :: T_solution
    integer                        :: nd, jmax, jms, jmv
    complex(kind=dbl), allocatable :: mech(:,:), temp(:,:), torr(:,:)
    
    contains
    
    procedure :: init_sub       => init_solution_sub
    procedure :: deallocate_sub => deallocate_solution_sub
    
    procedure :: init_stemp_sub
    procedure :: init_storr_sub
    procedure :: init_smech_sub

    procedure :: temp_fn, temp_jm_sub
    procedure :: velocity_fn, velocity_jml_sub
    
  end type T_solution
  
  interface
    module subroutine init_solution_sub(this, nd, jmax)
      class(T_solution), intent(inout) :: this
      integer,           intent(in)    :: nd, jmax
    end subroutine init_solution_sub
    
    module subroutine deallocate_solution_sub(this)
      class(T_solution), intent(inout) :: this
    end subroutine deallocate_solution_sub
    
    !! Interfaces :: temperature
    module subroutine init_stemp_sub(this)
      class(T_solution), intent(inout) :: this
    end subroutine init_stemp_sub
    
    module subroutine init_storr_sub(this)
      class(T_solution), intent(inout) :: this
    end subroutine init_storr_sub
    
    module subroutine init_smech_sub(this)
      class(T_solution), intent(inout) :: this
    end subroutine init_smech_sub
    
    module complex(kind=dbl) function temp_fn(this, ir, ijm)
      class(T_solution), intent(in) :: this
      integer,           intent(in) :: ir, ijm
    end function temp_fn
    
    module subroutine temp_jm_sub(this, ir, temp_jm)
      class(T_solution), intent(in)  :: this
      integer,           intent(in)  :: ir
      complex(kind=dbl), intent(out) :: temp_jm(*)
    end subroutine temp_jm_sub
    
    module complex(kind=dbl) function velocity_fn(this, ir, il, ijm)
      class(T_solution), intent(in) :: this
      integer,           intent(in) :: ir, il, ijm
    end function velocity_fn
    
    module subroutine velocity_jml_sub(this, ir, velocity_jml)
      class(T_solution), intent(in)  :: this
      integer,           intent(in)  :: ir
      complex(kind=dbl), intent(out) :: velocity_jml(*)
    end subroutine velocity_jml_sub
  end interface
  
end module solution