module equations
  use math
  implicit none
  
  type, public :: T_equations
    integer                        :: mm, n, ld, lu, ldu
    integer,           allocatable :: I(:)
    real(kind=dbl),    allocatable :: M(:,:), U(:,:), L(:,:)
    complex(kind=dbl), allocatable :: sol(:,:), rhs1(:,:), rhs2(:,:)
    
    contains
    
    procedure, pass :: init_sub       => init_equations_sub
    procedure, pass :: deallocate_sub => deallocate_equations_sub
    
    procedure, pass :: fill_sub       => lu_decomposition_sub
    procedure, pass :: luSolve_sub    => lu_solve_sub
    procedure, pass :: addmultipl_sub => matrix_multiple_sub

  end type T_equations
  
  interface
    module subroutine init_equations_sub(this, mm, nvar, nrhs, ld, lu, def_rhs2)
      class(T_equations), intent(inout) :: this
      integer,            intent(in)    :: mm, nvar, nrhs, ld, lu
      logical,            intent(in)    :: def_rhs2
    end subroutine init_equations_sub
    
    module subroutine deallocate_equations_sub(this)
      class(T_equations), intent(inout) :: this
    end subroutine deallocate_equations_sub
    
    module subroutine lu_decomposition_sub(this, matrixU, matrixM)
      class(T_equations), intent(inout) :: this
      real(kind=dbl),     intent(in)    :: matrixU(this%ldu,this%n), matrixM(this%ldu,this%n)
    end subroutine lu_decomposition_sub
    
    module subroutine lu_solve_sub(this)
      class(T_equations), intent(inout) :: this
    end subroutine lu_solve_sub
    
    module subroutine matrix_multiple_sub(this, i, bout)
      class(T_equations), intent(in)    :: this
      integer,            intent(in)    :: i
      complex(kind=dbl),  intent(inout) :: bout(0:this%mm)
    end subroutine matrix_multiple_sub
  end interface

end module equations
