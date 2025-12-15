module matrix
  !Original work on LU: Numerical Recipes in Fortran77
  use math
  implicit none
  
  type, public :: T_matrix
    integer                     :: n, ld, lu, ldu
    real(kind=dbl), allocatable :: M(:,:), U(:,:), L(:,:)
    integer,        allocatable :: I(:)
    
    contains
    
    procedure :: init_sub       => init_matrix_sub
    procedure :: fill_sub       => lu_decomposition_sub
    procedure :: luSolve_sub    => lu_solve_sub
    procedure :: addmultipl_sub => matrix_multiple_sub
    procedure :: deallocate_sub => deallocate_matrix_sub
    
  end type T_matrix
  
  interface
    module subroutine init_matrix_sub(this, n, ld, lu)
      class(T_matrix), intent(inout) :: this
      integer,         intent(in)    :: n, ld, lu
    end subroutine init_matrix_sub
    
    module subroutine deallocate_matrix_sub(this)
      class(T_matrix), intent(inout) :: this
    end subroutine deallocate_matrix_sub
    
    module subroutine lu_decomposition_sub(this, matrixU, matrixM)
      class(T_matrix), intent(inout) :: this
      real(kind=dbl),  intent(in)    :: matrixU(this%ldu,this%n), matrixM(this%ldu,this%n)
    end subroutine lu_decomposition_sub
    
    module subroutine lu_solve_sub(this, howmany, b)
      class(T_matrix),   intent(in)    :: this
      integer,           intent(in)    :: howmany
      complex(kind=dbl), intent(inout) :: b(0:howmany,*)
    end subroutine lu_solve_sub
    
    module subroutine matrix_multiple_sub(this, i, howmany, bin, bout)
      class(T_matrix),   intent(in)    :: this
      integer,           intent(in)    :: i, howmany
      complex(kind=dbl), intent(in)    :: bin(0:howmany,*)
      complex(kind=dbl), intent(inout) :: bout(0:howmany)
    end subroutine matrix_multiple_sub
  end interface
  
end module matrix