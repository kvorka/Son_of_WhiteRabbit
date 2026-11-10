module equations
  use math
  implicit none
  
  type, public :: T_equations
    integer                        :: mm, mm1, n, ld, lu, ldu
    integer,           allocatable :: I(:)
    real(kind=dbl),    allocatable :: M(:,:), U(:,:), L(:,:)
    complex(kind=dbl), allocatable :: sol(:,:), rhs1(:,:), rhs2(:,:), nlm1(:,:), nlm2(:,:)
    
    contains
    
    procedure, pass :: init_sub       => init_equations_sub
    procedure, pass :: deallocate_sub => deallocate_equations_sub
    
    procedure, pass :: luDecompose_sub  => lu_decomposition_sub
    procedure, pass :: luSolve_sub      => lu_solve_sub
    procedure, pass :: addmultipl1_sub  => matrix_multiple1_sub
    procedure, pass :: addmultipl2_sub  => matrix_multiple2_sub
    procedure, pass :: fill_nlm1_sub    => fill_nlm1_sub
    procedure, pass :: fill_nlm2_sub    => fill_nlm2_sub
    procedure, pass :: copy2_nlm1_sub   => copy2_nlm1_sub
    procedure, pass :: copy2_nlm2_sub   => copy2_nlm2_sub
    procedure, pass :: copy3_nlm1_sub   => copy3_nlm1_sub
    procedure, pass :: copy3_nlm2_sub   => copy3_nlm2_sub
    
  end type T_equations
  
  interface
    module subroutine init_equations_sub(this, mm, nvar, nrhs, ld, lu, def_rhs2, def_nlm2)
      class(T_equations), intent(inout) :: this
      integer,            intent(in)    :: mm, nvar, nrhs, ld, lu
      logical,            intent(in)    :: def_rhs2, def_nlm2
    end subroutine init_equations_sub
    
    module subroutine deallocate_equations_sub(this)
      class(T_equations), intent(inout) :: this
    end subroutine deallocate_equations_sub
    
    module subroutine lu_decomposition_sub(this)
      class(T_equations), intent(inout) :: this
    end subroutine lu_decomposition_sub
    
    module subroutine lu_solve_sub(this)
      class(T_equations), intent(inout) :: this
    end subroutine lu_solve_sub
    
    module subroutine matrix_multiple1_sub(this, isol, irhs)
      class(T_equations), intent(inout) :: this
      integer,            intent(in)    :: isol, irhs
    end subroutine matrix_multiple1_sub
    
    module subroutine matrix_multiple2_sub(this, isol, irhs)
      class(T_equations), intent(inout) :: this
      integer,            intent(in)    :: isol, irhs
    end subroutine matrix_multiple2_sub
    
    module subroutine fill_nlm1_sub(this, ir, arr)
      class(T_equations), intent(inout) :: this
      integer,            intent(in)    :: ir
      complex(kind=dbl),  intent(in)    :: arr(*)
    end subroutine fill_nlm1_sub
    
    module subroutine fill_nlm2_sub(this, ir, arr)
      class(T_equations), intent(inout) :: this
      integer,            intent(in)    :: ir
      complex(kind=dbl),  intent(in)    :: arr(*)
    end subroutine fill_nlm2_sub
    
    module subroutine copy2_nlm1_sub(this, n, fac)
      class(T_equations), intent(inout) :: this
      integer,            intent(in)    :: n
      real(kind=dbl),     intent(in)    :: fac
    end subroutine copy2_nlm1_sub
    
    module subroutine copy2_nlm2_sub(this, n, fac)
      class(T_equations), intent(inout) :: this
      integer,            intent(in)    :: n
      real(kind=dbl),     intent(in)    :: fac
    end subroutine copy2_nlm2_sub
    
    module subroutine copy3_nlm1_sub(this, n, fac)
      class(T_equations), intent(inout) :: this
      integer,            intent(in)    :: n
      real(kind=dbl),     intent(in)    :: fac
    end subroutine copy3_nlm1_sub
    
    module subroutine copy3_nlm2_sub(this, n, fac)
      class(T_equations), intent(inout) :: this
      integer,            intent(in)    :: n
      real(kind=dbl),     intent(in)    :: fac
    end subroutine copy3_nlm2_sub
  end interface
  
end module equations
