module gridsubs
  use math
  implicit none; public
  
  interface
    module subroutine grid_op_scvv_vcvxv_sub(nfour, grid, gtmp)
      integer,        intent(in)    :: nfour
      real(kind=dbl), intent(inout) :: grid(0:*)
      real(kind=dbl), intent(out)   :: gtmp(0:*)
    end subroutine grid_op_scvv_vcvxv_sub
    
    module subroutine scvv_vcvxv_sub(gin, gout, gtmp) bind(C, name="scvv_vcvxv_c")
      real(kind=dbl), intent(in)  :: gin(*)
      real(kind=dbl), intent(out) :: gtmp(*), gout(*)
    end subroutine scvv_vcvxv_sub
  end interface
  
end module gridsubs