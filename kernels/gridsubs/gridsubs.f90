module gridsubs
  use math
  implicit none; public
  
  interface
    module subroutine grid_op_scvv_vcvxv_sub(nfour, grid, gtmp)
      integer,        intent(in)    :: nfour
      real(kind=dbl), intent(inout) :: grid(ndbl,4,0:*)
      real(kind=dbl), intent(out)   :: gtmp(ndbl,4,0:*)
    end subroutine grid_op_scvv_vcvxv_sub
    
    module subroutine gcopy_sub(n, arr_from, arr_to) bind(C, name="gcopy_c")
      integer, value, intent(in)  :: n
      real(kind=dbl), intent(in)  :: arr_from(*)
      real(kind=dbl), intent(out) :: arr_to(*)
    end subroutine gcopy_sub
  end interface
  
end module gridsubs