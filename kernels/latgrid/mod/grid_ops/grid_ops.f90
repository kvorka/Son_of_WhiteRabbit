module grid_ops
  use math
  implicit none
  
  interface
    module subroutine grid_op_vcvv_vcvxv_sub(nfour, grid, gtmp)
      integer,        intent(in)    :: nfour
      real(kind=dbl), intent(inout) :: grid(ndbl,4,0:*)
      real(kind=dbl), intent(out)   :: gtmp(ndbl,4,0:*)
    end subroutine grid_op_vcvv_vcvxv_sub
  end interface
  
  interface
#if defined ( kernelC )
    module subroutine tempcpy_sub(n, arr_from, arr_to) bind(C, name="tempcpy_c")
      integer, value, intent(in)  :: n
      real(kind=dbl), intent(in)  :: arr_from(*)
      real(kind=dbl), intent(out) :: arr_to(*)
    end subroutine tempcpy_sub
#else
    module subroutine tempcpy_sub(n, arr_from, arr_to)
      integer,        intent(in)  :: n
      real(kind=dbl), intent(in)  :: arr_from(ndbl,4,n)
      real(kind=dbl), intent(out) :: arr_to(ndbl,4,n)
    end subroutine tempcpy_sub
#endif
  end interface
  
end module grid_ops