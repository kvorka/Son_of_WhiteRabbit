module sphbase
  use math
  implicit none
  
  interface
    module subroutine eee2xyz_sub(n, sum1, sum2, sum3, cc)
      integer,           intent(in)  :: n
      complex(kind=dbl), intent(in)  :: sum1(0:*), sum2(0:*), sum3(0:*)
      complex(kind=dbl), intent(out) :: cc(0:*)
    end subroutine eee2xyz_sub
  end interface
  
  
end module sphbase