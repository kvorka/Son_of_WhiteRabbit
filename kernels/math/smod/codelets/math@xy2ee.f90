submodule (math) xy2ee
  implicit none; contains
  
  module procedure xy2ee_sub
    real(kind=dbl), parameter :: fac = 0.7071067811865475_dbl
    integer                   :: i
    complex(kind=dbl)         :: ci
    
    !$omp simd
    do i = 1, n
      cy(i) = cy(i) * cunit
      cx(i) = cx(i) * fac
      
      ci    = +cx(i) + cy(i) * fac
      cy(i) = -cx(i) + cy(i) * fac
      cx(i) = ci
    end do
    
  end procedure xy2ee_sub
  
end submodule xy2ee