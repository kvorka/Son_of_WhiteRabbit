submodule (math) xy2ee
  implicit none; contains
  
  module procedure xy2ee_sub
    real(kind=dbl), parameter :: fac = 0.7071067811865475_dbl
    integer                   :: i
    complex(kind=dbl)         :: cix, ciy
    
    !$omp simd
    do i = 1, length
      cy(i) = cy(i) * cunit
      cx(i) = cx(i) * fac
      
      cix = fac * cy(i) + cx(i)
      ciy = fac * cy(i) - cx(i)
      
      cx(i) = cix
      cy(i) = ciy
    end do
    
  end procedure xy2ee_sub
  
end submodule xy2ee