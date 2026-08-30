submodule (math) xy2ee
  implicit none; contains
  
  module procedure xy2ee_sub
    integer           :: i
    real(kind=dbl)    :: sq2_1 = 1 / sqrt(2._dbl)
    complex(kind=dbl) :: ci
    
    !$omp simd
    do i = 1, n
      cy(i) = cy(i) * cunit
      cx(i) = cx(i) * sq2_1
      
      ci    = +cx(i) + cy(i) * sq2_1
      cy(i) = -cx(i) + cy(i) * sq2_1
      cx(i) = ci
    end do
    
  end procedure xy2ee_sub
  
end submodule xy2ee