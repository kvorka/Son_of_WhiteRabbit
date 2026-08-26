submodule (fourier_transform) fxsc
  implicit none; contains
  
  module procedure fxrsc
    integer :: i0, i2
    
    do i2 = 1, m
      !$omp simd
      do i0 = 1, ndbl
        arr(i0,1,i2) = fac * arr(i0,1,i2)
        arr(i0,2,i2) = fac * arr(i0,2,i2)
        arr(i0,3,i2) = fac * arr(i0,3,i2)
        arr(i0,4,i2) = fac * arr(i0,4,i2)
      end do
    end do
    
  end procedure fxrsc
  
end submodule fxsc