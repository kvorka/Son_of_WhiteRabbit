submodule (fourier_transform) fxcp
  implicit none; contains
  
  module procedure fxcpy
    integer :: i0, i1, i2
    
    do i2 = 1, m
      !$omp simd
      do i0 = 1, ndbl
        arr_to(i0,1,i2) = arr_from(i0,1,i2)
        arr_to(i0,2,i2) = arr_from(i0,2,i2)
        arr_to(i0,3,i2) = arr_from(i0,3,i2)
        arr_to(i0,4,i2) = arr_from(i0,4,i2)
        arr_to(i0,5,i2) = arr_from(i0,5,i2)
        arr_to(i0,6,i2) = arr_from(i0,6,i2)
        arr_to(i0,7,i2) = arr_from(i0,7,i2)
        arr_to(i0,8,i2) = arr_from(i0,8,i2)
      end do
    end do
    
  end procedure fxcpy
  
end submodule fxcp