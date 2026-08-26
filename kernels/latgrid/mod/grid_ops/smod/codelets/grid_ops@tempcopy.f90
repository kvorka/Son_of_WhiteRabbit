submodule (grid_ops) tempcopy
  implicit none; contains
  
  module procedure tempcpy_sub
    integer :: i0, i2
    
    do i2 = 1, n
      !$omp simd
      do i0 = 1, ndbl
        arr_to(i0,1,i2) = arr_from(i0,1,i2)
        arr_to(i0,2,i2) = arr_from(i0,2,i2)
        arr_to(i0,3,i2) = arr_from(i0,3,i2)
        arr_to(i0,4,i2) = arr_from(i0,4,i2)
      end do
    end do
    
  end procedure tempcpy_sub
  
end submodule tempcopy