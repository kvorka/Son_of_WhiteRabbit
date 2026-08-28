submodule (math) ctrans
  implicit none; contains
  
  module procedure trans_carray_sub
    integer :: i1, i2
    
    do i2 = 1, length2
      !$omp simd
      do i1 = 1, length1
        arr_to(i1,i2) = arr_from(i2,i1)
      end do
    end do
    
  end procedure trans_carray_sub
  
end submodule ctrans