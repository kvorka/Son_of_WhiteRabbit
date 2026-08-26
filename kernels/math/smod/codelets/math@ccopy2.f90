submodule (math) ccopy2
  implicit none; contains
  
  module procedure copy2_carray_sub
    integer :: i
    
    !$omp simd
    do i = 1, length
      arr_to(i) = fac * arr_from(i)
    end do
    
  end procedure copy2_carray_sub
  
end submodule ccopy2
