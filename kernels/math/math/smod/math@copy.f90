submodule (math) copy
  implicit none; contains
  
  module procedure copy_carray_sub
    integer :: i
    
    !$omp simd
    do i = 1, length
      arr_to(i) = arr_from(i)
    end do
    
  end procedure copy_carray_sub
  
end submodule copy