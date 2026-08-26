submodule (math) ccopy1
  implicit none; contains
  
  module procedure copy1_carray_sub
    integer :: i
    
    !$omp simd
    do i = 1, length
      arr(i) = arr(i) * fac
    end do
  
  end procedure copy1_carray_sub
  
end submodule ccopy1