submodule (math) rescale
  implicit none; contains
  
  module procedure rescale_carray_sub
    integer :: i
    
    !$omp simd
    do i = 1, length
      arr(i) = arr(i) * fac
    end do
  
  end procedure rescale_carray_sub
  
end submodule rescale
