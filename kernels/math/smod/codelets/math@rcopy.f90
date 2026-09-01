submodule (math) rcopy
  implicit none; contains
  
  module procedure copy_rarray_sub
    integer :: i
    
    !$omp simd
    do i = 1, length
      arr_to(i) = arr_from(istart+i-1)
    end do
    
  end procedure copy_rarray_sub
  
end submodule rcopy