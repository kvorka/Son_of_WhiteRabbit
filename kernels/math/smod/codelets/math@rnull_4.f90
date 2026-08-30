submodule (math) rnull
  implicit none; contains
  
  module procedure zero_rarray_4_sub
    integer :: i
    
    !$omp simd
    do i = istart+1, length
      arr(i) = zero
    end do
    
  end procedure zero_rarray_4_sub
  
end submodule rnull