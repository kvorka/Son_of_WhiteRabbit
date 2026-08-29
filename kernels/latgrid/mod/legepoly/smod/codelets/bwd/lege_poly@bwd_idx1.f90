submodule (lege_poly) bwd_idx1
  implicit none; contains
  
  module procedure bwd_idx1_sub
    integer :: i
    
    !$omp simd
    do i = 1, n
      rcab(1,i) = cff * cab(i)%re
      rcab(2,i) = cff * cab(i)%im
    end do
    
  end procedure bwd_idx1_sub
  
end submodule bwd_idx1