submodule (lege_poly) bwd_idx3
  implicit none; contains
  
  module procedure bwd_idx3_sub
    integer :: i
    
    !$omp simd
    do i = 1, n
      rcab(1,i,2) = cab(i)%re
      rcab(2,i,2) = cab(i)%im
    end do
    
  end procedure bwd_idx3_sub
  
end submodule bwd_idx3