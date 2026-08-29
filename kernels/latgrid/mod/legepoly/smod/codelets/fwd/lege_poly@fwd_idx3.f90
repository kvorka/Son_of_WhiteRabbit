submodule (lege_poly) fwd_idx3
  implicit none; contains
  
  module procedure fwd_idx3_sub
    integer :: i
    
    !$omp simd
    do i = 1, n
      cab(i)%re = rcab(1,i,2)
      cab(i)%im = rcab(2,i,2)
    end do
    
  end procedure fwd_idx3_sub
  
end submodule fwd_idx3