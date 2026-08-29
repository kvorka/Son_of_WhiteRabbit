submodule (lege_poly) bwd_idx2
  implicit none; contains
  
  module procedure bwd_idx2_sub
    integer        :: i
    real(kind=dbl) :: fac1, fac2
    
    fac1 = cff(1)
    fac2 = cff(2)
    
    !$omp simd
    do i = 1, n
      rcab(1,i) = fac1 * cab(i,1)%re + fac2 * cab(i,3)%re
      rcab(2,i) = fac1 * cab(i,1)%im + fac2 * cab(i,3)%im
    end do
    
  end procedure bwd_idx2_sub
  
end submodule bwd_idx2