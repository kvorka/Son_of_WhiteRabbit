submodule (lege_poly) fwd_idx2
  implicit none; contains
  
  module procedure fwd_idx2_sub
    integer        :: i
    real(kind=dbl) :: fac1, fac2
    
    fac1 = cff(1)
    fac2 = cff(2)
    
    !$omp simd
    do i = 1, n
      cab(i)%re = fac1 * rcab(1,i,1) + fac2 * rcab(1,i,3)
      cab(i)%im = fac1 * rcab(2,i,1) + fac2 * rcab(2,i,3)
    end do
    
  end procedure fwd_idx2_sub
  
end submodule fwd_idx2