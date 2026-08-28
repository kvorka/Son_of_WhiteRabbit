submodule (lege_poly) fwd_idx
  implicit none; contains
  
  module procedure fwd_idx_sub
    integer        :: i
    real(kind=dbl) :: fac1, fac2
    
    fac1 = cff(1)
    fac2 = cff(2)
    
    !$omp simd
    do i = 1, n
      cab(i,1) = fac2 * cmplx( rcab(1,i,3), rcab(2,i,3), kind=dbl ) + &
               & fac1 * cmplx( rcab(1,i,1), rcab(2,i,1), kind=dbl )
      cab(i,2) =        cmplx( rcab(1,i,4), rcab(2,i,4), kind=dbl )
    end do
    
  end procedure fwd_idx_sub
  
end submodule fwd_idx