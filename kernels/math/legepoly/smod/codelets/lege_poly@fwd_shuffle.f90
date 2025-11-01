submodule (lege_poly) fwd_shuffle
  implicit none; contains
  
  module procedure fwd_shuffle_sub
    integer :: i1, i2
    
    do i2 = 1, n
      !$omp simd
      do i1 = 1, 16
        swork(i1,1,i2,1) = ( sumN(i1,i2,1) - sumS(i1,i2,1) ) * w(i1)
        swork(i1,2,i2,1) = ( sumN(i1,i2,2) - sumS(i1,i2,2) ) * w(i1)
        swork(i1,1,i2,2) = ( sumN(i1,i2,1) + sumS(i1,i2,1) ) * w(i1) * cosx(i1)
        swork(i1,2,i2,2) = ( sumN(i1,i2,2) + sumS(i1,i2,2) ) * w(i1) * cosx(i1)
      end do
    end do
    
  end procedure fwd_shuffle_sub
  
end submodule fwd_shuffle