submodule (lege_poly) bwd_shuffle
  implicit none; contains
  
  module procedure bwd_shuffle_sub
    integer :: i1, i2
    
    do i2 = 1, n
      !$omp simd
      do i1 = 1, 16
        sumN(i1,i2,1) = swork(i1,1,i2,2) * cosx(i1) + swork(i1,1,i2,1)
        sumS(i1,i2,1) = swork(i1,1,i2,2) * cosx(i1) - swork(i1,1,i2,1)
        sumN(i1,i2,2) = swork(i1,2,i2,2) * cosx(i1) + swork(i1,2,i2,1)
        sumS(i1,i2,2) = swork(i1,2,i2,2) * cosx(i1) - swork(i1,2,i2,1)
      end do
    end do
    
  end procedure bwd_shuffle_sub

end submodule bwd_shuffle