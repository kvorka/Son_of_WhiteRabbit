submodule (lege_poly) fwd_sum
  implicit none; contains
  
  module procedure fwd_sum_sub
    integer :: i1, i2
    
    do i2 = 1, n
      !$omp simd
      do i1 = 1, 16
        cr(1,i2) = cr(1,i2) + pmj(i1) * swork(i1,1,i2)
        cr(2,i2) = cr(2,i2) + pmj(i1) * swork(i1,2,i2)
        cr(3,i2) = cr(3,i2) + pmj(i1) * swork(i1,3,i2)
        cr(4,i2) = cr(4,i2) + pmj(i1) * swork(i1,4,i2)
      end do
    end do
    
  end procedure fwd_sum_sub
  
end submodule fwd_sum
