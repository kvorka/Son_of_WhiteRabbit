submodule (lege_poly) fwd_rsc
  implicit none; contains
  
  module procedure fwd_rsc_sub
    integer :: i0, i1, i2, i3
    
    do i3 = 1, n
      do i2 = 1, 2
        do i1 = 1, 4
          !$omp simd
          do i0 = 1, ndbl
            swork(i0,i1,i2,i3,1) = ( sumN(i0,i1,i3,i2) - sumS(i0,i1,i3,i2) ) * w(i0,i1)
            swork(i0,i1,i2,i3,2) = ( sumN(i0,i1,i3,i2) + sumS(i0,i1,i3,i2) ) * w(i0,i1) * cosx(i0,i1)
          end do
        end do
      end do
    end do
    
  end procedure fwd_rsc_sub
  
end submodule fwd_rsc