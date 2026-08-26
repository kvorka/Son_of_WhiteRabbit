submodule (lege_poly) bwd_rsc
  implicit none; contains
  
  module procedure bwd_rsc_sub
    integer :: i0, i1, i2, i3
    
    do i3 = 1, n
      do i2 = 1, 2
        do i1 = 1, 4
          !$omp simd
          do i0 = 1, ndbl
            sumN(i0,i1,i3,i2) = swork(i0,i1,i2,i3,2) * cosx(i0,i1) + swork(i0,i1,i2,i3,1)
            sumS(i0,i1,i3,i2) = swork(i0,i1,i2,i3,2) * cosx(i0,i1) - swork(i0,i1,i2,i3,1)
          end do
        end do
      end do
    end do
    
  end procedure bwd_rsc_sub
  
end submodule bwd_rsc