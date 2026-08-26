submodule (lege_poly) bwd_rec
  implicit none; contains
  
  module procedure bwd_rec_sub
    integer        :: i0, i2, i3, i4
    real(kind=dbl) :: p1, p2, p3, p4
    
    do i4 = 1, nma
      !$omp simd
      do i0 = 1, ndbl
        p1 = fmj(3,i4) * pmj1(i0,1)
        p2 = fmj(3,i4) * pmj1(i0,2)
        p3 = fmj(3,i4) * pmj1(i0,3)
        p4 = fmj(3,i4) * pmj1(i0,4)
        
        pmj1(i0,1) = pmj(i0,1)
        pmj1(i0,2) = pmj(i0,2)
        pmj1(i0,3) = pmj(i0,3)
        pmj1(i0,4) = pmj(i0,4)
      
        pmj(i0,1)  = ( fmj(1,i4) * cosx2(i0,1) - fmj(2,i4) ) * pmj(i0,1) - p1
        pmj(i0,2)  = ( fmj(1,i4) * cosx2(i0,2) - fmj(2,i4) ) * pmj(i0,2) - p2
        pmj(i0,3)  = ( fmj(1,i4) * cosx2(i0,3) - fmj(2,i4) ) * pmj(i0,3) - p3
        pmj(i0,4)  = ( fmj(1,i4) * cosx2(i0,4) - fmj(2,i4) ) * pmj(i0,4) - p4
      end do
      
      do i3 = 1, n
        do i2 = 1, 4
          !$omp simd
          do i0 = 1, ndbl
            swork(i0,1,i2,i3) = swork(i0,1,i2,i3) + pmj(i0,1) * cc(i2,i3,i4)
            swork(i0,2,i2,i3) = swork(i0,2,i2,i3) + pmj(i0,2) * cc(i2,i3,i4)
            swork(i0,3,i2,i3) = swork(i0,3,i2,i3) + pmj(i0,3) * cc(i2,i3,i4)
            swork(i0,4,i2,i3) = swork(i0,4,i2,i3) + pmj(i0,4) * cc(i2,i3,i4)
          end do
        end do
      end do
    end do
    
  end procedure bwd_rec_sub
  
end submodule bwd_rec