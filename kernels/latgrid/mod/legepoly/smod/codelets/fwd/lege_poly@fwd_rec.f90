submodule (lege_poly) fwd_rec
  implicit none; contains
  
  module procedure fwd_rec_sub
    integer        :: i0, i3, i4
    real(kind=dbl) :: cr1, cr2, cr3, cr4, p1, p2, p3, p4
    
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
        cr1 = zero
        cr2 = zero
        cr3 = zero
        cr4 = zero
        
        !$omp simd reduction (+:cr1,cr2,cr3,cr4)
        do i0 = 1, ndbl
          p1 = pmj(i0,1)
          p2 = pmj(i0,2)
          p3 = pmj(i0,3)
          p4 = pmj(i0,4)
          
          cr1 = cr1 + p1 * swork(i0,1,1,i3) + p2 * swork(i0,2,1,i3) + p3 * swork(i0,3,1,i3) + p4 * swork(i0,4,1,i3)
          cr2 = cr2 + p1 * swork(i0,1,2,i3) + p2 * swork(i0,2,2,i3) + p3 * swork(i0,3,2,i3) + p4 * swork(i0,4,2,i3)
          cr3 = cr3 + p1 * swork(i0,1,3,i3) + p2 * swork(i0,2,3,i3) + p3 * swork(i0,3,3,i3) + p4 * swork(i0,4,3,i3)
          cr4 = cr4 + p1 * swork(i0,1,4,i3) + p2 * swork(i0,2,4,i3) + p3 * swork(i0,3,4,i3) + p4 * swork(i0,4,4,i3)
        end do
        
        cr(1,i3,i4) = cr(1,i3,i4) + cr1
        cr(2,i3,i4) = cr(2,i3,i4) + cr2
        cr(3,i3,i4) = cr(3,i3,i4) + cr3
        cr(4,i3,i4) = cr(4,i3,i4) + cr4
      end do
    end do
    
  end procedure fwd_rec_sub
  
end submodule fwd_rec