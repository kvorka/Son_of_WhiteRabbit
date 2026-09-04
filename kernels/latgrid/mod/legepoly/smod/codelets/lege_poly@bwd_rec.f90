submodule (lege_poly) bwd_rec
  implicit none; contains
  
  module procedure bwd_rec_sub
    integer        :: i0, i3, i4
    real(kind=dbl) :: p1, p2, p3, p4, fac1, fac2, fac3, cc1, cc2, cc3, cc4
    
    do i4 = 1, nma
      fac1 = fmj(1,i4)
      fac2 = fmj(2,i4)
      fac3 = fmj(3,i4)
      
      !$omp simd
      do i0 = 1, ndbl
        p1 = fac3 * pmj1(i0,1)
        p2 = fac3 * pmj1(i0,2)
        p3 = fac3 * pmj1(i0,3)
        p4 = fac3 * pmj1(i0,4)
        
        pmj1(i0,1) = pmj(i0,1)
        pmj1(i0,2) = pmj(i0,2)
        pmj1(i0,3) = pmj(i0,3)
        pmj1(i0,4) = pmj(i0,4)
      
        pmj(i0,1)  = ( fac1 * cosx2(i0,1) - fac2 ) * pmj(i0,1) - p1
        pmj(i0,2)  = ( fac1 * cosx2(i0,2) - fac2 ) * pmj(i0,2) - p2
        pmj(i0,3)  = ( fac1 * cosx2(i0,3) - fac2 ) * pmj(i0,3) - p3
        pmj(i0,4)  = ( fac1 * cosx2(i0,4) - fac2 ) * pmj(i0,4) - p4
      end do
      
      do i3 = 1, n
          cc1 = cc(1,i3,i4)
          cc2 = cc(2,i3,i4)
          cc3 = cc(3,i3,i4)
          cc4 = cc(4,i3,i4)
          
          !$omp simd
          do i0 = 1, ndbl
            p1 = pmj(i0,1)
            p2 = pmj(i0,2)
            p3 = pmj(i0,3)
            p4 = pmj(i0,4)
            
            swork(i0,1,1,i3) = swork(i0,1,1,i3) + p1 * cc1
            swork(i0,2,1,i3) = swork(i0,2,1,i3) + p2 * cc1
            swork(i0,3,1,i3) = swork(i0,3,1,i3) + p3 * cc1
            swork(i0,4,1,i3) = swork(i0,4,1,i3) + p4 * cc1
            swork(i0,1,2,i3) = swork(i0,1,2,i3) + p1 * cc2
            swork(i0,2,2,i3) = swork(i0,2,2,i3) + p2 * cc2
            swork(i0,3,2,i3) = swork(i0,3,2,i3) + p3 * cc2
            swork(i0,4,2,i3) = swork(i0,4,2,i3) + p4 * cc2
            swork(i0,1,3,i3) = swork(i0,1,3,i3) + p1 * cc3
            swork(i0,2,3,i3) = swork(i0,2,3,i3) + p2 * cc3
            swork(i0,3,3,i3) = swork(i0,3,3,i3) + p3 * cc3
            swork(i0,4,3,i3) = swork(i0,4,3,i3) + p4 * cc3
            swork(i0,1,4,i3) = swork(i0,1,4,i3) + p1 * cc4
            swork(i0,2,4,i3) = swork(i0,2,4,i3) + p2 * cc4
            swork(i0,3,4,i3) = swork(i0,3,4,i3) + p3 * cc4
            swork(i0,4,4,i3) = swork(i0,4,4,i3) + p4 * cc4
          end do
      end do
    end do
    
  end procedure bwd_rec_sub
  
end submodule bwd_rec