submodule (lege_poly) fwd_rsc
  implicit none; contains
  
  module procedure fwd_rsc_sub
    integer        :: i0, i2, i3
    real(kind=dbl) :: w1, w2, c1, c2, sn1, sn2, ss1, ss2
    
    do i3 = 1, n
      do i2 = 1, 2
        !$omp simd
        do i0 = 1, ndbl
          w1 = w(i0,1)
          w2 = w(i0,2)
          
          c1 = w1 * cosx(i0,1)
          c2 = w2 * cosx(i0,2)
          
          sn1 = sumN(i0,1,i3,i2)
          sn2 = sumN(i0,2,i3,i2)
          ss1 = sumS(i0,1,i3,i2)
          ss2 = sumS(i0,2,i3,i2)
          
          swork(i0,1,i2,i3,1) = ( sn1 - ss1 ) * w1
          swork(i0,2,i2,i3,1) = ( sn2 - ss2 ) * w2
          swork(i0,1,i2,i3,2) = ( sn1 + ss1 ) * c1
          swork(i0,2,i2,i3,2) = ( sn2 + ss2 ) * c2
          
          w1 = w(i0,3)
          w2 = w(i0,4)
          
          c1 = w1 * cosx(i0,3)
          c2 = w2 * cosx(i0,4)
          
          sn1 = sumN(i0,3,i3,i2)
          sn2 = sumN(i0,4,i3,i2)
          ss1 = sumS(i0,3,i3,i2)
          ss2 = sumS(i0,4,i3,i2)
          
          swork(i0,3,i2,i3,1) = ( sn1 - ss1 ) * w1
          swork(i0,4,i2,i3,1) = ( sn2 - ss2 ) * w2
          swork(i0,3,i2,i3,2) = ( sn1 + ss1 ) * c1
          swork(i0,4,i2,i3,2) = ( sn2 + ss2 ) * c2
        end do
      end do
    end do
    
  end procedure fwd_rsc_sub
  
end submodule fwd_rsc