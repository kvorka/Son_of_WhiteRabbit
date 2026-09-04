submodule (lege_poly) bwd_rsc
  implicit none; contains
  
  module procedure bwd_rsc_sub
    integer        :: i0, i2, i3
    real(kind=dbl) :: s11, s21, s12, s22, cx1, cx2, cx3, cx4
    
    do i3 = 1, n
      do i2 = 1, 2
        !$omp simd
        do i0 = 1, ndbl
          cx1 = cosx(i0,1)
          cx2 = cosx(i0,2)
          cx3 = cosx(i0,3)
          cx4 = cosx(i0,4)
          
          s11 = swork(i0,1,i2,i3,1)
          s12 = swork(i0,2,i2,i3,1)
          s21 = swork(i0,1,i2,i3,2)
          s22 = swork(i0,2,i2,i3,2)
          
          sumN(i0,1,i3,i2) = s21 * cx1 + s11
          sumN(i0,2,i3,i2) = s22 * cx2 + s12
          sumS(i0,1,i3,i2) = s21 * cx1 - s11
          sumS(i0,2,i3,i2) = s22 * cx2 - s12
          
          s11 = swork(i0,3,i2,i3,1)
          s12 = swork(i0,4,i2,i3,1)
          s21 = swork(i0,3,i2,i3,2)
          s22 = swork(i0,4,i2,i3,2)
          
          sumN(i0,3,i3,i2) = s21 * cx3 + s11
          sumN(i0,4,i3,i2) = s22 * cx4 + s12
          sumS(i0,3,i3,i2) = s21 * cx3 - s11
          sumS(i0,4,i3,i2) = s22 * cx4 - s12
        end do
      end do
    end do
    
  end procedure bwd_rsc_sub
  
end submodule bwd_rsc