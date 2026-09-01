submodule (fourier_transform) fx3a
  implicit none; contains
  
  module procedure fxzm3a
    integer        :: i0, i1, i2, i3, i4
    real(kind=dbl) :: t1re, t1im, t2re, t2im, x0re, x0im, x1re, x1im, x2re, x2im, r01, r02
    
    do i4 = 0, k-1
      i0 = 2 * i4
      
      t1re = t(1,i0  )
      t1im = t(2,i0  )
      t2re = t(1,i0+1)
      t2im = t(2,i0+1)
      
      do i3 = 1, l/3
        do i2 = 1, m
          do i1 = 1, 4
            !$omp simd
            do i0 = 1, ndbl
              x1re = x(i0,i1,i2,1,i3,1,i4)
              x1im = x(i0,i1,i2,2,i3,1,i4)
              x2re = x(i0,i1,i2,1,i3,2,i4)
              x2im = x(i0,i1,i2,2,i3,2,i4)
              
              r01 = t1re * x1re - t1im * x1im
              r02 = t1re * x1im + t1im * x1re
              
              x1re = r01 - t2re * x2re + t2im * x2im
              x1im = r02 - t2re * x2im - t2im * x2re
              
              x0re = x(i0,i1,i2,1,i3,0,i4)
              x0im = x(i0,i1,i2,2,i3,0,i4)
              
              r01 = 2 * r01 - x1re
              r02 = 2 * r02 - x1im
              
              x2re = x0re + C31 * r01
              x2im = x0im + C31 * r02
              
              x0re = r01 + x0re
              x0im = r02 + x0im
              x1im = x2re + C32 * x1im
              x1re = x2im - C32 * x1re
              
              x(i0,i1,i2,1,i3,0,i4) = x0re
              x(i0,i1,i2,2,i3,0,i4) = x0im
              x(i0,i1,i2,1,i3,2,i4) = x1im
              x(i0,i1,i2,2,i3,2,i4) = x1re
              
              x2re = 2 * x2re - x1im
              x2im = 2 * x2im - x1re
              
              x(i0,i1,i2,1,i3,1,i4) = x2re
              x(i0,i1,i2,2,i3,1,i4) = x2im
            end do
          end do
        end do
      end do
    end do
    
  end procedure fxzm3a
  
end submodule fx3a
