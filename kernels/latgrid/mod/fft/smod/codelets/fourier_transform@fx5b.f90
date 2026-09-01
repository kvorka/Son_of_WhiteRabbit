submodule (fourier_transform) fx5b
  implicit none; contains
  
  module procedure fxzm5b
    integer        :: i0, i1, i2, i3
    real(kind=dbl) :: x0re, x0im, x1re, x1im, x2re, x2im, x3re, x3im, x4re, x4im, x01, x02
    
    do i3 = 1, l/5
      do i2 = 1, m
        do i1 = 1, 4
          !$omp simd
          do i0 = 1, ndbl
            x01  = x(i0,i1,i2,1,i3,1)
            x02  = x(i0,i1,i2,2,i3,1)
            x0re = x(i0,i1,i2,1,i3,4)
            x0im = x(i0,i1,i2,2,i3,4)
            
            x4re = x01 - x0re
            x4im = x02 - x0im
            x1re = x01 + x0re
            x1im = x02 + x0im
            
            x2re = x(i0,i1,i2,1,i3,2)
            x2im = x(i0,i1,i2,2,i3,2)
            x0re = x(i0,i1,i2,1,i3,3)
            x0im = x(i0,i1,i2,2,i3,3)
            
            x3re = x2re - x0re
            x3im = x2im - x0im
            x01  = x2re + x0re
            x02  = x2im + x0im
            
            x2re = x4re + C53 * x3re
            x2im = x4im + C53 * x3im
            
            x3re = C53 * x4re - x3re
            x3im = C53 * x4im - x3im
            
            x4re = x1re + x01
            x4im = x1im + x02
            x1re = x1re - x01
            x1im = x1im - x02
            
            x0re = x(i0,i1,i2,1,i3,0)
            x0im = x(i0,i1,i2,2,i3,0)
            
            x01 = x0re - C51 * x4re
            x02 = x0im - C51 * x4im
            
            x1re = x01 - C52 * x1re
            x1im = x02 - C52 * x1im
            
            x0re = x0re + x4re
            x0im = x0im + x4im
            x01  = x01  + x01
            x02  = x02  + x02
            
            x01  = x01 - x1re
            x02  = x02 - x1im
            
            x3re = x1im + C54 * x3re
            x3im = x1re - C54 * x3im
            x2im = x01  - C54 * x2im
            x2re = x02  + C54 * x2re
            
            x(i0,i1,i2,1,i3,0) = x0re
            x(i0,i1,i2,2,i3,0) = x0im
            x(i0,i1,i2,1,i3,4) = x2im
            x(i0,i1,i2,2,i3,4) = x2re
            x(i0,i1,i2,1,i3,3) = x3im
            x(i0,i1,i2,2,i3,3) = x3re
            
            x1re = x1re + x1re
            x1im = x1im + x1im
            x01  = x01  + x01
            x02  = x02  + x02
            
            x1re = x1re - x3im
            x1im = x1im - x3re
            x01  = x01  - x2im
            x02  = x02  - x2re
            
            x(i0,i1,i2,1,i3,2) = x1re
            x(i0,i1,i2,2,i3,2) = x1im
            x(i0,i1,i2,1,i3,1) = x01
            x(i0,i1,i2,2,i3,1) = x02
          end do
        end do
      end do
    end do
    
  end procedure fxzm5b
  
end submodule fx5b
