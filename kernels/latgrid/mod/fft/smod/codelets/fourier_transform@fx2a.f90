submodule (fourier_transform) fx2a
  implicit none; contains
  
  module procedure fxzm2a
    integer        :: i0, i1, i2, i3, i4
    real(kind=dbl) :: x0re, x0im, t1re, t1im
    
    do i4 = 0, k-1
      t1re = t(1,i4)
      t1im = t(2,i4)
      
      do i3 = 1, l/2
        do i2 = 1, m
          do i1 = 1, 4
            !$omp simd
            do i0 = 1, ndbl
              x0re = x(i0,i1,i2,1,i3,0,i4) - t1re * x(i0,i1,i2,1,i3,1,i4) + t1im * x(i0,i1,i2,2,i3,1,i4)
              x0im = x(i0,i1,i2,2,i3,0,i4) - t1im * x(i0,i1,i2,1,i3,1,i4) - t1re * x(i0,i1,i2,2,i3,1,i4)
              
              x(i0,i1,i2,1,i3,1,i4) = x0re
              x(i0,i1,i2,2,i3,1,i4) = x0im
              x(i0,i1,i2,1,i3,0,i4) = 2 * x(i0,i1,i2,1,i3,0,i4) - x0re
              x(i0,i1,i2,2,i3,0,i4) = 2 * x(i0,i1,i2,2,i3,0,i4) - x0im
            end do
          end do
        end do
      end do
    end do
    
  end procedure fxzm2a
  
end submodule fx2a