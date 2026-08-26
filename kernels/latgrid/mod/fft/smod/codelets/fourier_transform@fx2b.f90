submodule (fourier_transform) fx2b
  implicit none; contains
  
  module procedure fxzm2b
    integer :: i0, i1, i2, i3
    
    do i3 = 1, l/2
      do i2 = 1, m
        do i1 = 1, 4
        !$omp simd
          do i0 = 1, ndbl
            x(i0,i1,i2,1,i3,1) =     x(i0,i1,i2,1,i3,0) - x(i0,i1,i2,1,i3,1)
            x(i0,i1,i2,2,i3,1) =     x(i0,i1,i2,2,i3,0) - x(i0,i1,i2,2,i3,1)
            x(i0,i1,i2,1,i3,0) = 2 * x(i0,i1,i2,1,i3,0) - x(i0,i1,i2,1,i3,1)
            x(i0,i1,i2,2,i3,0) = 2 * x(i0,i1,i2,2,i3,0) - x(i0,i1,i2,2,i3,1)
          end do
        end do
      end do
    end do
    
  end procedure fxzm2b
  
end submodule fx2b
