submodule (fourier_transform) fxad
  implicit none; contains
  
  module procedure fxaddsub
    integer        :: i0, i1, i2
    real(kind=dbl) :: add
    
    do i2 = 1, m
      do i1 = 1, 4
        !$omp simd
        do i0 = 1, ndbl
          add            = arr1(i0,i1,i2)
          arr1(i0,i1,i2) = arr1(i0,i1,i2) + arr2(i0,i1,i2)
          arr2(i0,i1,i2) = add            - arr2(i0,i1,i2)
        end do
      end do
    end do
    
  end procedure fxaddsub
  
end submodule fxad