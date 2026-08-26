submodule (physicalobject) ccopy4
  implicit none; contains
  
  module procedure copy4_carray_sub
    integer :: i
    
    !$omp simd
    do i = 1, length
      arr_to(i) = fac1 * arr1(i) + fac2 * arr2(i) + fac3 * arr_to(i)
    end do
    
  end procedure copy4_carray_sub
  
end submodule ccopy4