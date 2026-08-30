submodule (sphsvt) cadj3
  implicit none; contains
  
  module procedure cadj3_carray_sub
    integer :: i
    
    !$omp simd
    do i = 1, n
      arr_to(i) = arr_to(i) + fac * conjg( arr_from(i) )
    end do
    
  end procedure cadj3_carray_sub
  
end submodule cadj3