submodule (equations) cswap
  implicit none; contains
  
  module procedure swap_carray_sub
    integer           :: i
    complex(kind=dbl) :: dum
    
    !$omp simd
    do i = 1, length
      dum     = arr1(i)
      arr1(i) = arr2(i)
      arr2(i) = dum
    end do
    
  end procedure swap_carray_sub
  
end submodule cswap