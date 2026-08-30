submodule (math) ctra4s
  implicit none; contains
  
  module procedure trans_4_carray_sub
    integer :: i1
    
    !$omp simd
    do i1 = 1, length
      arr_to(i1,1) = arr_from(1,i1)
      arr_to(i1,2) = arr_from(2,i1)
      arr_to(i1,3) = arr_from(3,i1)
      arr_to(i1,4) = arr_from(4,i1)
    end do
    
  end procedure trans_4_carray_sub
  
end submodule ctra4s