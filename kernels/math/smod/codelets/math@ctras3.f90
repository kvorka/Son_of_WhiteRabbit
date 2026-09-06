submodule (math) ctras3
  implicit none; contains
  
  module procedure trshf_3_3_carray_sub
    integer :: i
    
    !$omp simd
    do i = 1, length
      arr_to(1,i) = arr_from(i,1)
      arr_to(2,i) = arr_from(i,3)
      arr_to(3,i) = arr_from(i,2)
    end do
    
  end procedure trshf_3_3_carray_sub
  
end submodule ctras3