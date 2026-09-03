submodule (math) ctra3v
  implicit none; contains
  
  module procedure trshf_3_carray_sub
    integer :: i
    
    !$omp simd
    do i = 1, length
      ca(1,i) = v1(i,1)
      ca(2,i) = v2(i,1)
      ca(3,i) = v3(i,1)
      ca(4,i) = v1(i,2)
      ca(5,i) = v2(i,2)
      ca(6,i) = v3(i,2)
      ca(7,i) = v1(i,3)
      ca(8,i) = v2(i,3)
      ca(9,i) = v3(i,3)
    end do
    
  end procedure trshf_3_carray_sub
  
end submodule ctra3v