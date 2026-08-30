submodule (math) ctra3v
  implicit none; contains
  
  module procedure trshf_3_carray_sub
    integer :: i
    
    !$omp simd
    do i = 1, n
      ca(1,1,i) = v1(i,1)
      ca(2,1,i) = v2(i,1)
      ca(3,1,i) = v3(i,1)
      
      ca(1,2,i) = v1(i,2)
      ca(2,2,i) = v2(i,2)
      ca(3,2,i) = v3(i,2)
      
      ca(1,3,i) = v1(i,3)
      ca(2,3,i) = v2(i,3)
      ca(3,3,i) = v3(i,3)
    end do
    
  end procedure trshf_3_carray_sub
  
end submodule ctra3v