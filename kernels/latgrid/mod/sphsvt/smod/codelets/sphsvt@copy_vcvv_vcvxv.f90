submodule (sphsvt) copy_vgradT_vcurlv
  implicit none; contains
  
  module procedure copy_vgradT_vcurlv_sub
    integer :: i
    
    !$omp simd
    do i = 1, n
      ca(1,i) = v(i,1)
      ca(2,i) = q(i,1)
      ca(3,i) = curlv(i,1)
      
      ca(4,i) = v(i,2)
      ca(5,i) = q(i,2)
      ca(6,i) = curlv(i,2)
      
      ca(7,i) = v(i,3)
      ca(8,i) = q(i,3)
      ca(9,i) = curlv(i,3)
    end do
    
  end procedure copy_vgradT_vcurlv_sub
  
end submodule copy_vgradT_vcurlv