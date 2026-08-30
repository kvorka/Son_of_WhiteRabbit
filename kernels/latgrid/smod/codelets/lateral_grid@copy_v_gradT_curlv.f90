submodule (lateral_grid) copy_v_gradT_curlv
  implicit none; contains
  
  module procedure copy_v_gradT_curlv_sub
    integer :: i
    
    !$omp simd
    do i = 1, n
      ca(1,1,i) = v(i,1)
      ca(2,1,i) = q(i,1)
      ca(3,1,i) = curlv(i,1)
      
      ca(1,2,i) = v(i,2)
      ca(2,2,i) = q(i,2)
      ca(3,2,i) = curlv(i,2)
      
      ca(1,3,i) = v(i,3)
      ca(2,3,i) = q(i,3)
      ca(3,3,i) = curlv(i,3)
    end do
    
  end procedure copy_v_gradT_curlv_sub
  
end submodule copy_v_graT_curlv