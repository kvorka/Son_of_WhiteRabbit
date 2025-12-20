submodule (sphbase) eee2xyz
  implicit none; contains
  
  module procedure eee2xyz_sub
    integer :: i
    
    !$omp simd
    do i = 0, n-1
      cc(3*i  ) =         ( +sum1(i) - sum2(i) ) * sq2_1
      cc(3*i+1) = cunit * ( -sum1(i) - sum2(i) ) * sq2_1
      cc(3*i+2) =           +sum3(i)
    end do
    
  end procedure eee2xyz_sub

end submodule eee2xyz
  