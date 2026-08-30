submodule (math) eee2xyz
  implicit none; contains
  
  module procedure eee2xyz_sub
    integer        :: i
    real(kind=dbl) :: sq2_1 = 1 / sqrt(2._dbl)
    
    !$omp simd
    do i = 1, n
      cxyz(1,i) =         ( +sumPTP(i,1) - sumPTP(i,3) ) * sq2_1
      cxyz(2,i) = cunit * ( -sumPTP(i,1) - sumPTP(i,3) ) * sq2_1
      cxyz(3,i) =           +sumPTP(i,2)
    end do
    
  end procedure eee2xyz_sub

end submodule eee2xyz
  