submodule (sphsvt) eee2xyz
  implicit none; contains
  
  module procedure eee2xyz_sub
    integer :: iv
    
    !$omp simd
    do iv = 1, n
      cc(1,iv) =         ( +sumPTP(iv,1) - sumPTP(iv,3) ) * sq2_1
      cc(2,iv) = cunit * ( -sumPTP(iv,1) - sumPTP(iv,3) ) * sq2_1
      cc(3,iv) =           +sumPTP(iv,2)
    end do
    
  end procedure eee2xyz_sub

end submodule eee2xyz
  