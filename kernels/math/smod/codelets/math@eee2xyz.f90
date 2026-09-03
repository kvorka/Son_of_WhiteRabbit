submodule (math) eee2xyz
  implicit none; contains
  
  module procedure eee2xyz_sub
    real(kind=dbl), parameter :: fac = 0.7071067811865475_dbl
    integer                   :: i
    
    !$omp simd
    do i = 1, n
      cxyz(1,i) =          ( +sumPTP(i,1) - sumPTP(i,3) ) * fac
      cxyz(2,i) = -cunit * ( +sumPTP(i,1) + sumPTP(i,3) ) * fac
      cxyz(3,i) =            +sumPTP(i,2)
    end do
    
  end procedure eee2xyz_sub

end submodule eee2xyz
  