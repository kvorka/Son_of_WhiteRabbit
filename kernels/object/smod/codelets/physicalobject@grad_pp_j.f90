submodule (physicalobject) grad_pp_j
  implicit none; contains
  
  module procedure grad_pp_j_sub
    integer :: i
    
    !$omp simd
    do i = 1, length
      grad1(i) = fac1 * ( darr(i) + fac2 * arr(i) )
      grad3(i) = fac3 * ( darr(i) + fac4 * arr(i) )
    end do
    
  end procedure grad_pp_j_sub
  
end submodule grad_pp_j