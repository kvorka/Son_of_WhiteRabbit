submodule (physicalobject) curl_ptp_j
  implicit none; contains
  
  module procedure curl_ptp_j_sub
    integer :: i
    
    !$omp simd
    do i = 1, length
      curl1(i) = fac1 * ( darr2(i) + fac3 * arr2(i) )
      curl2(i) = fac1 * ( darr1(i) - fac2 * arr1(i) ) + fac4 * ( darr3(i) + fac6 * arr3(i) )
      curl3(i) =                                        fac4 * ( darr2(i) - fac5 * arr2(i) )
      
      curl1(i) = cunit * curl1(i)
      curl2(i) = cunit * curl2(i)
      curl3(i) = cunit * curl3(i)
    end do
    
  end procedure curl_ptp_j_sub
  
end submodule curl_ptp_j