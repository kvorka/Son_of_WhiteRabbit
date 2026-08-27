submodule (physicalobject) curl_ptp
  implicit none; contains
  
  module procedure curl_ptp_sub
    integer        :: ij, im, ij0
    real(kind=dbl) :: crr, cj1, cj2, cjr1, cjr2, cjr3, cjr4
    
    crr = 1 / this%rad_grid%rr(ir)
    
    !ij = 0
      !im = 0
        curlv(1,1) = czero
        curlv(1,2) = czero
        curlv(1,3) = czero
    
    do ij = 1, this%jmax
      cj1 = sqrt( (ij+1) / (2*ij+one) )
      cj2 = sqrt( (ij  ) / (2*ij+one) )
      
      cjr1 = (ij-1) * crr
      cjr2 = (ij  ) * crr
      cjr3 = (ij+1) * crr
      cjr4 = (ij+2) * crr
      
      ij0 = jm(ij,0)
      
      !$omp simd
      do im = 0, ij
        curlv(ij0+im,1) = cj1 * ( dv_dr(ij0+im,2) + cjr3 * v(ij0+im,2) )
        curlv(ij0+im,2) = cj1 * ( dv_dr(ij0+im,1) - cjr1 * v(ij0+im,1) ) + cj2 * ( dv_dr(ij0+im,3) + cjr4 * v(ij0+im,3) )
        curlv(ij0+im,3) =                                                  cj2 * ( dv_dr(ij0+im,2) - cjr2 * v(ij0+im,2) )
        
        curlv(ij0+im,1) = cunit * curlv(ij0+im,1)
        curlv(ij0+im,2) = cunit * curlv(ij0+im,2)
        curlv(ij0+im,3) = cunit * curlv(ij0+im,3)
      end do
    end do
    
  end procedure curl_ptp_sub
  
end submodule curl_ptp