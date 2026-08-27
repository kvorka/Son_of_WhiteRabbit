submodule (physicalobject) curl_ptp
  implicit none; contains
  
  module procedure curl_ptp_sub
    integer        :: ij, ij0
    real(kind=dbl) :: crr, cj1, cj2, cjr1, cjr2, cjr3, cjr4
    
    crr = 1 / this%rad_grid%rr(ir)
    
    !ij = 0
      !im = 0
        curlv(1,1) = czero
        curlv(1,2) = czero
        curlv(1,3) = czero
    
    do ij = 1, this%jmax
      cj1 = sqrt( ( ij+1 ) / ( 2*ij + one ) )
      cj2 = sqrt( ( ij   ) / ( 2*ij + one ) )
      
      cjr1 = (ij-1) * crr
      cjr2 = (ij  ) * crr
      cjr3 = (ij+1) * crr
      cjr4 = (ij+2) * crr
      
      ij0 = jm(ij,0)
      
      call curl_ptp_j_sub( ij+1, cj1, cjr1, cjr3, cj2, cjr2, cjr4, &
                         & dv_dr(ij0,1), dv_dr(ij0,2), dv_dr(ij0,3), v(ij0,1), v(ij0,2), v(ij0,3), &
                         & curlv(ij0,1), curlv(ij0,2), curlv(ij0,3)  )
    end do
    
  end procedure curl_ptp_sub
  
end submodule curl_ptp