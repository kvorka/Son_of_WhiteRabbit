submodule (physicalobject) curl_ptp
  implicit none; contains
  
  module procedure curl_ptp_sub
    integer        :: ij, ij0
    real(kind=dbl) :: crr
    
    crr = fac / this%rad_grid%rr(ir)
    
    !ij = 0
      !im = 0
        curlv(1,1) = czero
        curlv(1,2) = czero
        curlv(1,3) = czero
    
    do ij = 1, this%jmax
      ij0 = jm(ij,0)
      
      call curl_ptp_j_sub( ij+1, sqrt( ( ij+1 ) / ( 2*ij + one ) ) * fac,  &
                         &             ( ij-1 ) * crr,                     &
                         &             ( ij+1 ) * crr,                     &
                         &       sqrt( ( ij   ) / ( 2*ij + one ) ) * fac,  &
                         &             ( ij   ) * crr,                     &
                         &             ( ij+2 ) * crr,                     &
                         &       dv_dr(ij0,1), &
                         &       dv_dr(ij0,2), &
                         &       dv_dr(ij0,3), &
                         &       v(ij0,1),     &
                         &       v(ij0,2),     &
                         &       v(ij0,3),     &
                         &       curlv(ij0,1), &
                         &       curlv(ij0,2), &
                         &       curlv(ij0,3)  )
    end do
    
  end procedure curl_ptp_sub
  
end submodule curl_ptp