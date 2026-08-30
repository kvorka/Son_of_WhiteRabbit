submodule (physicalobject) curlV
  implicit none; contains
  
  module procedure curlv_ptp_rr_jm_sub
    complex(kind=dbl), allocatable :: dv(:,:)
    
    allocate( dv(this%jms,3) )
      
      call this%dv_dr_ptp_rr_jm_sub( ir, v, dv )
      
      call this%curl_ptp_sub( fac, ir, v, dv, curlv )
      
    deallocate( dv )
    
  end procedure curlv_ptp_rr_jm_sub
  
end submodule curlV