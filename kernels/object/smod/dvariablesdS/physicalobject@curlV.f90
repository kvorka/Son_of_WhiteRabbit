submodule (physicalobject) curlV
  implicit none; contains
  
  module procedure curlv_ptp_rr_jm_sub
    
    !! curlv is being reused for temporal velocity store, while
    !! work is getting dv_dr
    call this%dv_dr_ptp_rr_jm_sub( ir, v, work, curlv )
    
    !! actual recombination to obtain curl from v and dv_dr
    call this%curl_ptp_sub( fac, ir, v, work, curlv )
    
  end procedure curlv_ptp_rr_jm_sub
  
end submodule curlV