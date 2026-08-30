submodule (physicalobject) gradT
  implicit none; contains
  
  module procedure gradT_ptp_rr_jm_sub
    
    !! gradT is being reused for temporal temperature store, while
    !! work is getting dT_dr
    call this%dT_dr_rr_jm_sub( ir, T, work, gradT )
    
    !! actual recombination to obtain grad from T and dT_dr
    call this%grad_ptp_sub( fac, ir, T, work, gradT )
    
  end procedure gradT_ptp_rr_jm_sub
  
end submodule gradT