submodule (physicalobject) gradT
  implicit none; contains
  
  module procedure gradT_ptp_rr_jm_sub
    complex(kind=dbl), allocatable :: dT(:)
    
    allocate( dT(this%jms) )
      
      call this%dT_dr_rr_jm_sub( ir, T, dT )
      
      call this%grad_ptp_sub( sgn, ir, T, dT, gradT )
      
    deallocate( dT )
    
  end procedure gradT_ptp_rr_jm_sub
  
end submodule gradT