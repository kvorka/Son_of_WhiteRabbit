submodule (physicalobject) dVelc_j
  implicit none; contains
  
  module procedure dv_dr_ptp_rr_jm_sub
    integer                        :: ijm
    real(kind=dbl)                 :: fac1, fac2, fac3
    complex(kind=dbl), allocatable :: v3(:,:)
    
    fac1 = this%rad_grid%drr(ir,-1)
    fac2 = this%rad_grid%drr(ir, 0)
    fac3 = this%rad_grid%drr(ir,+1)
    
    allocate( v3(this%jms,3) )
      
      call this%velc3_ptp_rr_jm_sub( ir-1, dv, v, v3 )
      
      call copy4_carray_sub( this%jms, fac3, fac2, fac1, v3(1,1), v(1,1), dv(1,1) )
      call copy4_carray_sub( this%jms, fac3, fac2, fac1, v3(1,2), v(1,2), dv(1,2) )
      call copy4_carray_sub( this%jms, fac3, fac2, fac1, v3(1,3), v(1,3), dv(1,3) )
      
    deallocate( v3 )
    
  end procedure dv_dr_ptp_rr_jm_sub
  
end submodule dVelc_j