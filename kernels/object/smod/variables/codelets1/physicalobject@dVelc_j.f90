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
      
      !$omp simd
      do ijm = 1, this%jms
        dv(ijm,1) = fac1 * dv(ijm,1) + fac2 * v(ijm,1) + fac3 * v3(ijm,1)
        dv(ijm,2) = fac1 * dv(ijm,2) + fac2 * v(ijm,2) + fac3 * v3(ijm,2)
        dv(ijm,3) = fac1 * dv(ijm,3) + fac2 * v(ijm,3) + fac3 * v3(ijm,3)
      end do
      
    deallocate( v3 )
    
  end procedure dv_dr_ptp_rr_jm_sub
  
end submodule dVelc_j