submodule (physicalobject) dVelc_j
  implicit none; contains
  
  module procedure dv_dr_rr_jml_sub
    integer                        :: ijml
    real(kind=dbl)                 :: fac1, fac2, fac3
    complex(kind=dbl), allocatable :: v3(:)
    
    fac1 = this%rad_grid%drr(ir,-1)
    fac2 = this%rad_grid%drr(ir, 0)
    fac3 = this%rad_grid%drr(ir,+1)
    
    allocate( v3(this%jmv) )
      
      call this%velc3_rr_jml_sub( ir-1, dv, v, v3 )
      
      do concurrent ( ijml = 1:this%jmv )
        dv(ijml) = fac1 * dv(ijml) + fac2 * v(ijml) + fac3 * v3(ijml)
      end do
      
    deallocate( v3 )
    
  end procedure dv_dr_rr_jml_sub
  
end submodule dVelc_j