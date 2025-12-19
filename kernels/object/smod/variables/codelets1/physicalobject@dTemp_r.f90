submodule (physicalobject) dTemp_r
  implicit none; contains
  
  module procedure dT_dr_r_fn
    real(kind=dbl) :: fac1, fac2, fac3, fac4
    
    fac1 = this%rad_grid%d(ir,-2)
    fac2 = this%rad_grid%d(ir,-1)
    fac3 = this%rad_grid%d(ir,+1)
    fac4 = this%rad_grid%d(ir,+2)
    
    if ( (ir > 1) .and. (ir < this%nd) ) then
      dT_dr_r_fn = fac1 * this%temp_rr_fn(ir-1,ij,im) + &
                 & fac2 * this%temp_rr_fn(ir  ,ij,im) + &
                 & fac3 * this%temp_rr_fn(ir+1,ij,im) + &
                 & fac4 * this%temp_rr_fn(ir+2,ij,im)
    
    else if ( ir == 1) then
      dT_dr_r_fn = fac2 * this%temp_rr_fn(ir  ,ij,im) + &
                 & fac3 * this%temp_rr_fn(ir+1,ij,im) + &
                 & fac4 * this%temp_rr_fn(ir+2,ij,im)
    
    else
      dT_dr_r_fn = fac1 * this%temp_rr_fn(ir-1,ij,im) + &
                 & fac2 * this%temp_rr_fn(ir  ,ij,im) + &
                 & fac3 * this%temp_rr_fn(ir+1,ij,im)
    
    end if
    
  end procedure dT_dr_r_fn
  
end submodule dTemp_r