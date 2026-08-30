submodule (physicalobject) dTemp_j
  implicit none; contains
  
  module procedure dT_dr_r_jm_sub
    real(kind=dbl)                 :: fac1, fac2, fac3, fac4
    complex(kind=dbl), allocatable :: temp1(:), temp2(:), temp3(:)
    
    fac1 = this%rad_grid%d(ir,-2)
    fac2 = this%rad_grid%d(ir,-1)
    fac3 = this%rad_grid%d(ir,+1)
    fac4 = this%rad_grid%d(ir,+2)
    
    if ( (ir > 1) .and. (ir < this%nd) ) then
      allocate( temp1(this%jms), temp2(this%jms), temp3(this%jms) )
        
        call this%temp4_rr_jm_sub( ir-1, temp1, temp2, temp3, dT_dr_r )
        
        call copy5_carray_sub( this%jms, fac1, fac2, fac3, fac4, temp1, temp2, temp3, dT_dr_r )
      
      deallocate( temp1, temp2, temp3 )
      
    else if ( ir == 1) then
      allocate( temp2(this%jms), temp3(this%jms) )
        
        call this%temp3_rr_jm_sub( ir, temp2, temp3, dT_dr_r )
        
        call copy4_carray_sub( this%jms, fac2, fac3, fac4, temp2, temp3, dT_dr_r )
        
      deallocate( temp2, temp3 )
    
    else
      allocate( temp1(this%jms), temp2(this%jms) )
        
        call this%temp3_rr_jm_sub( ir-1, temp1, temp2, dT_dr_r )
        
        call copy4_carray_sub( this%jms, fac1, fac2, fac3, temp1, temp2, dT_dr_r )
        
      deallocate( temp1, temp2 )
    
    end if
    
  end procedure dT_dr_r_jm_sub
  
  module procedure dT_dr_rr_jm_sub
    integer        :: ijm
    real(kind=dbl) :: fac1, fac2, fac3
    
    fac1 = this%rad_grid%drr(ir,-1)
    fac2 = this%rad_grid%drr(ir, 0)
    fac3 = this%rad_grid%drr(ir,+1)
    
    call this%temp3_rr_jm_sub( ir-1, dT, T, temp3 )
    
    call copy4_carray_sub( this%jms, fac3, fac2, fac1, temp3, T, dT )
    
    
  end procedure dT_dr_rr_jm_sub
  
end submodule dTemp_j