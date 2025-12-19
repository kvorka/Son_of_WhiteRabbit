submodule (physicalobject) dTemp_j
  implicit none; contains
  
  module procedure dT_dr_r_jm_sub
    integer                        :: ijm
    real(kind=dbl)                 :: fac1, fac2, fac3, fac4
    complex(kind=dbl), allocatable :: temp1(:), temp2(:), temp3(:), temp4(:)
    
    fac1 = this%rad_grid%d(ir,-2)
    fac2 = this%rad_grid%d(ir,-1)
    fac3 = this%rad_grid%d(ir,+1)
    fac4 = this%rad_grid%d(ir,+2)
    
    if ( (ir > 1) .and. (ir < this%nd) ) then
      allocate( temp1(this%jms), temp2(this%jms), temp3(this%jms), temp4(this%jms) )
        
        call this%temp4_rr_jm_sub( ir-1, temp1, temp2, temp3, temp4 )
        
        do concurrent ( ijm = 1:this%jms )
          dT_dr_r(ijm) = fac1 * temp1(ijm) + &
                       & fac2 * temp2(ijm) + &
                       & fac3 * temp3(ijm) + &
                       & fac4 * temp4(ijm)
        end do
      
      deallocate( temp1, temp2, temp3, temp4 )
      
    else if ( ir == 1) then
      allocate( temp2(this%jms), temp3(this%jms), temp4(this%jms) )
        
        call this%temp3_rr_jm_sub( ir, temp2, temp3, temp4 )
        
        do concurrent ( ijm = 1:this%jms )
          dT_dr_r(ijm) = fac2 * temp2(ijm) + &
                       & fac3 * temp3(ijm) + &
                       & fac4 * temp4(ijm)
        end do
      
      deallocate( temp2, temp3, temp4 )
    
    else
      allocate( temp1(this%jms), temp2(this%jms), temp3(this%jms) )
      
        call this%temp3_rr_jm_sub( ir-1, temp1, temp2, temp3 )
        
        do concurrent ( ijm = 1:this%jms )
          dT_dr_r(ijm) = fac1 * temp1(ijm) + &
                       & fac2 * temp2(ijm) + &
                       & fac3 * temp3(ijm)
        end do
      
      deallocate( temp1, temp2, temp3 )
    
    end if
    
  end procedure dT_dr_r_jm_sub
  
  module procedure dT_dr_rr_jm_sub
    integer                        :: ijm
    real(kind=dbl)                 :: fac1, fac2, fac3
    complex(kind=dbl), allocatable :: temp3(:)
    
    fac1 = this%rad_grid%drr(ir,-1)
    fac2 = this%rad_grid%drr(ir, 0)
    fac3 = this%rad_grid%drr(ir,+1)
    
    allocate( temp3(this%jms) )
      
      call this%temp3_rr_jm_sub( ir-1, dT, T, temp3 )
      
      do concurrent ( ijm = 1:this%jms )
        dT(ijm) = fac1 * dT(ijm) + &
                & fac2 * T(ijm)  + &
                & fac3 * temp3(ijm)
      end do
    
    deallocate( temp3 )
    
  end procedure dT_dr_rr_jm_sub
  
end submodule dTemp_j