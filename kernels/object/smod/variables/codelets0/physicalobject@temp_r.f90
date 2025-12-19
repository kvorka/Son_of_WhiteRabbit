submodule (physicalobject) temp_r
  implicit none; contains
  
  module procedure temp_rr_fn
    
    temp_rr_fn = this%temp(ij)%sol(im,2*(ir-1)+1)
    
  end procedure temp_rr_fn
  
  module procedure temp_r_fn
    
    temp_r_fn = this%rad_grid%c(ir,-1) * this%temp(ij)%sol(im,2*(ir-1)+1) + &
              & this%rad_grid%c(ir,+1) * this%temp(ij)%sol(im,2*(ir  )+1)
    
  end procedure temp_r_fn
  
end submodule temp_r