submodule (solution) temperature
  implicit none; contains
  
  module procedure temp_fn
    
    temp_fn = this%temp(2*(ir-1)+1,ijm)
    
  end procedure temp_fn
  
  module procedure temp_jm_sub
    integer :: ijm, is
    
    is = 2*(ir-1)+1
    
    do ijm = 1, this%jms
      temp_jm(ijm) = this%temp(is,ijm)
    end do
    
  end procedure temp_jm_sub
  
  module procedure temp3_jm_sub
    integer :: ijm, is
    
    is = 2*(ir-1)+1
    
    do ijm = 1, this%jms
      temp1(ijm) = this%temp(is  ,ijm)
      temp2(ijm) = this%temp(is+2,ijm)
      temp3(ijm) = this%temp(is+4,ijm)
    end do
    
  end procedure temp3_jm_sub
  
end submodule temperature