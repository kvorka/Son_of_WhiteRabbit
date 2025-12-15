submodule (solution) temperature
  implicit none; contains
  
  module procedure temp_fn
    
    temp_fn = this%temp(ij)%arr(im,2*(ir-1)+1)
    
  end procedure temp_fn
  
  module procedure temp_jm_sub
    integer :: is, ij, im, ij0
    
    is = 2*(ir-1)+1
    
    do ij = 0, this%jmax
      ij0 = ij*(ij+1)/2+1
      
      do concurrent ( im = 0:ij )
        temp_jm(ij0+im) = this%temp(ij)%arr(im,is)
      end do
    end do
    
  end procedure temp_jm_sub
  
  module procedure temp3_jm_sub
    integer :: is, ij, im, ij0
    
    is = 2*(ir-1)+1
    
    do ij = 0, this%jmax
      ij0 = ij*(ij+1)/2+1
      
      do concurrent ( im = 0:ij )
        temp1(ij0+im) = this%temp(ij)%arr(im,is)
      end do
      
      do concurrent ( im = 0:ij )
        temp2(ij0+im) = this%temp(ij)%arr(im,is+2)
      end do
      
      do concurrent ( im = 0:ij )
        temp3(ij0+im) = this%temp(ij)%arr(im,is+4)
      end do
    end do
    
  end procedure temp3_jm_sub
  
end submodule temperature