submodule (physicalobject) temp_jm
  implicit none; contains
  
  module procedure temp_rr_jm_sub
    integer :: is, ij, im, ij0
    
    is = 2*(ir-1)+1
    
    do ij = 0, this%jmax
      ij0 = ij*(ij+1)/2+1
      
      do concurrent ( im = 0:ij )
        temp_jm(ij0+im) = this%temp(ij)%sol(im,is)
      end do
    end do
    
  end procedure temp_rr_jm_sub
  
  module procedure temp3_rr_jm_sub
    integer :: is, ij, im, ij0
    
    is = 2*(ir-1)+1
    
    do ij = 0, this%jmax
      ij0 = ij*(ij+1)/2+1
      
      do concurrent ( im = 0:ij )
        temp1(ij0+im) = this%temp(ij)%sol(im,is)
      end do
      
      do concurrent ( im = 0:ij )
        temp2(ij0+im) = this%temp(ij)%sol(im,is+2)
      end do
      
      do concurrent ( im = 0:ij )
        temp3(ij0+im) = this%temp(ij)%sol(im,is+4)
      end do
    end do
    
  end procedure temp3_rr_jm_sub
  
  module procedure temp4_rr_jm_sub
    integer :: is, ij, im, ij0
    
    is = 2*(ir-1)+1
    
    do ij = 0, this%jmax
      ij0 = ij*(ij+1)/2+1
      
      do concurrent ( im = 0:ij )
        temp1(ij0+im) = this%temp(ij)%sol(im,is)
      end do
      
      do concurrent ( im = 0:ij )
        temp2(ij0+im) = this%temp(ij)%sol(im,is+2)
      end do
      
      do concurrent ( im = 0:ij )
        temp3(ij0+im) = this%temp(ij)%sol(im,is+4)
      end do
      
      do concurrent ( im = 0:ij )
        temp4(ij0+im) = this%temp(ij)%sol(im,is+6)
      end do
    end do
    
  end procedure temp4_rr_jm_sub
  
end submodule temp_jm