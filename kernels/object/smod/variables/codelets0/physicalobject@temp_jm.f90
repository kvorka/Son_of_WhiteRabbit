submodule (physicalobject) temp_jm
  implicit none; contains
  
  module procedure temp_rr_jm_sub
    integer :: is, ij
    
    is = 2*(ir-1)+1
    
    do ij = 0, this%jmax
      call copy_carray_sub( ij+1, this%temp(ij)%sol(0,is), temp_jm(ij*(ij+1)/2+1) )
    end do
    
  end procedure temp_rr_jm_sub
  
  module procedure temp3_rr_jm_sub
    integer :: is, ij, ij0
    
    is = 2*(ir-1)+1
    
    do ij = 0, this%jmax
      ij0 = jm(ij,0)
      
      call copy_carray_sub( ij+1, this%temp(ij)%sol(0,is  ), temp1(ij0) )
      call copy_carray_sub( ij+1, this%temp(ij)%sol(0,is+2), temp2(ij0) )
      call copy_carray_sub( ij+1, this%temp(ij)%sol(0,is+4), temp3(ij0) )
    end do
    
  end procedure temp3_rr_jm_sub
  
  module procedure temp4_rr_jm_sub
    integer :: is, ij, ij0
    
    is = 2*(ir-1)+1
    
    do ij = 0, this%jmax
      ij0 = jm(ij,0)
      
      call copy_carray_sub( ij+1, this%temp(ij)%sol(0,is  ), temp1(ij0) )
      call copy_carray_sub( ij+1, this%temp(ij)%sol(0,is+2), temp2(ij0) )
      call copy_carray_sub( ij+1, this%temp(ij)%sol(0,is+4), temp3(ij0) )
      call copy_carray_sub( ij+1, this%temp(ij)%sol(0,is+6), temp4(ij0) )
    end do
    
  end procedure temp4_rr_jm_sub
  
end submodule temp_jm
