submodule (matrices) inittemp
  implicit none; contains
  
  module procedure init_mtemp_sub
    integer :: j
    
    allocate( this%temp(0:this%jmax) )
    
    do j = 0, this%jmax
      call this%temp(j)%init_sub(2*this%nd+1, 3, 3)
    end do
    
  end procedure init_mtemp_sub
  
end submodule inittemp