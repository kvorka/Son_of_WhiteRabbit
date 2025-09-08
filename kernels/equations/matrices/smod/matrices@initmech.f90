submodule (Matrices) initmech
  implicit none; contains
  
  module procedure init_mmech_sub
    integer :: j
    
    allocate( this%mech(1:this%jmax) )
    
    do j = 1, this%jmax
      call this%mech(j)%init_sub(5*this%nd+2, 9, 8)
    end do
    
  end procedure init_mmech_sub
  
end submodule initmech