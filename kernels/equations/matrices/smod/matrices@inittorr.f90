submodule (matrices) inittorr
  implicit none; contains
  
  module procedure init_mtorr_sub
    integer :: j
    
    allocate( this%torr(1:this%jmax) )
    
    do j = 1, this%jmax
      call this%torr(j)%init_sub(3*this%nd+1, 5, 5)
    end do
    
  end procedure init_mtorr_sub
  
end submodule inittorr