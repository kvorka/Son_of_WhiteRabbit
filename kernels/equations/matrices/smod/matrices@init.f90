submodule (matrices) init
  implicit none; contains
  
  module procedure init_matrices_sub
    
    this%nd   = nd
    this%jmax = jmax
    
  end procedure init_matrices_sub
  
  module procedure deallocate_matrices_sub
    integer :: j
    
    call this%temp(0)%deallocate_sub()
    
    do j = 1, this%jmax
      call this%temp(j)%deallocate_sub()
      call this%torr(j)%deallocate_sub()
      call this%mech(j)%deallocate_sub()
    end do
    
  end procedure deallocate_matrices_sub
  
end submodule init