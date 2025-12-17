submodule (solution) init
  implicit none; contains
  
  module procedure init_solution_sub
    
    this%nd   = nd
    this%jmax = jmax
    
  end procedure init_solution_sub
  
  module procedure deallocate_solution_sub
    integer :: ij
    
    call this%temp(0)%deallocate_sub()
    
    do ij = 1, this%jmax
      call this%temp(ij)%deallocate_sub()
      call this%mech(ij)%deallocate_sub()
      call this%torr(ij)%deallocate_sub()
    end do
    
    deallocate( this%temp )
    deallocate( this%torr )
    deallocate( this%mech )
    
  end procedure deallocate_solution_sub
  
end submodule init