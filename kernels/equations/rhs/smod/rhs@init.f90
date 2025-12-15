submodule (rhs) init
  implicit none; contains
  
  module procedure init_rhs_sub
    
    this%nd   = nd
    this%jmax = jmax
    
  end procedure init_rhs_sub
  
  module procedure deallocate_rhs_sub
    integer :: ij
    
    do ij = 0, this%jmax
      call this%temp(ij)%deallocate_sub()
      call this%sph1(ij)%deallocate_sub()
      call this%sph2(ij)%deallocate_sub()
      call this%torr(ij)%deallocate_sub()
    end do
    
    deallocate( this%temp )
    deallocate( this%torr )
    deallocate( this%sph1 )
    deallocate( this%sph2 )
    
  end procedure deallocate_rhs_sub
  
end submodule init