submodule (solution) init
  implicit none; contains
  
  module procedure init_solution_sub
    
    this%nd   = nd
    this%jmax = jmax
    this%jms  =       jmax * (jmax+1) / 2 + jmax   + 1
    this%jmv  = 3 * ( jmax * (jmax+1) / 2 + jmax ) + 1
    
  end procedure init_solution_sub
  
  module procedure deallocate_solution_sub
    
    deallocate( this%temp )
    deallocate( this%torr )
    deallocate( this%mech )
    
  end procedure deallocate_solution_sub
  
end submodule init