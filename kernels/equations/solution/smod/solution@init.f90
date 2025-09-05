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
  
  module procedure init_stemp_sub
    
    allocate( this%temp(2*this%nd+1, this%jms) ); this%temp = czero
    
  end procedure init_stemp_sub
  
  module procedure init_storr_sub
      
    allocate( this%torr(2*this%nd+1, this%jms) ); this%torr = czero
    
  end procedure init_storr_sub
  
  module procedure init_smech_sub
    
    allocate( this%mech(5*this%nd+2,this%jms) ); this%mech = czero
    
  end procedure init_smech_sub
  
end submodule init