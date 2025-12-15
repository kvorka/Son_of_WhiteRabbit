submodule (spharray) init
  implicit none; contains
  
  module procedure init_spharray_sub
    
    allocate( this%arr(0:mm,nd) ) ; this%arr = czero
    
  end procedure init_spharray_sub
  
  module procedure deallocate_spharray_sub
    
    deallocate( this%arr )
    
  end procedure deallocate_spharray_sub
  
end submodule init