submodule (solution) initmech
  implicit none; contains
  
  module procedure init_smech_sub
    
    allocate( this%mech(5*this%nd+2, this%jms) )
      this%mech = czero
    
  end procedure init_smech_sub
  
end submodule initmech