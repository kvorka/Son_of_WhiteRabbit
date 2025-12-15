submodule (physicalobject) equations_mech
  implicit none ; contains
  
  module procedure init_eq_mech_sub
    
    call this%sol%init_smech_sub()
    call this%mat%init_mmech_sub()
    call this%rhs%init_rmech_sub()
    
  end procedure init_eq_mech_sub

end submodule equations_mech