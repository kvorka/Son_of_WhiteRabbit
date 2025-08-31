submodule (physicalobject) equations_mech
  implicit none ; contains
  
  module procedure init_eq_mech_sub
    
    call this%sol%init_smech_sub()
    call this%mat%init_mmech_sub()

    allocate( this%rsph1(this%nd+1,this%jms) ) ; this%rsph1 = czero
    allocate( this%rsph2(this%nd+1,this%jms) ) ; this%rsph2 = czero
    
  end procedure init_eq_mech_sub

end submodule equations_mech