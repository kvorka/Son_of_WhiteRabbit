submodule (physicalobject) equations_temp
  implicit none ; contains
  
  module procedure init_eq_temp_sub
    
    call this%sol%init_stemp_sub()
    call this%mat%init_mtemp_sub()
    
    allocate( this%rtemp(this%nd+1,this%jms) ) ; this%rtemp = czero
    
  end procedure init_eq_temp_sub

end submodule equations_temp