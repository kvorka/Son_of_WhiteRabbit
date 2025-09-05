submodule (solution) inittemp
  implicit none; contains
  
  module procedure init_stemp_sub
    
    allocate( this%temp(2*this%nd+1, this%jms) )
      this%temp = czero
    
  end procedure init_stemp_sub
  
end submodule inittemp