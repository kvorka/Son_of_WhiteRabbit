submodule (sphsvt2) init
  implicit none; contains
  
  module procedure init_sphsvt2_sub
    
    this%jmax = jmax
    this%jms  = jm(jmax,jmax)
    
  end procedure init_sphsvt2_sub
  
end submodule init