submodule (sphsvt) init
  implicit none; contains
  
  module procedure init_sphsvt_sub
    
    this%jmax  = jmax
    this%jmax1 = jmax+1
    this%jmax2 = jmax+2
    
    this%jms  = jm(this%jmax  ,this%jmax  )
    this%jms1 = jm(this%jmax+1,this%jmax+1)
    
    this%jmv  = jml(this%jmax,this%jmax,+1)
    
  end procedure init_sphsvt_sub
  
end submodule init