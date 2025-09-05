submodule (solution) inittorr
  implicit none; contains
  
  module procedure init_storr_sub
    
    allocate( this%torr(2*this%nd+1, this%jms) )
      this%torr = czero
    
  end procedure init_storr_sub
  
end submodule inittorr