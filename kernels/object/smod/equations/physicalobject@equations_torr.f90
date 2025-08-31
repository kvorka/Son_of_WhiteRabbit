submodule (physicalobject) equations_torr
  implicit none ; contains
  
  module procedure init_eq_torr_sub
    
    call this%sol%init_storr_sub()
    call this%mat%init_mtorr_sub()
    
    allocate( this%rtorr(this%nd+1,this%jms) ) ; this%rtorr = czero
    
  end procedure init_eq_torr_sub

end submodule equations_torr