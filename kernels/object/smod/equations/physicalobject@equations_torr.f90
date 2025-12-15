submodule (physicalobject) equations_torr
  implicit none ; contains
  
  module procedure init_eq_torr_sub
    
    call this%sol%init_storr_sub()
    call this%mat%init_mtorr_sub()
    call this%rhs%init_rtorr_sub()
    
  end procedure init_eq_torr_sub

end submodule equations_torr