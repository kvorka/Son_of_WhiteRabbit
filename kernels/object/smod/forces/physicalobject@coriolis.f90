submodule (physicalobject) coriolis
  implicit none; contains
  
  module procedure coriolis_rr_jml_sub
    
    call ezvv_sub(this%jmax, 2/this%Ek, v, coriolis)
    
  end procedure coriolis_rr_jml_sub
  
end submodule coriolis