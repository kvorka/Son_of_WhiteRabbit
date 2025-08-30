submodule (physicalobject) balance_eqs
  implicit none ; contains
  
  module procedure laws_temp_fn
    real(kind=dbl) :: flow_dn, flow_up
    
    flow_dn = c2r_fn( -this%dT_dr_r_fn(1,1)       )
    flow_up = c2r_fn( +this%dT_dr_r_fn(this%nd,1) )
        
    laws_temp_fn = -flow_up / flow_dn / this%r_ud**2
    
  end procedure laws_temp_fn
  
end submodule balance_eqs