submodule (solution) velocity
  implicit none; contains
  
  module procedure velocity_fn
    integer :: isp, ist
    
    velocity_fn = czero
    
    if ( ijm >= 2 ) then
      isp = 6*(ir-1)+1
      ist = 3*(ir-1)+1
      
      select case (il)
        case (-1)
          velocity_fn = this%mech(isp,ijm)
        case ( 0)
          velocity_fn = this%torr(ist,ijm)
        case (+1)
          velocity_fn = this%mech(isp+1,ijm)
      end select
    end if
    
  end procedure velocity_fn
  
  module procedure velocity_jml_sub
    integer :: ijm, ijml, isp, ist
    
    isp = 6*(ir-1)+1
    ist = 3*(ir-1)+1
    
    !ijm = 1
      !ijml = 1
        velocity_jml(1) = czero
    
    do ijm = 2, this%jms
      ijml = 3*(ijm-1)-1
      
      velocity_jml(ijml  ) = this%mech(isp  ,ijm)
      velocity_jml(ijml+1) = this%torr(ist  ,ijm)
      velocity_jml(ijml+2) = this%mech(isp+1,ijm)
    end do
    
  end procedure velocity_jml_sub
  
end submodule velocity