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
    
    call zero_carray_sub( this%jmv, velocity1 )
    
    isp = 6*(ir-1)+1
    ist = 3*(ir-1)+1
    
    do concurrent ( ijm = 2:this%jms )
      ijml = 3*(ijm-1)-1
      
      velocity1(ijml  ) = this%mech(isp,ijm)
      velocity1(ijml+1) = this%torr(ist,ijm)
      velocity1(ijml+2) = this%mech(isp+1,ijm)
    end do
    
  end procedure velocity_jml_sub
  
end submodule velocity