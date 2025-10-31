submodule (solution) velocity
  implicit none; contains
  
  module procedure velocity_fn
    integer :: isp, ist
    
    velocity_fn = czero
    
    if ( ijm >= 2 ) then
      isp = 5*(ir-1)+1
      ist = 2*(ir-1)+1
      
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
    
    isp = 5*(ir-1)+1
    ist = 2*(ir-1)+1
    
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
  
  module procedure velocity3_jml_sub
    integer :: ijm, ijml, isp, ist
    
    velocity1(1) = czero
    velocity2(1) = czero
    velocity3(1) = czero
    
    isp = 5*(ir-1)+1
    ist = 2*(ir-1)+1
    
    do ijm = 2, this%jms
      ijml = 3*(ijm-1)-1
      
      velocity1(ijml  ) = this%mech(isp  ,ijm)
      velocity1(ijml+1) = this%torr(ist  ,ijm)
      velocity1(ijml+2) = this%mech(isp+1,ijm)
      
      velocity2(ijml  ) = this%mech(isp+5,ijm)
      velocity2(ijml+1) = this%torr(ist+2,ijm)
      velocity2(ijml+2) = this%mech(isp+6,ijm)
      
      velocity3(ijml  ) = this%mech(isp+10,ijm)
      velocity3(ijml+1) = this%torr(ist+ 4,ijm)
      velocity3(ijml+2) = this%mech(isp+11,ijm)
    end do
    
  end procedure velocity3_jml_sub
  
end submodule velocity