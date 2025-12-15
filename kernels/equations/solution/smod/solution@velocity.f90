submodule (solution) velocity
  implicit none; contains
  
  module procedure velocity_fn
    integer :: isp, ist
    
    velocity_fn = czero
    
    if ( ij >= 1 ) then
      isp = 5*(ir-1)+1
      ist = 2*(ir-1)+1
      
      select case (il)
        case (-1)
          velocity_fn = this%mech(ij)%arr(im,isp)
        case ( 0)
          velocity_fn = this%torr(ij)%arr(im,ist)
        case (+1)
          velocity_fn = this%mech(ij)%arr(im,isp+1)
      end select
    end if
    
  end procedure velocity_fn
  
  module procedure velocity_jml_sub
    integer :: ij, im, ij0, isp, ist
    
    isp = 5*(ir-1)+1
    ist = 2*(ir-1)+1
    
    !ij = 0
      !im = 0
        velocity_jml(1) = czero
    
    do ij = 1, this%jmax
      ij0 = 3*(ij*(ij+1)/2)-1
      
      do im = 0, ij
        velocity_jml(ij0+3*im  ) = this%mech(ij)%arr(im,isp  )
        velocity_jml(ij0+3*im+1) = this%torr(ij)%arr(im,ist  )
        velocity_jml(ij0+3*im+2) = this%mech(ij)%arr(im,isp+1)
      end do
    end do
    
  end procedure velocity_jml_sub
  
  module procedure velocity3_jml_sub
    integer :: ij, im, ij0, isp, ist
    
    isp = 5*(ir-1)+1
    ist = 2*(ir-1)+1
    
    !ij = 0
      !im = 0
        velocity1(1) = czero
        velocity2(1) = czero
        velocity3(1) = czero
        
    do ij = 1, this%jmax
      ij0 = 3*(ij*(ij+1)/2)-1
      
      do im = 0, ij
        velocity1(ij0+3*im  ) = this%mech(ij)%arr(im,isp  )
        velocity1(ij0+3*im+1) = this%torr(ij)%arr(im,ist  )
        velocity1(ij0+3*im+2) = this%mech(ij)%arr(im,isp+1)
      end do
      
      do im = 0, ij
        velocity2(ij0+3*im  ) = this%mech(ij)%arr(im,isp+5)
        velocity2(ij0+3*im+1) = this%torr(ij)%arr(im,ist+2)
        velocity2(ij0+3*im+2) = this%mech(ij)%arr(im,isp+6)
      end do
      
      do im = 0, ij
        velocity3(ij0+3*im  ) = this%mech(ij)%arr(im,isp+10)
        velocity3(ij0+3*im+1) = this%torr(ij)%arr(im,ist+ 4)
        velocity3(ij0+3*im+2) = this%mech(ij)%arr(im,isp+11)
      end do
    end do
    
  end procedure velocity3_jml_sub
  
end submodule velocity