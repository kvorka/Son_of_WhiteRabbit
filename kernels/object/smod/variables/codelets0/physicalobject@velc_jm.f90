submodule (physicalobject) velc_jm
  implicit none; contains
  
  module procedure velc_rr_jml_sub
    integer :: ij, im, ij0, isp, ist
    
    isp = 5*(ir-1)+1
    ist = 2*(ir-1)+1
    
    !ij = 0
      !im = 0
        v_jml(1) = czero
    
    do ij = 1, this%jmax
      ij0 = 3*(ij*(ij+1)/2)-1
      
      do concurrent ( im = 0:ij )
        v_jml(ij0+3*im  ) = this%mech(ij)%sol(im,isp  )
        v_jml(ij0+3*im+1) = this%torr(ij)%sol(im,ist  )
        v_jml(ij0+3*im+2) = this%mech(ij)%sol(im,isp+1)
      end do
    end do
    
  end procedure velc_rr_jml_sub
  
  module procedure velc3_rr_jml_sub
    integer :: ij, im, ij0, isp, ist
    
    isp = 5*(ir-1)+1
    ist = 2*(ir-1)+1
    
    !ij = 0
      !im = 0
        v1_jml(1) = czero
        v2_jml(1) = czero
        v3_jml(1) = czero
        
    do ij = 1, this%jmax
      ij0 = 3*(ij*(ij+1)/2)-1
      
      do concurrent ( im = 0:ij )
        v1_jml(ij0+3*im  ) = this%mech(ij)%sol(im,isp  )
        v1_jml(ij0+3*im+1) = this%torr(ij)%sol(im,ist  )
        v1_jml(ij0+3*im+2) = this%mech(ij)%sol(im,isp+1)
      end do
      
      do concurrent ( im = 0:ij )
        v2_jml(ij0+3*im  ) = this%mech(ij)%sol(im,isp+5)
        v2_jml(ij0+3*im+1) = this%torr(ij)%sol(im,ist+2)
        v2_jml(ij0+3*im+2) = this%mech(ij)%sol(im,isp+6)
      end do
      
      do concurrent ( im = 0:ij )
        v3_jml(ij0+3*im  ) = this%mech(ij)%sol(im,isp+10)
        v3_jml(ij0+3*im+1) = this%torr(ij)%sol(im,ist+ 4)
        v3_jml(ij0+3*im+2) = this%mech(ij)%sol(im,isp+11)
      end do
    end do
    
  end procedure velc3_rr_jml_sub
  
end submodule velc_jm