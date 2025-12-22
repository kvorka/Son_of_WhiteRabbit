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
  
  module procedure velc3_ptp_rr_jm_sub
    integer :: ij, ij0, is
    
    !! Poloidal coefficients of velocity
    is = 5*(ir-1)+1
    
    !ij = 0
      !im = 0
        v1(1,1) = czero
        v1(1,3) = czero
        v2(1,1) = czero
        v2(1,3) = czero
        v3(1,1) = czero
        v3(1,3) = czero
        
    do ij = 1, this%jmax
      ij0 = jm(ij,0)
      
      call copy_carray_sub( ij+1, this%mech(ij)%sol(0,is   ), v1(ij0,1) )
      call copy_carray_sub( ij+1, this%mech(ij)%sol(0,is+ 1), v1(ij0,3) )
      call copy_carray_sub( ij+1, this%mech(ij)%sol(0,is+ 5), v2(ij0,1) )
      call copy_carray_sub( ij+1, this%mech(ij)%sol(0,is+ 6), v2(ij0,3) )
      call copy_carray_sub( ij+1, this%mech(ij)%sol(0,is+10), v3(ij0,1) )
      call copy_carray_sub( ij+1, this%mech(ij)%sol(0,is+11), v3(ij0,3) )
    end do
    
    !! Toroidal coefficient of velocity
    is = 2*(ir-1)+1
    
    !ij = 0
      !im = 0
        v1(1,2) = czero
        v2(1,2) = czero
        v3(1,2) = czero
        
    do ij = 1, this%jmax
      ij0 = jm(ij,0)
      
      call copy_carray_sub( ij+1, this%torr(ij)%sol(0,is  ), v1(ij0,2) )
      call copy_carray_sub( ij+1, this%torr(ij)%sol(0,is+2), v2(ij0,2) )
      call copy_carray_sub( ij+1, this%torr(ij)%sol(0,is+4), v3(ij0,2) )
    end do
    
  end procedure velc3_ptp_rr_jm_sub
  
end submodule velc_jm
