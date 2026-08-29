submodule (physicalobject) velc_jm
  implicit none; contains
  
  module procedure velc_rr_jml_sub
    integer                        :: ij, im, ij0, isp, ist
    complex(kind=dbl), allocatable :: vpol1(:), vtorr(:), vpol2(:)
    
    allocate( vpol1(0:this%jmax), &
            & vtorr(0:this%jmax), &
            & vpol2(0:this%jmax)  )
    
    isp = 5*(ir-1)+1
    ist = 2*(ir-1)+1
    
    !ij = 0
      !im = 0
        v_jml(1) = czero
    
    do ij = 1, this%jmax
      ij0 = 3*(ij*(ij+1)/2)-1
      
      call copy_carray_sub( ij+1, this%mech(ij)%sol(0,isp  ), vpol1(0) )
      call copy_carray_sub( ij+1, this%torr(ij)%sol(0,ist  ), vtorr(0) )
      call copy_carray_sub( ij+1, this%mech(ij)%sol(0,isp+1), vpol2(0) )
      
      !$omp simd
      do im = 0, ij
        v_jml(ij0+3*im  ) = vpol1(im)
        v_jml(ij0+3*im+1) = vtorr(im)
        v_jml(ij0+3*im+2) = vpol2(im)
      end do
    end do
    
    deallocate( vpol1, vtorr, vpol2 )
    
  end procedure velc_rr_jml_sub
  
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
