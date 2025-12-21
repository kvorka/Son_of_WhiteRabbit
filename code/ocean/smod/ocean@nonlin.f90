submodule (ocean) nonlin
  implicit none; contains
  
  module procedure vgradT_vcurlv_ocean_sub
    integer                        :: ir, ij, ij0
    complex(kind=dbl), allocatable :: v(:), curlv(:), T(:), gradT(:)
    
    !$omp parallel private (v, curlv, T, gradT)
    
    !$omp do private (ir,ij0) schedule (guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        call copy2_carray_sub(ij+1, 1-this%ab, this%ntemp(ij0,ir), this%temp(ij)%rhs1(0,ir))
        call copy2_carray_sub(ij+1, 1-this%ab, this%ntorr(ij0,ir), this%torr(ij)%rhs1(0,ir))
        call copy2_carray_sub(ij+1, 1-this%ab, this%nsph1(ij0,ir), this%mech(ij)%rhs1(0,ir))
        call copy2_carray_sub(ij+1, 1-this%ab, this%nsph2(ij0,ir), this%mech(ij)%rhs2(0,ir))
      end do
    end do
    !$omp end do
    
    allocate( v(this%jmv), curlv(this%jmv), T(this%jms), gradT(this%jmv) )
    
    !$omp do
    do ir = 2, this%nd
      !! Get vorticity and temperature gradient
      call this%curlv_rr_jml_sub(ir, v, curlv)
      call this%gradT_rr_jml_sub(ir, T, gradT, -1)
      
      !! Rescale curl(v) with Prandtl number and add ez for Coriolis force
      curlv(2) = curlv(2) * this%facPr + cs4pi * this%facEk
      call rescale_carray_sub( this%jmv-2, this%facPr, curlv(3) )
      
      !! Compute nonlinear terms
      call this%lat_grid%vcvv_vcvxv_sub( gradT, curlv, v, this%ntemp(1,ir), this%nsph1(1,ir), &
                                                       &  this%ntorr(1,ir), this%nsph2(1,ir)  )
      
      !! Add the buoyancy force with Newtonian gravity profile
      call this%buoy_rr_jml_sub(ir, T, this%nsph1(1,ir), this%nsph2(1,ir))
    end do
    !$omp end do
    
    deallocate( v, curlv, T, gradT )
    
    !$omp do private (ir,ij0) schedule (guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        call copy3_carray_sub(ij+1, this%ab, this%ntemp(ij0,ir), this%temp(ij)%rhs1(0,ir))
        call copy3_carray_sub(ij+1, this%ab, this%ntorr(ij0,ir), this%torr(ij)%rhs1(0,ir))
        call copy3_carray_sub(ij+1, this%ab, this%nsph1(ij0,ir), this%mech(ij)%rhs1(0,ir))
        call copy3_carray_sub(ij+1, this%ab, this%nsph2(ij0,ir), this%mech(ij)%rhs2(0,ir))
      end do
    end do
    !$omp end do
    
    !$omp end parallel
    
  end procedure vgradT_vcurlv_ocean_sub
  
end submodule nonlin