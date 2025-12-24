submodule (ocean) nonlin
  implicit none; contains
  
  module procedure vgradT_vcurlv_ocean_sub
    integer                        :: ir, ij, ij0
    complex(kind=dbl), allocatable :: v(:,:), curlv(:,:), T(:), gradT(:,:)
    
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
    
    allocate( v(this%jms,3), curlv(this%jms,3), T(this%jms), gradT(this%jms,3) )
    
    !$omp do
    do ir = 2, this%nd
      !! Get vorticity and temperature gradient
      call this%curlv_ptp_rr_jm_sub(ir, v, curlv)
      call this%gradT_ptp_rr_jm_sub(ir, T, gradT, -1)
      
      !! Rescale curl(v) with Prandtl number and add ez for Coriolis force
      call rescale_carray_sub( 3*this%jms, this%facPr, curlv(1,1) )
      curlv(2,1) = curlv(2,1) + cs4pi * this%facEk
      
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
  
  module procedure vgradT_vcurlv_ocean_2_sub
    integer                        :: ir, ij, ij0, irtp, i1
    type(c_ptr)                    :: vr_c, vt_c, vp_c, gr_c, gt_c, gp_c, hr_c, ht_c, hp_c
    real(kind=dbl),    pointer     :: vr(:), vt(:), vp(:), gr(:), gt(:), gp(:), hr(:), ht(:), hp(:)
    complex(kind=dbl), allocatable :: v(:,:), curlv(:,:), T(:), gradT(:,:)
    
    !$omp parallel private ( v, curlv, T, gradT, vr_c, vr, vt_c, vt, vp_c, vp, &
    !$omp                  &                     gr_c, gr, gt_c, gt, gp_c, gp, &
    !$omp                  &                     hr_c, hr, ht_c, ht, hp_c, hp  )
    
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
    
    allocate( v(this%jms,3), curlv(this%jms,3), T(this%jms), gradT(this%jms,3) )
    
    call this%lat_grid2%alloc_grid_sub( vr_c, vr )
    call this%lat_grid2%alloc_grid_sub( vt_c, vt )
    call this%lat_grid2%alloc_grid_sub( vp_c, vp )

    call this%lat_grid2%alloc_grid_sub( gr_c, gr )
    call this%lat_grid2%alloc_grid_sub( gt_c, gt )
    call this%lat_grid2%alloc_grid_sub( gp_c, gp )
    
    call alloc_aligned1d_sub( 16, hr_c, hr )
    call alloc_aligned1d_sub( 16, ht_c, ht )
    call alloc_aligned1d_sub( 16, hp_c, hp )
    
    !$omp do private (irtp,i1)
    do ir = 2, this%nd
      !! Get vorticity and temperature gradient
      call this%curlv_ptp_rr_jm_sub(ir, v, curlv)
      call this%gradT_ptp_rr_jm_sub(ir, T, gradT, -1)
      
      !! Rescale curl(v) with Prandtl number and add ez for Coriolis force
      call rescale_carray_sub( 3*this%jms, this%facPr, curlv(1,1) )
      curlv(2,1) = curlv(2,1) + cs4pi * this%facEk
      
      !! Non-linear terms :: curlv x v and buoyancy
      call this%lat_grid2%jml_to_grid_sub( v(1,1), v(1,2), v(1,3), vr, vt, vp )
      call this%lat_grid2%jml_to_grid_sub( curlv(1,1), curlv(1,2), curlv(1,3), gr, gt, gp )
      
      do irtp = 1, this%lat_grid2%nL * this%lat_grid2%nF, 16
        !$omp simd
        do i1 = 1, 16
          hr(i1) = gt(irtp+i1-1) * vp(irtp+i1-1) - gp(irtp+i1-1) * vt(irtp+i1-1)
          ht(i1) = gp(irtp+i1-1) * vr(irtp+i1-1) - gr(irtp+i1-1) * vp(irtp+i1-1)
          hp(i1) = gr(irtp+i1-1) * vt(irtp+i1-1) - gt(irtp+i1-1) * vr(irtp+i1-1)
        end do
        
        !$omp simd
        do i1 = 1, 16
          gr(irtp+i1-1) = hr(i1)
          gt(irtp+i1-1) = ht(i1)
          gp(irtp+i1-1) = hp(i1)
        end do
      end do
      
      call this%lat_grid2%grid_to_jml_sub( gr, gt, gp, this%nsph1(1,ir), this%ntorr(1,ir), this%nsph2(1,ir) )
      call this%buoy_rr_jml_sub(ir, T, this%nsph1(1,ir), this%nsph2(1,ir))
      
      this%nsph1(1,ir) = czero
      this%ntorr(1,ir) = czero
      this%nsph2(1,ir) = czero
      
      !! Non-linear terms :: gradT . v
      call this%lat_grid2%jml_to_grid_sub( gradT(1,1), gradT(1,2), gradT(1,3), gr, gt, gp )
      
      !$omp simd
      do irtp = 1, this%lat_grid2%nL * this%lat_grid2%nF
        gr(irtp) = gr(irtp) * vr(irtp) + gt(irtp) * vt(irtp) + gp(irtp) * vp(irtp)
      end do
      
      call this%lat_grid2%grid_to_jm_sub( gr, this%ntemp(1,ir) )
    end do
    !$omp end do
    
    deallocate( v, curlv, T, gradT )
    
    call free_aligned1d_sub( vr_c, vr )
    call free_aligned1d_sub( vt_c, vt )
    call free_aligned1d_sub( vp_c, vp )
    
    call free_aligned1d_sub( gr_c, gr )
    call free_aligned1d_sub( gt_c, gt )
    call free_aligned1d_sub( gp_c, gp )
    
    call free_aligned1d_sub( hr_c, hr )
    call free_aligned1d_sub( ht_c, ht )
    call free_aligned1d_sub( hp_c, hp )
    
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
    
  end procedure vgradT_vcurlv_ocean_2_sub
  
end submodule nonlin