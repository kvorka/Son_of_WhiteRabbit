submodule (ocean) nonlin
  implicit none; contains
  
  module procedure vgradT_vgradv_ocean_sub
    integer                        :: ir, ijm
    complex(kind=dbl), allocatable :: v(:), dv(:), T(:), gradT(:), nlm(:,:), buoy(:,:), coriolis(:,:)
    
    !$omp parallel private (v, dv, T, gradT, ijm, nlm, coriolis, buoy)
    allocate( v(this%jmv), dv(this%jmv), T(this%jms), gradT(this%jmv), &
            & nlm(4,this%jms), buoy(2,this%jms), coriolis(3,this%jms)  )
    
    !$omp do
    do ir = 2, this%nd
      call this%dv_dr_rr_ijml_sub(ir, v, dv)
      call this%gradT_rr_ijml_sub(ir, T, gradT, -1)
      
      call this%lat_grid%vcvv_vcvgv_sub(this%rad_grid%rr(ir), gradT, dv, v, nlm)
      
      do ijm = 1, this%jms
        nlm(2,ijm) = nlm(2,ijm) * this%facPr
        nlm(3,ijm) = nlm(3,ijm) * this%facPr
        nlm(4,ijm) = nlm(4,ijm) * this%facPr
      end do
      
      call this%coriolis_rr_jml_sub(v, coriolis)
      call this%buoy_rr_jml_sub(ir, T, buoy)
      
      do ijm = 1, this%jms
        this%ntemp(ijm,ir) = nlm(1,ijm)
        this%nsph1(ijm,ir) = nlm(2,ijm)+buoy(1,ijm)+coriolis(1,ijm)
        this%ntorr(ijm,ir) = nlm(3,ijm)+            coriolis(2,ijm)
        this%nsph2(ijm,ir) = nlm(4,ijm)+buoy(2,ijm)+coriolis(3,ijm)
      end do
    end do
    !$omp end do
    
    deallocate( v , dv, T , gradT, nlm, buoy, coriolis )
    !$omp end parallel
    
  end procedure vgradT_vgradv_ocean_sub
  
  module procedure vgradT_vcurlv_ocean_sub
    integer                        :: ir, ij, ijm
    real(kind=dbl)                 :: facj1, facj2
    complex(kind=dbl), allocatable :: v(:), curlv(:), T(:), gradT(:), nlm(:,:)
    
    !$omp parallel private (ijm, ij, facj1, facj2, v, curlv, T, gradT, nlm)
    allocate( v(this%jmv), curlv(this%jmv), T(this%jms), gradT(this%jmv), nlm(4,this%jms) )
    
    !$omp do
    do ir = 2, this%nd
      !! Get vorticity and temperature gradient
      call this%curlv_rr_ijml_sub(ir, v, curlv)
      call this%gradT_rr_ijml_sub(ir, T, gradT, -1)
      
      !! Rescale curl(v) with Prandtl number and add ez for Coriolis force
      curlv(2) = curlv(2) * this%facPr + cs4pi * this%facEk
      
      do ijm = 3, this%jmv
        curlv(ijm) = curlv(ijm) * this%facPr
      end do
      
      !! Compute nonlinear terms
      call this%lat_grid%vcvv_vcvxv_sub(gradT, curlv, v, nlm)
      
      !! Add the buoyancy force with Newtonian gravity profile
      do ij = 1, this%jmax
        facj1 = -sqrt( (ij  ) / (2*ij+one) ) * this%facRa / this%rad_grid%rr(ir)**2
        facj2 = +sqrt( (ij+1) / (2*ij+one) ) * this%facRa / this%rad_grid%rr(ir)**2
        
        do ijm = ij*(ij+1)/2+1, ij*(ij+1)/2+ij+1
          nlm(2,ijm) = nlm(2,ijm) + facj1 * T(ijm)
          nlm(4,ijm) = nlm(4,ijm) + facj2 * T(ijm)
        end do
      end do
      
      !! Assign to output arrays
      do ijm = 1, this%jms
        this%ntemp(ijm,ir) = nlm(1,ijm)
        this%nsph1(ijm,ir) = nlm(2,ijm)
        this%ntorr(ijm,ir) = nlm(3,ijm)
        this%nsph2(ijm,ir) = nlm(4,ijm)
      end do
    end do
    !$omp end do
    
    deallocate( v , curlv, T , gradT, nlm )
    !$omp end parallel
    
  end procedure vgradT_vcurlv_ocean_sub
  
end submodule nonlin