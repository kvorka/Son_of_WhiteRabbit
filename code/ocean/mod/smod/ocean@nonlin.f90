submodule (ocean) nonlin
  implicit none; contains
  
  module procedure fullnl_ocean_sub
    integer                        :: ir, ijm
    real(kind=dbl)                 :: fac
    complex(kind=dbl), allocatable :: v(:), dv(:), T(:), gradT(:), nlm(:,:), buoy(:,:), coriolis(:,:)
    
    !$omp parallel private (v, dv, T, gradT, ijm, fac, nlm, coriolis, buoy)
    allocate( v(this%jmv), dv(this%jmv), T(this%jms), gradT(this%jmv), &
            & nlm(4,this%jms), buoy(2,this%jms), coriolis(3,this%jms)  )
    
    !$omp do
    do ir = 2, this%nd
      call this%dv_dr_rr_ijml_sub(ir, v, dv)
      call this%gradT_rr_ijml_sub(ir, T, gradT, -1)
      
      call this%lat_grid%vcvv_vcvgv_sub(this%rad_grid%rr(ir), gradT, dv, v, nlm)
      
      fac = 1 / this%Pr
        do ijm = 1, this%jms
          nlm(2,ijm) = nlm(2,ijm) * fac
          nlm(3,ijm) = nlm(3,ijm) * fac
          nlm(4,ijm) = nlm(4,ijm) * fac
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
    
  end procedure fullnl_ocean_sub
  
  module procedure fullnl2_ocean_sub
    integer                        :: ir, ij, ijm
    real(kind=dbl)                 :: facPr, facBu, facj1, facj2
    complex(kind=dbl)              :: facEk
    complex(kind=dbl), allocatable :: v(:), curlv(:), T(:), gradT(:), nlm(:,:)
    
    facPr = 1 / this%Pr
    facBu = this%Ra / ( 1 - this%r_ud )**2
    facEk = cs4pi * 2 / this%Ek
    
    !$omp parallel private (ijm, ij, facj1, facj2, v, curlv, T, gradT, nlm)
    allocate( v(this%jmv), curlv(this%jmv), T(this%jms), gradT(this%jmv), nlm(4,this%jms) )
    
    !$omp do
    do ir = 2, this%nd
      !! Get vorticity and temperature gradient
      call this%curlv_rr_ijml_sub(ir, v, curlv)
      call this%gradT_rr_ijml_sub(ir, T, gradT, -1)
      
      !! Rescale curl(v) with Prandtl number and add ez for Coriolis force
      curlv(2) = curlv(2) * facPr + cs4pi * 2 / this%Ek
      
      do ijm = 3, this%jmv
        curlv(ijm) = curlv(ijm) * facPr
      end do
      
      !! Compute nonlinear terms
      call this%lat_grid%vcvv_vcvxv_sub(gradT, curlv, v, nlm)
      
      !! Add the buoyancy force with Newtonian gravity profile
      do ij = 1, this%jmax
        facj1 = -sqrt( (ij  ) / (2*ij+one) ) * facBu / this%rad_grid%rr(ir)**2
        facj2 = +sqrt( (ij+1) / (2*ij+one) ) * facBu / this%rad_grid%rr(ir)**2
        
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
    
  end procedure fullnl2_ocean_sub
  
end submodule nonlin