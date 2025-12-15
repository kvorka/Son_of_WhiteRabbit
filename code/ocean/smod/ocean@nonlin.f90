submodule (ocean) nonlin
  implicit none; contains
  
  module procedure vgradT_vcurlv_ocean_sub
    integer                        :: ir, ij, ijm, i1, i2, im, ij0
    real(kind=dbl)                 :: facj1, facj2
    complex(kind=dbl), allocatable :: v(:), curlv(:), T(:), gradT(:), nlm(:,:)
    
    !$omp parallel private (im, ij0, v, curlv, T, gradT, nlm)

    !$omp do collapse (2) schedule(guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        do im = 0, ij
          this%rhs%temp(ij)%arr(im,ir) = (1-this%ab) * this%ntemp(ij0+im,ir)
          this%rhs%torr(ij)%arr(im,ir) = (1-this%ab) * this%ntorr(ij0+im,ir)
          this%rhs%sph1(ij)%arr(im,ir) = (1-this%ab) * this%nsph1(ij0+im,ir)
          this%rhs%sph2(ij)%arr(im,ir) = (1-this%ab) * this%nsph2(ij0+im,ir)
        end do
      end do
    end do
    !$omp end do

    allocate( v(this%jmv), curlv(this%jmv), T(this%jms), gradT(this%jmv), nlm(4,this%jms) )
    
    !$omp do private(ij, ijm, facj1, facj2)
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
        
        do ijm = jm(ij,0), jm(ij,ij)
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
    
    !$omp do collapse (2) schedule(guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        do im = 0, ij
          this%rhs%temp(ij)%arr(im,ir) = this%rhs%temp(ij)%arr(im,ir) + this%ab * this%ntemp(ij0+im,ir)
          this%rhs%torr(ij)%arr(im,ir) = this%rhs%torr(ij)%arr(im,ir) + this%ab * this%ntorr(ij0+im,ir)
          this%rhs%sph1(ij)%arr(im,ir) = this%rhs%sph1(ij)%arr(im,ir) + this%ab * this%nsph1(ij0+im,ir)
          this%rhs%sph2(ij)%arr(im,ir) = this%rhs%sph2(ij)%arr(im,ir) + this%ab * this%nsph2(ij0+im,ir)
        end do
      end do
    end do
    !$omp end do
    
    !$omp end parallel
    
  end procedure vgradT_vcurlv_ocean_sub
  
end submodule nonlin