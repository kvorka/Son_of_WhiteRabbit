submodule (ocean) nonlin
  implicit none; contains
  
  module procedure vgradT_vcurlv_ocean_sub
    integer                        :: ir, ij, ijm, i1, i2, im, ij0
    real(kind=dbl)                 :: facj1, facj2
    complex(kind=dbl), allocatable :: v(:), curlv(:), T(:), gradT(:), ntemp(:), nsph1(:), ntorr(:), nsph2(:)
    
    !$omp parallel private (im, ij0, ijm, v, curlv, T, gradT, ntemp, nsph1, ntorr, nsph2)
    
    !$omp do schedule(guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        do im = 0, ij
          this%rhs%temp(ij)%arr(im,ir) = (1-this%ab) * this%ntemp(ij0+im,ir)
        end do
      end do
    end do
    !$omp end do nowait
    
    !$omp do schedule(guided,2)
    do ij = 1, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        do im = 0, ij
          this%rhs%torr(ij)%arr(im,ir) = (1-this%ab) * this%ntorr(ij0+im,ir)
          this%rhs%sph1(ij)%arr(im,ir) = (1-this%ab) * this%nsph1(ij0+im,ir)
          this%rhs%sph2(ij)%arr(im,ir) = (1-this%ab) * this%nsph2(ij0+im,ir)
        end do
      end do
    end do
    !$omp end do
    
    allocate( v(this%jmv), curlv(this%jmv), T(this%jms), gradT(this%jmv),        &
            & ntemp(this%jms), nsph1(this%jms), ntorr(this%jms), nsph2(this%jms) )
    
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
      call this%lat_grid%vcvv_vcvxv_sub(gradT, curlv, v, ntemp, nsph1, ntorr, nsph2)
      
      !! Add the buoyancy force with Newtonian gravity profile
      call this%buoy_rr_jml_sub(ir, T, nsph1, nsph2)
      
      !! Assign to output arrays
      call copy_carray_sub(this%jms, this%ntemp(1,ir), ntemp)
      call copy_carray_sub(this%jms, this%nsph1(1,ir), nsph1)
      call copy_carray_sub(this%jms, this%ntorr(1,ir), ntorr)
      call copy_carray_sub(this%jms, this%nsph2(1,ir), nsph2)
    end do
    !$omp end do
    
    deallocate( v , curlv, T , gradT, ntemp, nsph1, ntorr, nsph2 )
    
    !$omp do schedule(guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        do im = 0, ij
          this%rhs%temp(ij)%arr(im,ir) = this%rhs%temp(ij)%arr(im,ir) + this%ab * this%ntemp(ij0+im,ir)
        end do
      end do
    end do
    !$omp end do nowait
    
    !$omp do schedule(guided,2)
    do ij = 1, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        do im = 0, ij
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