submodule (ocean) timescheme
  implicit none; contains
  
  module procedure time_scheme_ocean_sub
    integer :: ir, ijm
    
    !$omp parallel do collapse (2)
    do ijm = 1, this%jms
      do ir = 2, this%nd
        this%rtemp(ir,ijm) = (1-this%ab) * this%ntemp(ijm,ir)
        this%rtorr(ir,ijm) = (1-this%ab) * this%ntorr(ijm,ir)
        this%rsph1(ir,ijm) = (1-this%ab) * this%nsph1(ijm,ir)
        this%rsph2(ir,ijm) = (1-this%ab) * this%nsph2(ijm,ir)
      end do
    end do
    !$omp end parallel do
    
    call this%fullnl_sub()
    
    !$omp parallel do collapse (2)
    do ijm = 1, this%jms
      do ir = 2, this%nd
        this%rtemp(ir,ijm) = this%rtemp(ir,ijm) + this%ab * this%ntemp(ijm,ir)
        this%rtorr(ir,ijm) = this%rtorr(ir,ijm) + this%ab * this%ntorr(ijm,ir)
        this%rsph1(ir,ijm) = this%rsph1(ir,ijm) + this%ab * this%nsph1(ijm,ir)
        this%rsph2(ir,ijm) = this%rsph2(ir,ijm) + this%ab * this%nsph2(ijm,ir)
      end do
    end do
    !$omp end parallel do
    
    call this%solve_temp_sub()
    call this%solve_torr_sub()
    call this%solve_mech_sub()
    
  end procedure time_scheme_ocean_sub
  
  module procedure time_scheme2_ocean_sub
    integer                        :: ir, is, ij, ijm
    real(kind=dbl)                 :: facPr, facRa, facEk, facj1, facj2
    complex(kind=dbl), allocatable :: v(:), curlv(:), T(:), gradT(:), nlm(:,:)
    
    facPr = 1 / this%Pr
    facRa = this%Ra / ( 1 - this%r_ud )**2
    facEk = 2 / this%Ek
    
    !$omp parallel private(v, curlv, T, gradT, nlm)
    !$omp do collapse (2)
    do ijm = 1, this%jms
      do ir = 2, this%nd
        this%rtemp(ir,ijm) = (1-this%ab) * this%ntemp(ijm,ir)
        this%rtorr(ir,ijm) = (1-this%ab) * this%ntorr(ijm,ir)
        this%rsph1(ir,ijm) = (1-this%ab) * this%nsph1(ijm,ir)
        this%rsph2(ir,ijm) = (1-this%ab) * this%nsph2(ijm,ir)
      end do
    end do
    !$omp end do
    
    allocate( v(this%jmv), curlv(this%jmv), T(this%jms), gradT(this%jmv), nlm(4,this%jms) )
    
    !$omp do private (ijm, ij, facj1, facj2)
    do ir = 2, this%nd
      !! Get vorticity and temperature gradient
      call this%curlv_rr_ijml_sub(ir, v, curlv)
      call this%gradT_rr_ijml_sub(ir, T, gradT, -1)
      
      !! Rescale curl(v) with Prandtl number and add ez for Coriolis force
      curlv(2) = curlv(2) * facPr + cs4pi * facEk
      
      do ijm = 3, this%jmv
        curlv(ijm) = curlv(ijm) * facPr
      end do
      
      !! Compute nonlinear terms
      call this%lat_grid%vcvv_vcvxv_sub(gradT, curlv, v, nlm)
      
      !! Add the buoyancy force with Newtonian gravity profile
      do ij = 1, this%jmax
        facj1 = -sqrt( (ij  ) / (2*ij+one) ) * facRa / this%rad_grid%rr(ir)**2
        facj2 = +sqrt( (ij+1) / (2*ij+one) ) * facRa / this%rad_grid%rr(ir)**2
        
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
    
    !$omp do collapse (2)
    do ijm = 1, this%jms
      do ir = 2, this%nd
        this%rtemp(ir,ijm) = this%rtemp(ir,ijm) + this%ab * this%ntemp(ijm,ir)
        this%rtorr(ir,ijm) = this%rtorr(ir,ijm) + this%ab * this%ntorr(ijm,ir)
        this%rsph1(ir,ijm) = this%rsph1(ir,ijm) + this%ab * this%nsph1(ijm,ir)
        this%rsph2(ir,ijm) = this%rsph2(ir,ijm) + this%ab * this%nsph2(ijm,ir)
      end do
    end do
    !$omp end do
    
    !$omp do private (ir,is,ij)
    do ijm = 1, this%jms
      ij = this%j_indx(ijm)
      
      do ir = 2, this%nd
        this%rtemp(ir,ijm) = this%rtemp(ir,ijm) + this%mat%temp(ij)%multipl_fn(2*(ir-1)+1, this%sol%temp(:,ijm))
      end do
      
      do ir = 1, this%nd
        is = 2*(ir-1)+1
        
        this%sol%temp(is  ,ijm) = this%rtemp(ir,ijm)
        this%sol%temp(is+1,ijm) = czero
      end do
      
      ir = this%nd+1
        this%sol%temp(2*this%nd+1,ijm) = this%rtemp(ir,ijm)
      
      call this%mat%temp(ij)%luSolve_sub( this%sol%temp(:,ijm) )
    end do
    !$omp end do
    
    !$omp do private (ir,is,ij)
    do ijm = 2, this%jms
      ij = this%j_indx(ijm)
      
      do ir = 2, this%nd
        this%rtorr(ir,ijm) = this%rtorr(ir,ijm) + this%mat%torr(ij)%multipl_fn(2*(ir-1)+1,this%sol%torr(:,ijm))
      end do
      
      do ir = 1, this%nd
        is = 2*(ir-1) + 1
        
        this%sol%torr(is  ,ijm) = this%rtorr(ir,ijm)
        this%sol%torr(is+1,ijm) = czero
      end do
      
      ir = this%nd+1
        this%sol%torr(2*this%nd+1,ijm) = this%rtorr(ir,ijm)
        
      call this%mat%torr(ij)%luSolve_sub( this%sol%torr(:,ijm) )
    end do
    !$omp end do
    
    !$omp do private (ir,is,ij)
    do ijm = 2, this%jms
      ij = this%j_indx(ijm)
      
      do ir = 2, this%nd
        this%rsph1(ir,ijm) = this%rsph1(ir,ijm) + this%mat%mech(ij)%multipl_fn(5*(ir-1)+1,this%sol%mech(:,ijm))
        this%rsph2(ir,ijm) = this%rsph2(ir,ijm) + this%mat%mech(ij)%multipl_fn(5*(ir-1)+2,this%sol%mech(:,ijm))
      end do
      
      do ir = 1, this%nd
        is = 5*(ir-1)+1
        
        this%sol%mech(is  ,ijm) = this%rsph1(ir,ijm)
        this%sol%mech(is+1,ijm) = this%rsph2(ir,ijm)
        this%sol%mech(is+2,ijm) = czero
        this%sol%mech(is+3,ijm) = czero
        this%sol%mech(is+4,ijm) = czero
      end do
        
      ir = this%nd+1
        this%sol%mech(5*this%nd+1,ijm) = this%rsph1(ir,ijm)
        this%sol%mech(5*this%nd+2,ijm) = this%rsph2(ir,ijm)
        
      call this%mat%mech(ij)%luSolve_sub( this%sol%mech(:,ijm) )
    end do
    !$omp end do
    !$omp end parallel
    
  end procedure time_scheme2_ocean_sub
  
end submodule timescheme
