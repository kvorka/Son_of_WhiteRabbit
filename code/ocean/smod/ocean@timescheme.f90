submodule (ocean) timescheme
  implicit none; contains
  
  module procedure time_scheme_ocean_sub
    integer                        :: ik, ir, ij, ij0
    real(kind=dbl)                 :: grr
    complex(kind=dbl), allocatable :: v(:), curlv(:), T(:), gradT(:)
    
    this%t = this%t + this%dt
    
    !$omp parallel private (v, curlv, T, gradT)
    
    !$omp do private (ir,ij0) schedule (guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        call copy2_carray_sub( ij+1, 1-this%ab, this%ntemp(ij0,ir), this%temp(ij)%rhs1(0,ir) )
        call copy2_carray_sub( ij+1, 1-this%ab, this%ntorr(ij0,ir), this%torr(ij)%rhs1(0,ir) )
        call copy2_carray_sub( ij+1, 1-this%ab, this%nsph1(ij0,ir), this%mech(ij)%rhs1(0,ir) )
        call copy2_carray_sub( ij+1, 1-this%ab, this%nsph2(ij0,ir), this%mech(ij)%rhs2(0,ir) )
      end do
    end do
    !$omp end do
    
    allocate( v(3*this%jms), curlv(3*this%jms), T(this%jms), gradT(3*this%jms) )
    
    !$omp do private (grr)
    do ir = 2, this%nd
      !! Get vorticity and temperature gradient with scaling factors
      call this%curlv_ptp_rr_jm_sub( ir, v, curlv, 1/this%Pr )
      call this%gradT_ptp_rr_jm_sub( ir, T, gradT, -one )
      
      !! Add ez for Coriolis force
      curlv(2)%re = curlv(2)%re + s4pi * ( 2 / this%Ek )
      
      !! Compute nonlinear terms
      call this%lat_grid%scvv_vcvxv_sub( v1   = v,                &
                                         v2   = gradT,            &
                                         v3   = curlv,            &
                                         scal = this%ntemp(1,ir), &
                                         pol1 = this%nsph1(1,ir), &
                                         torr = this%ntorr(1,ir), &
                                         pol2 = this%nsph2(1,ir)  )
      
      !! Add the thermal buoyancy force with Newtonian gravity profile
      grr = this%Ra / ( 1 - this%r_ud )**2 / this%rad_grid%rr(ir)**2
      
      call this%buoy_rr_jml_sub( fac  = grr,              &
                                 src  = T,                &
                                 pol1 = this%nsph1(1,ir), &
                                 pol2 = this%nsph2(1,ir)  )
    end do
    !$omp end do
    
    deallocate( v, curlv, T, gradT )
    
    !$omp do private (ir,ij0) schedule (guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        call copy3_carray_sub( ij+1, this%ab, this%ntemp(ij0,ir), this%temp(ij)%rhs1(0,ir) )
        call copy3_carray_sub( ij+1, this%ab, this%ntorr(ij0,ir), this%torr(ij)%rhs1(0,ir) )
        call copy3_carray_sub( ij+1, this%ab, this%nsph1(ij0,ir), this%mech(ij)%rhs1(0,ir) )
        call copy3_carray_sub( ij+1, this%ab, this%nsph2(ij0,ir), this%mech(ij)%rhs2(0,ir) )
      end do
    end do
    !$omp end do
    
    !$omp do
    do ik = 0, (this%jmax-1)/2
      call this%solve_temp_ij_sub(ik)
      call this%solve_temp_ij_sub(this%jmax-ik)
      
      call this%solve_torr_ij_sub(ik)
      call this%solve_torr_ij_sub(this%jmax-ik)
      
      call this%solve_mech_ij_sub(ik)
      call this%solve_mech_ij_sub(this%jmax-ik)
    end do
    !$omp end do
    
    !$omp end parallel
    
  end procedure time_scheme_ocean_sub
  
end submodule timescheme
