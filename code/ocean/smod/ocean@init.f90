submodule (ocean) init
  implicit none; contains
  
  module procedure init_ocean_sub
    integer :: ir, ij, ik
    
    call this%init_objects_sub( nd = nd_ocean, jmax = jmax_ocean, r_ud = r_ud_ocean )
    
    this%cf     = 0.6_dbl
    this%ab     = 1.5_dbl
    
    this%Pr = Pr_ocean
    this%Ra = Ra_ocean
    this%Ek = Ek_ocean
    
    this%diffusion_type = diffusion_ocean
    this%thermal_bnd    = therm_bnd_ocean
    
    !! Prepare Nusselt and Energy saving check files if needed
    if ( .not. speed ) then
      open(unit=11, file='data/Nuss.dat', status='new', action='write')
      open(unit=12, file='data/Laws.dat', status='new', action='write')
    end if
    
    !! Initialize the equations: right-hand sides, matrices, arrays for solutions
    allocate( this%temp(0:this%jmax) )
    allocate( this%torr(0:this%jmax) )
    allocate( this%mech(0:this%jmax) )
    
    !$omp parallel do private (ij)
    do ik = 0, (this%jmax-1)/2
      ij = ik
        call this%temp(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%torr(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%mech(ij)%init_sub( mm=ij, nvar=5*this%nd+2, nrhs=this%nd+1, ld=9, lu=8, def_rhs2=.true.  )
      
      ij = this%jmax-ik
        call this%temp(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%torr(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%mech(ij)%init_sub( mm=ij, nvar=5*this%nd+2, nrhs=this%nd+1, ld=9, lu=8, def_rhs2=.true.  )
    end do
    !$omp end parallel do
    
    call this%prepare_mat_temp_sub()
    call this%prepare_mat_torr_sub()
    call this%prepare_mat_mech_sub()
    
    !! Initialize the non-linear terms
    allocate( this%ntemp(this%jms,2:this%nd) )
    allocate( this%ntorr(this%jms,2:this%nd) )
    allocate( this%nsph1(this%jms,2:this%nd) )
    allocate( this%nsph2(this%jms,2:this%nd) )
    
    !$omp parallel do
    do ir = 2, this%nd
      call zero_carray_sub( this%jms, this%ntemp(1,ir) )
      call zero_carray_sub( this%jms, this%ntorr(1,ir) )
      call zero_carray_sub( this%jms, this%nsph1(1,ir) )
      call zero_carray_sub( this%jms, this%nsph2(1,ir) )
    end do
    !$omp end parallel do
    
    !! Set the thermal bottom boundary condition and compute
    !! the initial state of the ocean
    call init_temp_bbnd_ocean_sub(this)
    call init_state_ocean_sub(this)
    
  end procedure init_ocean_sub
  
  module procedure deallocate_ocean_sub
    
    close(11)
    close(12)
    
    call this%deallocate_objects_sub()
    
  end procedure deallocate_ocean_sub
  
end submodule init