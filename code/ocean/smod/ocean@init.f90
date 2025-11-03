submodule (ocean) init
  implicit none; contains
  
  module procedure init_ocean_sub
    
    call this%init_objects_sub( nd = nd_ocean, jmax = jmax_ocean, r_ud = r_ud_ocean )
    
    this%n_iter = n_iter_ocean
    this%cf     = 0.6_dbl
    this%ab     = 1.5_dbl
    
    this%Pr = Pr_ocean
    this%Ra = Ra_ocean
    this%Ek = Ek_ocean
    
    this%facPr = 1 / this%Pr
    this%facRa = this%Ra / ( 1 - this%r_ud )**2
    this%facEk = 2 / this%Ek
    
    this%diffusion_type = diffusion_ocean
    this%thermal_bnd    = therm_bnd_ocean
    
    if ( .not. present(speed) ) then
      open(unit=11, file='data/Nuss.dat', status='new', action='write')
      open(unit=12, file='data/Laws.dat', status='new', action='write')
    end if
    
    !! Initialize the equations
    call this%init_eq_temp_sub()
    call this%init_eq_torr_sub()
    call this%init_eq_mech_sub()
    
    !! Initialize the matrices
    call this%prepare_mat_temp_sub()
    call this%prepare_mat_torr_sub()
    call this%prepare_mat_mech_sub()
    
    !! Initialize the non-linear terms
    allocate( this%ntemp(this%jms,2:this%nd) ); this%ntemp = czero
    allocate( this%ntorr(this%jms,2:this%nd) ); this%ntorr = czero
    allocate( this%nsph1(this%jms,2:this%nd) ); this%nsph1 = czero
    allocate( this%nsph2(this%jms,2:this%nd) ); this%nsph2 = czero
    
    !! Set the thermal bottom boundary condition
    call this%init_temp_bbnd_sub()
    
    !! Initialize the state
    call this%init_state_sub()
    
  end procedure init_ocean_sub
  
  module procedure deallocate_ocean_sub
    
    deallocate( this%nsph1 )
    deallocate( this%nsph2 )
    deallocate( this%ntorr )
    deallocate( this%ntemp )
    
    close(11)
    close(12)
    
    call this%deallocate_objects_sub()

  end procedure deallocate_ocean_sub
  
end submodule init