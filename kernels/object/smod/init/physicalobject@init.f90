submodule (physicalobject) init
  implicit none ; contains
  
  module procedure init_objects_sub
    
    this%nd   = nd
    this%jmax = jmax
    this%r_ud = r_ud
    
    this%jms  =     ( this%jmax * ( this%jmax+1 ) / 2 + this%jmax ) + 1
    this%jmv  = 3 * ( this%jmax * ( this%jmax+1 ) / 2 + this%jmax ) + 1
    
    call this%rad_grid%init_sub(this%nd, r_ud/(1-r_ud), 1/(1-r_ud))
    call this%lat_grid%init_sub(this%jmax)
    
    this%poc = 0
    this%t   = zero
    
    this%dt  = 0.49_dbl * ( this%rad_grid%r(2)-this%rad_grid%r(1) )**2
    
  end procedure init_objects_sub
  
  module procedure deallocate_objects_sub
    
    if ( allocated( this%ntemp ) ) deallocate( this%ntemp )
    if ( allocated( this%ntorr ) ) deallocate( this%ntorr )
    if ( allocated( this%nsph1 ) ) deallocate( this%nsph1 )
    if ( allocated( this%nsph2 ) ) deallocate( this%nsph2 )
    
    call this%deallocEqs_sub( this%temp )
    call this%deallocEqs_sub( this%torr )
    call this%deallocEqs_sub( this%mech )
    
    call this%rad_grid%deallocate_sub()
    call this%lat_grid%deallocate_sub()
    
  end procedure deallocate_objects_sub
  
end submodule init