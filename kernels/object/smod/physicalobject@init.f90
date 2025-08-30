submodule (physicalobject) init
  implicit none ; contains
  
  module procedure init_objects_sub
    integer :: j, m
    
    this%nd   = nd
    this%jmax = jmax
    this%r_ud = r_ud
    
    this%jms  =     ( this%jmax * ( this%jmax+1 ) / 2 + this%jmax ) + 1
    this%jmv  = 3 * ( this%jmax * ( this%jmax+1 ) / 2 + this%jmax ) + 1
    
    call this%rad_grid%init_sub(this%nd, r_ud/(1-r_ud), 1/(1-r_ud))
    call this%lat_grid%init_sub(this%jmax)
    call this%sol%init_sub(this%nd, this%jmax)
    call this%mat%init_sub(this%nd, this%jmax)
    
    allocate( this%j_indx(this%jms) )
      do j = 0, this%jmax
        do m = 0, j
          this%j_indx(j*(j+1)/2+m+1) = j
        end do
      end do
    
    this%poc = 0
    this%t   = zero
    
    this%dt  = 0.49_dbl * ( this%rad_grid%r(2)-this%rad_grid%r(1) )**2
    
  end procedure init_objects_sub
  
  module procedure deallocate_objects_sub
    
    if ( allocated(this%j_indx)  ) deallocate( this%j_indx )
    
    if ( allocated(this%rsph1) ) deallocate( this%rsph1 )
    if ( allocated(this%rsph2) ) deallocate( this%rsph2 )
    if ( allocated(this%rtorr) ) deallocate( this%rtorr )
    if ( allocated(this%rtemp) ) deallocate( this%rtemp )
    
    call this%sol%deallocate_sub()
    call this%mat%deallocate_sub()
    call this%rad_grid%deallocate_sub()
    call this%lat_grid%deallocate_sub()
    
  end procedure deallocate_objects_sub
  
end submodule init