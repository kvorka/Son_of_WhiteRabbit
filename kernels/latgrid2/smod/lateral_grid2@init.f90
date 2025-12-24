submodule (lateral_grid2) init
  implicit none; contains
  
  module procedure init_shtns_sub
    
    this%jmax = jmax
    this%jms  = jm(jmax,jmax)
    
    this%nL = (3*jmax/2+1)+8-mod(3*jmax/2+1,8)
    this%nF = 3*(jmax+3)
    
    this%shtns_c = shtns_create( jmax, jmax, 1, SHT_ORTHONORMAL )
    call shtns_set_grid( this%shtns_c, SHT_GAUSS+SHT_NATIVE_LAYOUT, 1.0e-10_dbl, this%nL, this%nF )
    
    call this%rxd%init_sub( jmax )
    
  end procedure init_shtns_sub
  
end submodule init