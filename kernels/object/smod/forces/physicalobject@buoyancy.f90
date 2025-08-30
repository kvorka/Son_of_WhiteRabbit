submodule (physicalobject) buoyancy
  implicit none; contains
  
  module procedure buoy_rr_jml_sub
    integer        :: ijm, ij
    real(kind=dbl) :: fac, fac1, fac2
    
    !!TODO: now only Newtonian profile
    fac = this%Ra * ( 1 / this%rad_grid%rr(ir)**2 / ( 1 - this%r_ud**2 ) )
    
    do ij = 1, this%jmax
      fac1 = -sqrt( (ij  ) / (2*ij+one) ) * fac
      fac2 = +sqrt( (ij+1) / (2*ij+one) ) * fac
      
      do concurrent ( ijm = jm(ij,0):jm(ij,ij) )
        force(1,ijm) = fac1 * T(ijm)
        force(2,ijm) = fac2 * T(ijm)
      end do
    end do
      
  end procedure buoy_rr_jml_sub
  
end submodule buoyancy