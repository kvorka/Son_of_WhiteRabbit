submodule (physicalobject) buoyancy
  implicit none; contains
  
  module procedure buoy_rr_jml_sub
    integer        :: ij, ijm
    real(kind=dbl) :: fac, fac1, fac2
    
    !!TODO: now only Newtonian profile
    fac = this%Ra / this%rad_grid%rr(ir)**2 / ( 1 - this%r_ud )**2
    
    do ij = 1, this%jmax
      fac1 = -sqrt( (ij  ) / (2*ij+one) ) * fac
      fac2 = +sqrt( (ij+1) / (2*ij+one) ) * fac
      
      !$omp simd
      do ijm = ij*(ij+1)/2+1, ij*(ij+1)/2+ij+1
        force(1,ijm) = fac1 * T(ijm)
        force(2,ijm) = fac2 * T(ijm)
      end do
    end do
      
  end procedure buoy_rr_jml_sub
  
end submodule buoyancy