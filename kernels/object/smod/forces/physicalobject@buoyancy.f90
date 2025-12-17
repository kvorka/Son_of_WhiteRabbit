submodule (physicalobject) buoyancy
  implicit none; contains
  
  module procedure buoy_rr_jml_sub
    integer        :: ij, ijm
    real(kind=dbl) :: facrr, facj1, facj2
    
    facrr = this%facRa / this%rad_grid%rr(ir)**2
    
    do ij = 1, this%jmax
      facj1 = -sqrt( (ij  ) / (2*ij+one) ) * facrr
      facj2 = +sqrt( (ij+1) / (2*ij+one) ) * facrr
      
      do concurrent ( ijm = jm(ij,0):jm(ij,ij) )
        nsph1(ijm) = nsph1(ijm) + facj1 * T(ijm)
        nsph2(ijm) = nsph2(ijm) + facj2 * T(ijm)
      end do
    end do
      
  end procedure buoy_rr_jml_sub
  
end submodule buoyancy