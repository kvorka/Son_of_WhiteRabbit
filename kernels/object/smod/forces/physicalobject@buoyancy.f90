submodule (physicalobject) buoyancy
  implicit none; contains
  
  module procedure buoy_rr_jml_sub
    integer        :: ij, ij0
    real(kind=dbl) :: facrr
    
    facrr = this%facRa / this%rad_grid%rr(ir)**2
    
    do ij = 1, this%jmax
      ij0 = jm(ij,0)
      
      call copy3_carray_sub( ij+1, -sqrt( (ij  ) / (2*ij+one) ) * facrr, T(ij0), nsph1(ij0) )
      call copy3_carray_sub( ij+1, +sqrt( (ij+1) / (2*ij+one) ) * facrr, T(ij0), nsph2(ij0) )
    end do
      
  end procedure buoy_rr_jml_sub
  
end submodule buoyancy