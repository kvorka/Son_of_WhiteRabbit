submodule (physicalobject) buoyancy
  implicit none; contains
  
  module procedure buoy_rr_jml_sub
    integer :: ij, ij0
    
    !! poloidal forcing + er * source
    do ij = 1, this%jmax
      ij0 = jm(ij,0)
      
      call copy3_carray_sub( ij+1, -sqrt( (ij  ) / (2*ij+one) ) * fac, src(ij0), pol1(ij0) )
      call copy3_carray_sub( ij+1, +sqrt( (ij+1) / (2*ij+one) ) * fac, src(ij0), pol2(ij0) )
    end do
    
  end procedure buoy_rr_jml_sub
  
end submodule buoyancy