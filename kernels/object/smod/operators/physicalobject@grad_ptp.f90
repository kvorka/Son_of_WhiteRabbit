submodule (physicalobject) grad_ptp
  implicit none; contains
  
  module procedure grad_ptp_sub
    integer        :: ij, ij0
    real(kind=dbl) :: cj1, cj2, cjr1, cjr2
    
    !ij = 0
    !  im = 0
        gradT(1,1) = czero
        gradT(1,3) = -sgn * dT_dr(1)
    
    do ij = 1, this%jmax
      cj1 = +sqrt( ( ij   ) / ( 2*ij + one ) ) * sgn
      cj2 = -sqrt( ( ij+1 ) / ( 2*ij + one ) ) * sgn
      
      cjr1 = +(ij+1) / this%rad_grid%rr(ir)
      cjr2 = -(ij  ) / this%rad_grid%rr(ir)
      
      ij0 = jm(ij,0)
      
      call grad_pp_j_sub( ij+1, cj1, cjr1, cj2, cjr2, dT_dr(ij0), T(ij0), gradT(ij0,1), gradT(ij0,3) )
    end do
    
    call zero_carray_sub( this%jms, gradT(1,2) )
    
  end procedure grad_ptp_sub
  
end submodule grad_ptp