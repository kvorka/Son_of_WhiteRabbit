submodule (physicalobject) gradT
  implicit none; contains
  
  module procedure gradT_rr_jml_sub
    integer                        :: ij, im, ijm, ijml
    real(kind=dbl)                 :: cj1, cj2, cjr1, cjr2
    complex(kind=dbl), allocatable :: dT_dr(:)
    
    allocate( dT_dr(this%jms) )
      
      call this%dT_dr_rr_jm_sub( ir, T, dT_dr )
      
      ij = 0
        im = 0
          gradT(1) = -sgn * dT_dr(1)
      
      do ij = 1, this%jmax
        cj1 = +sqrt( (ij  ) / (2*ij+one) ) * sgn
        cj2 = -sqrt( (ij+1) / (2*ij+one) ) * sgn
        
        cjr1 = +(ij+1) / this%rad_grid%rr(ir)
        cjr2 = -(ij  ) / this%rad_grid%rr(ir)
        
        do im = 0, ij
          ijm  = ij*(ij+1)/2+im+1
          ijml = 3*(ijm-1)-1
          
          gradT(ijml  ) = cj1 * ( dT_dr(ijm) + cjr1 * T(ijm) )
          gradT(ijml+1) = czero
          gradT(ijml+2) = cj2 * ( dT_dr(ijm) + cjr2 * T(ijm) )
        end do
      end do
      
    deallocate( dT_dr )
    
  end procedure gradT_rr_jml_sub
  
end submodule gradT