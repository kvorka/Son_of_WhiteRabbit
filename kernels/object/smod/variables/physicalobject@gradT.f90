submodule (physicalobject) gradT
  implicit none; contains
  
  module procedure gradT_ptp_rr_jm_sub
    integer                        :: ij, ijm
    real(kind=dbl)                 :: cj1, cj2, cjr1, cjr2
    complex(kind=dbl), allocatable :: dT_dr(:)
    
    allocate( dT_dr(this%jms) )
      
      call this%dT_dr_rr_jm_sub( ir, T, dT_dr )
      
      !ij = 0
      !  im = 0
          gradT(1,1) = czero
          gradT(1,2) = czero
          gradT(1,3) = -sgn * dT_dr(1)
      
      do ij = 1, this%jmax
        cj1 = +sqrt( (ij  ) / (2*ij+one) ) * sgn
        cj2 = -sqrt( (ij+1) / (2*ij+one) ) * sgn
        
        cjr1 = +(ij+1) / this%rad_grid%rr(ir)
        cjr2 = -(ij  ) / this%rad_grid%rr(ir)
        
        !$omp simd
        do ijm = jm(ij,0), jm(ij,ij)
          gradT(ijm,1) = cj1 * ( dT_dr(ijm) + cjr1 * T(ijm) )
          gradT(ijm,2) = czero
          gradT(ijm,3) = cj2 * ( dT_dr(ijm) + cjr2 * T(ijm) )
        end do
      end do
      
    deallocate( dT_dr )
    
  end procedure gradT_ptp_rr_jm_sub
  
end submodule gradT