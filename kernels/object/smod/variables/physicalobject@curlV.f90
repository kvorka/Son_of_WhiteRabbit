submodule (physicalobject) curlV
  implicit none; contains
  
  module procedure curlv_ptp_rr_jm_sub
    integer                        :: ij, ijm
    real(kind=dbl)                 :: cjr1, cjr2, cjr3, cjr4, crr
    complex(kind=dbl)              :: cj1, cj2
    complex(kind=dbl), allocatable :: dv(:,:)
    
    crr = 1 / this%rad_grid%rr(ir)
    
    allocate( dv(this%jms,3) )
    
    call this%dv_dr_ptp_rr_jm_sub( ir, v, dv )
    
    !ij = 0
      !im = 0
        curlv(1,1) = czero
        curlv(1,2) = czero
        curlv(1,3) = czero
    
    do ij = 1, this%jmax
      cj1 = sqrt( (ij+1) / (2*ij+one) ) * cunit
      cj2 = sqrt( (ij  ) / (2*ij+one) ) * cunit
      
      cjr1 = (ij-1) * crr
      cjr2 = (ij  ) * crr
      cjr3 = (ij+1) * crr
      cjr4 = (ij+2) * crr
      
      !$omp simd
      do ijm = jm(ij,0), jm(ij,ij)
        curlv(ijm,1) = cj1 * ( dv(ijm,2) + cjr3 * v(ijm,2) )
        curlv(ijm,2) = cj1 * ( dv(ijm,1) - cjr1 * v(ijm,1) ) + cj2 * ( dv(ijm,3) + cjr4 * v(ijm,3) )
        curlv(ijm,3) =                                         cj2 * ( dv(ijm,2) - cjr2 * v(ijm,2) )
      end do
    end do
    
    deallocate( dv )
    
  end procedure curlv_ptp_rr_jm_sub
  
end submodule curlV