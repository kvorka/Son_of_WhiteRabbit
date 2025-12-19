submodule (physicalobject) curlV
  implicit none; contains
  
  module procedure curlv_rr_jml_sub
    integer                        :: ij, im, ijml
    real(kind=dbl)                 :: cjr1, cjr2, cjr3, cjr4, crr
    complex(kind=dbl)              :: cj1, cj2
    complex(kind=dbl), allocatable :: dv(:)
    
    crr = 1 / this%rad_grid%rr(ir)
    
    allocate( dv(this%jmv) )
    
    call this%dv_dr_rr_jml_sub( ir, v, dv )
    
    !ij = 0
      !im = 0
        curlv(1) = czero
    
    do ij = 1, this%jmax
      cj1 = sqrt( (ij+1) / (2*ij+one) ) * cunit
      cj2 = sqrt( (ij  ) / (2*ij+one) ) * cunit
      
      cjr1 = (ij-1) * crr
      cjr2 = (ij  ) * crr
      cjr3 = (ij+1) * crr
      cjr4 = (ij+2) * crr
      
      do im = 0, ij
        ijml = 3*(ij*(ij+1)/2+im)-1
        
        curlv(ijml  ) = cj1 * ( dv(ijml+1) + cjr3 * v(ijml+1) )
        curlv(ijml+1) = cj1 * ( dv(ijml  ) - cjr1 * v(ijml  ) ) + cj2 * ( dv(ijml+2) + cjr4 * v(ijml+2) )
        curlv(ijml+2) =                                           cj2 * ( dv(ijml+1) - cjr2 * v(ijml+1) )
      end do
    end do
    
    deallocate( dv )
    
  end procedure curlv_rr_jml_sub
  
end submodule curlV