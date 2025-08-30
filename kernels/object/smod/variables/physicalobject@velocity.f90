submodule (physicalobject) velocity
  implicit none; contains
  
  module procedure v_rr_fn
    
    v_rr_fn = this%sol%velocity_fn(ir,il,ijm)
    
  end procedure v_rr_fn
  
  module procedure v_rr_ijml_sub
    
    call this%sol%velocity_jml_sub( ir, v_rr_ijml )
    
  end procedure v_rr_ijml_sub
  
  module procedure dv_dr_rr_ijml_sub
    integer                        :: ijml
    real(kind=dbl)                 :: fac1, fac2, fac3
    complex(kind=dbl), allocatable :: v3(:)
    
    fac1 = this%rad_grid%drr(ir,-1)
    fac2 = this%rad_grid%drr(ir, 0)
    fac3 = this%rad_grid%drr(ir,+1)
    
    allocate( v3(this%jmv) )
      
      call this%sol%velocity_jml_sub( ir-1, dv )
      call this%sol%velocity_jml_sub( ir  , v  )
      call this%sol%velocity_jml_sub( ir+1, v3 )

      do concurrent ( ijml = 1:this%jmv )
        dv(ijml) = fac1 * dv(ijml) + fac2 * v(ijml) + fac3 * v3(ijml)
      end do
      
    deallocate( v3 )
    
  end procedure dv_dr_rr_ijml_sub
  
  module procedure curlv_rr_ijml_sub
    integer                        :: ij, im, ijml
    real(kind=dbl)                 :: cjr1, cjr2, cjr3, cjr4, crr
    complex(kind=dbl)              :: cj1, cj2
    complex(kind=dbl), allocatable :: dv(:)
    
    crr = 1 / this%rad_grid%rr(ir)
    
    allocate( dv(this%jmv) )
    
    call this%dv_dr_rr_ijml_sub( ir, v, dv )
    
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
      
      do im = 0, this%jmax
        ijml = 3*(ij*(ij+1)/2+im)-1
        
        curlv(ijml  ) = cj1 * ( dv(ijml+1) + cjr3 * v(ijml+1) )
        curlv(ijml+1) = cj1 * ( dv(ijml  ) - cjr1 * v(ijml  ) ) + cj2 * ( dv(ijml+2) + cjr4 * v(ijml+2) )
        curlv(ijml+2) =                                           cj2 * ( dv(ijml+1) - cjr2 * v(ijml+1) )
      end do
    end do
    
    deallocate( dv )
    
  end procedure curlv_rr_ijml_sub
  
end submodule velocity