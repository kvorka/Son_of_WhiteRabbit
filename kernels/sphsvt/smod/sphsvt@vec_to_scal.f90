submodule (sphsvt) vec_to_scal
  implicit none; contains
  
  module procedure vec2scal_jm_to_mj_sub
    integer                        :: ij, im, il, ilm, ilj
    real(kind=dbl)                 :: cg
    complex(kind=dbl), allocatable :: sum1(:), sum2(:), sum3(:)
    
    allocate( sum1(3), sum2(3), sum3(3) )
    
    im = 0
      do ij = im, this%jmax1
        call zero_carray_sub( 3, sum1(1) )
        call zero_carray_sub( 3, sum2(1) )
        call zero_carray_sub( 3, sum3(1) )
        
        do il = abs(ij-1), min(this%jmax, ij+1)
          ilm = jm(il,im+1)
          ilj = ij-il+2
          
          cg = cleb1_fn(ij,im,1,0,il,im)
            sum3(1) = sum3(1) + cg * v1(ilm-1,ilj)
            sum3(2) = sum3(2) + cg * v2(ilm-1,ilj)
            sum3(3) = sum3(3) + cg * v3(ilm-1,ilj)
          
          cg = (-1)**(ij+il) * cleb1_fn(ij,im,1,-1,il,im-1)
            sum1(1) = sum1(1) + cg * conjg( v1(ilm,ilj) )
            sum1(2) = sum1(2) + cg * conjg( v2(ilm,ilj) )
            sum1(3) = sum1(3) + cg * conjg( v3(ilm,ilj) )
          
          cg = cleb1_fn(ij,im,1,+1,il,im+1)
            sum2(1) = sum2(1) + cg * v1(ilm,ilj)
            sum2(2) = sum2(2) + cg * v2(ilm,ilj)
            sum2(3) = sum2(3) + cg * v3(ilm,ilj)
        end do
        
        call eee2xyz_sub( 3, sum1, sum2, sum3, cc(1,im*this%jmax2-im*(im+1)/2+ij+1) )
      end do
    
    do im = 1, this%jmax1
      do ij = im, this%jmax1
        call zero_carray_sub( 3, sum1(1) )
        call zero_carray_sub( 3, sum2(1) )
        call zero_carray_sub( 3, sum3(1) )
        
        do il = ij-1, min(this%jmax, ij+1)
          ilm = jm(il,im-1)
          ilj = ij-il+2
          
          cg = cleb1_fn(ij,im,1,-1,il,im-1)
            sum1(1) = sum1(1) + cg * v1(ilm,ilj)
            sum1(2) = sum1(2) + cg * v2(ilm,ilj)
            sum1(3) = sum1(3) + cg * v3(ilm,ilj)
          
          if ( il > im-1 ) then
            cg = cleb1_fn(ij,im,1,0,il,im)
              sum3(1) = sum3(1) + cg * v1(ilm+1,ilj)
              sum3(2) = sum3(2) + cg * v2(ilm+1,ilj)
              sum3(3) = sum3(3) + cg * v3(ilm+1,ilj)
          end if
          
          if ( il > im ) then
            cg = cleb1_fn(ij,im,1,+1,il,im+1)
              sum2(1) = sum2(1) + cg * v1(ilm+2,ilj)
              sum2(2) = sum2(2) + cg * v2(ilm+2,ilj)
              sum2(3) = sum2(3) + cg * v3(ilm+2,ilj)
          end if
        end do
        
        call eee2xyz_sub( 3, sum1, sum2, sum3, cc(1,im*this%jmax2-im*(im+1)/2+ij+1) )
      end do
    end do
    
    deallocate( sum1, sum2, sum3 )
    
  end procedure vec2scal_jm_to_mj_sub
  
end submodule vec_to_scal