submodule (sphsvt) vec_to_scal
  implicit none; contains
  
  module procedure vec2scal_jml_to_mj_sub
    integer                        :: j, m, l, lmj
    complex(kind=dbl), allocatable :: sum1(:), sum2(:), sum3(:)
    
    allocate( sum1(ncab), sum2(ncab), sum3(ncab) )
    
    m = 0
      do j = m, this%jmax2
        call zero_carray_sub( ncab, sum1(1) )
        call zero_carray_sub( ncab, sum2(1) )
        call zero_carray_sub( ncab, sum3(1) )
        
        do l = abs(j-1), min(this%jmax1, j+1)
          lmj = 3*(l*(l+1)/2+m+1)+j-l
          
          call copy3_carray_sub( ncab,               cleb1_fn(j,m,1, 0,l,m  ), cab(1,lmj-3), sum3 )
          call copy4_carray_sub( ncab, (-1)**(j+l) * cleb1_fn(j,m,1,-1,l,m-1), cab(1,lmj  ), sum1 )
          call copy3_carray_sub( ncab,               cleb1_fn(j,m,1,+1,l,m+1), cab(1,lmj  ), sum2 )
        end do
        
        call eee2xyz_sub( ncab, sum1, sum2, sum3, cc(ccpadding,m*this%jmax3-m*(m+1)/2+j+1) )
      end do
    
    do m = 1, this%jmax2
      do j = m, this%jmax2
        call zero_carray_sub( ncab, sum1(1) )
        call zero_carray_sub( ncab, sum2(1) )
        call zero_carray_sub( ncab, sum3(1) )
        
        do l = j-1, min(this%jmax1, j+1)
          lmj = 3*(l*(l+1)/2+m-1)+j-l
          
                         call copy3_carray_sub( ncab, cleb1_fn(j,m,1,-1,l,m-1), cab(1,lmj  ), sum1 )
          if ( l > m-1 ) call copy3_carray_sub( ncab, cleb1_fn(j,m,1, 0,l,m  ), cab(1,lmj+3), sum3 )
          if ( l > m   ) call copy3_carray_sub( ncab, cleb1_fn(j,m,1,+1,l,m+1), cab(1,lmj+6), sum2 )
        end do
        
        call eee2xyz_sub( ncab, sum1, sum2, sum3, cc(ccpadding,m*this%jmax3-m*(m+1)/2+j+1) )
      end do
    end do
    
    deallocate( sum1, sum2, sum3 )
    
  end procedure vec2scal_jml_to_mj_sub
  
end submodule vec_to_scal