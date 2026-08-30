submodule (sphsvt) vec_to_scal
  implicit none; contains
  
  module procedure vec2scal_jm_to_mj_sub
    integer                        :: ij, im, il, ilm, ilj
    complex(kind=dbl), allocatable :: sumPTP(:,:)
    
    allocate( sumPTP(nca,3) )
    
    im = 0
      do ij = im, this%jmax1
        call zero_carray_sub( 3*nca, sumPTP )
        
        do il = abs(ij-1), min(this%jmax, ij+1)
          ilm = jm(il,im+1)
          ilj = ij-il+2
          
          call cadj3_carray_sub( nca, cleb1_fn(ij,im,1,-1,il,im-1) * (-1)**(ij+il), ca(1,ilj,ilm  ), sumPTP(1,1) )
          call copy3_carray_sub( nca, cleb1_fn(ij,im,1, 0,il,im  ),                 ca(1,ilj,ilm-1), sumPTP(1,2) )
          call copy3_carray_sub( nca, cleb1_fn(ij,im,1,+1,il,im+1),                 ca(1,ilj,ilm  ), sumPTP(1,3) )
        end do
        
        call eee2xyz_sub( nca, sumPTP, cc(1,im*this%jmax2-im*(im+1)/2+ij+1) )
      end do
    
    do im = 1, this%jmax1
      do ij = im, this%jmax1
        call zero_carray_sub( 3*nca, sumPTP )
        
        do il = ij-1, min(this%jmax, ij+1)
          ilm = jm(il,im-1)
          ilj = ij-il+2
          
          if ( il > im ) then
            
            call copy3_carray_sub( nca, cleb1_fn(ij,im,1,-1,il,im-1), ca(1,ilj,ilm  ), sumPTP(1,1) )
            call copy3_carray_sub( nca, cleb1_fn(ij,im,1, 0,il,im  ), ca(1,ilj,ilm+1), sumPTP(1,2) )
            call copy3_carray_sub( nca, cleb1_fn(ij,im,1,+1,il,im+1), ca(1,ilj,ilm+2), sumPTP(1,3) )
            
          else if ( il > im-1 ) then
            
            call copy3_carray_sub( nca, cleb1_fn(ij,im,1,-1,il,im-1), ca(1,ilj,ilm  ), sumPTP(1,1) )
            call copy3_carray_sub( nca, cleb1_fn(ij,im,1, 0,il,im  ), ca(1,ilj,ilm+1), sumPTP(1,2) )
            
          else
            
            call copy3_carray_sub( nca, cleb1_fn(ij,im,1,-1,il,im-1), ca(1,ilj,ilm  ), sumPTP(1,1) )
            
          end if
        end do
        
        call eee2xyz_sub( nca, sumPTP, cc(1,im*this%jmax2-im*(im+1)/2+ij+1) )
      end do
    end do
    
    deallocate( sumPTP )
    
  end procedure vec2scal_jm_to_mj_sub
  
end submodule vec_to_scal