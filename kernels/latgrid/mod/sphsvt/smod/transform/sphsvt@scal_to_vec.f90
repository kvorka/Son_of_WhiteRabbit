submodule (sphsvt) scal_to_vec
  implicit none; contains
  
  module procedure scal2vec_mj_to_jm_sub
    integer           :: ij, im, imj, imj1, imj2, ijm
    complex(kind=dbl) :: cr12
    
    !$omp simd
    do imj = 1, this%jms1
      cr12                = ( +cr(crpadding,imj) + cr(crpadding+1,imj) * cunit ) * sq2_1
      cr(crpadding+1,imj) = ( -cr(crpadding,imj) + cr(crpadding+1,imj) * cunit ) * sq2_1
      cr(crpadding  ,imj) = cr12
    end do
      
    ij = 0
      im = 0
        ijm  = 1
        imj  = mj( this%jmax1, im  , ij   )
        imj2 = mj( this%jmax1, im+1, ij+1 )
        
        cjm1(ijm) = czero
        cjm2(ijm) = czero
        cjm3(ijm) =        cr(crpadding  ,imj2 )   * cleb1_fn(ij+1,im+1,1,-1,ij,im) + &
                  &        cr(crpadding+2,imj+1)   * cleb1_fn(ij+1,im+0,1, 0,ij,im) + &
                  & conjg( cr(crpadding  ,imj2 ) ) * cleb1_fn(ij+1,im-1,1,+1,ij,im); cjm3(ijm)%im = zero
        
    do ij = 1, this%jmax
      im = 0
        ijm  = ijm+1
        imj  = mj( this%jmax1, im  , ij )
        imj2 = mj( this%jmax1, im+1, ij )
        
        cjm1(ijm) =        cr(crpadding  ,imj2-1)   * cleb1_fn(ij-1,im+1,1,-1,ij,im) + &
                  &        cr(crpadding+2,imj -1)   * cleb1_fn(ij-1,im+0,1, 0,ij,im) + &
                  & conjg( cr(crpadding  ,imj2-1) ) * cleb1_fn(ij-1,im-1,1,+1,ij,im) ; cjm1(ijm)%im = zero
        cjm2(ijm) =        cr(crpadding  ,imj2  )   * cleb1_fn(ij  ,im+1,1,-1,ij,im) + &
                  &        cr(crpadding+2,imj   )   * cleb1_fn(ij  ,im+0,1, 0,ij,im) + &
                  & conjg( cr(crpadding  ,imj2  ) ) * cleb1_fn(ij  ,im-1,1,+1,ij,im) ; cjm2(ijm)%re = zero
        cjm3(ijm) =        cr(crpadding  ,imj2+1)   * cleb1_fn(ij+1,im+1,1,-1,ij,im) + &
                  &        cr(crpadding+2,imj +1)   * cleb1_fn(ij+1,im+0,1, 0,ij,im) + &
                  & conjg( cr(crpadding  ,imj2+1) ) * cleb1_fn(ij+1,im-1,1,+1,ij,im) ; cjm3(ijm)%im = zero
      
      do im = 1, ij
        ijm  = ijm+1
        imj1 = mj( this%jmax1, im-1, ij )
        imj  = mj( this%jmax1, im  , ij )
        imj2 = mj( this%jmax1, im+1, ij )
        
        cjm1(ijm) = cr(crpadding  ,imj2-1) * cleb1_fn(ij-1,im+1,1,-1,ij,im) + &
                  & cr(crpadding+2,imj -1) * cleb1_fn(ij-1,im+0,1, 0,ij,im) + &
                  & cr(crpadding+1,imj1-1) * cleb1_fn(ij-1,im-1,1,+1,ij,im)
        cjm2(ijm) = cr(crpadding  ,imj2  ) * cleb1_fn(ij  ,im+1,1,-1,ij,im) + &
                  & cr(crpadding+2,imj   ) * cleb1_fn(ij  ,im+0,1, 0,ij,im) + &
                  & cr(crpadding+1,imj1  ) * cleb1_fn(ij  ,im-1,1,+1,ij,im)
        cjm3(ijm) = cr(crpadding  ,imj2+1) * cleb1_fn(ij+1,im+1,1,-1,ij,im) + &
                  & cr(crpadding+2,imj +1) * cleb1_fn(ij+1,im+0,1, 0,ij,im) + &
                  & cr(crpadding+1,imj1+1) * cleb1_fn(ij+1,im-1,1,+1,ij,im)
      end do
    end do
    
  end procedure scal2vec_mj_to_jm_sub
  
end submodule scal_to_vec