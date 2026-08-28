submodule (sphsvt) scal_to_vec
  implicit none; contains
  
  module procedure scal2vec_mj_to_jm_sub
    integer        :: ij, im, imj0, imj1, imj2, ijm
    real(kind=dbl) :: c1, c2, c3, c4, c5, c6, c7, c8, c9
    
    call xy2ee_sub( this%jms1, cr(1,1), cr(1,2) )
    
    ij = 0
      im = 0
        ijm  = 1
        imj1 = mj( this%jmax1, im  , ij+1 )
        imj2 = mj( this%jmax1, im+1, ij+1 )
        
        c7 = cleb1_fn(ij+1,im+1,1,-1,ij,im)
        c8 = cleb1_fn(ij+1,im+0,1, 0,ij,im)
        c9 = cleb1_fn(ij+1,im-1,1,+1,ij,im)
        
        cjm1(ijm) = czero
        cjm2(ijm) = czero
        cjm3(ijm) = cr(imj2,1) * c7 + cr(imj1,2) * c8 + conjg( cr(imj2,1) ) * c9
        
        cjm1(ijm)%im = zero
        cjm2(ijm)%im = zero
        cjm3(ijm)%im = zero
    
    do ij = 1, this%jmax
      im = 0
        ijm  = ijm+1
        imj0 = mj( this%jmax1, im  , ij )
        imj2 = mj( this%jmax1, im+1, ij )
        
        c1 = cleb1_fn(ij-1,im+1,1,-1,ij,im)
        c2 = cleb1_fn(ij-1,im+0,1, 0,ij,im)
        c3 = cleb1_fn(ij-1,im-1,1,+1,ij,im)
        c4 = cleb1_fn(ij  ,im+1,1,-1,ij,im)
        c5 = cleb1_fn(ij  ,im+0,1, 0,ij,im)
        c6 = cleb1_fn(ij  ,im-1,1,+1,ij,im)
        c7 = cleb1_fn(ij+1,im+1,1,-1,ij,im)
        c8 = cleb1_fn(ij+1,im+0,1, 0,ij,im)
        c9 = cleb1_fn(ij+1,im-1,1,+1,ij,im)
        
        cjm1(ijm) = cr(imj2-1,1) * c1 + cr(imj0-1,3) * c2 + conjg( cr(imj2-1,1) ) * c3
        cjm2(ijm) = cr(imj2  ,1) * c4 + cr(imj0  ,3) * c5 + conjg( cr(imj2  ,1) ) * c6
        cjm3(ijm) = cr(imj2+1,1) * c7 + cr(imj0+1,3) * c8 + conjg( cr(imj2+1,1) ) * c9
        
        cjm1(ijm)%im = zero
        cjm2(ijm)%re = zero
        cjm3(ijm)%im = zero
        
      do im = 1, ij
        ijm  = ijm+1
        imj1 = mj( this%jmax1, im-1, ij )
        imj0 = mj( this%jmax1, im  , ij )
        imj2 = mj( this%jmax1, im+1, ij )
        
        c1 = cleb1_fn(ij-1,im+1,1,-1,ij,im)
        c2 = cleb1_fn(ij-1,im+0,1, 0,ij,im)
        c3 = cleb1_fn(ij-1,im-1,1,+1,ij,im)
        c4 = cleb1_fn(ij  ,im+1,1,-1,ij,im)
        c5 = cleb1_fn(ij  ,im+0,1, 0,ij,im)
        c6 = cleb1_fn(ij  ,im-1,1,+1,ij,im)
        c7 = cleb1_fn(ij+1,im+1,1,-1,ij,im)
        c8 = cleb1_fn(ij+1,im+0,1, 0,ij,im)
        c9 = cleb1_fn(ij+1,im-1,1,+1,ij,im)
        
        cjm1(ijm) = cr(imj2-1,1) * c1 + cr(imj0-1,3) * c2 + cr(imj1-1,2) * c3
        cjm2(ijm) = cr(imj2  ,1) * c4 + cr(imj0  ,3) * c5 + cr(imj1  ,2) * c6
        cjm3(ijm) = cr(imj2+1,1) * c7 + cr(imj0+1,3) * c8 + cr(imj1+1,2) * c9
      end do
    end do
    
  end procedure scal2vec_mj_to_jm_sub
  
end submodule scal_to_vec