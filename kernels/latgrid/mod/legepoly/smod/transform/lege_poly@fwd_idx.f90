submodule (lege_poly) fwd_idx
  implicit none; contains
  
  module procedure index_fwd_sub
    integer :: i, im, ij, imj, ima
    
    ima = 0
    imj = 0
    
    do im = 0, this%jmax-1
      ima = ima+1
      imj = imj+1
      
      !$omp simd
      do i = 1, ncab
        cab(i,imj)%re = rcab(1,i,2,ima)
        cab(i,imj)%im = rcab(2,i,2,ima)
      end do
      
      do ij = 1, (this%jmax-1-im)/2
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj-1)%re = this%emj(imj+im+0) * rcab(1,i,1,ima) + this%emj(imj+im-1) * rcab(1,i,1,ima-1)
          cab(i,imj-1)%im = this%emj(imj+im+0) * rcab(2,i,1,ima) + this%emj(imj+im-1) * rcab(2,i,1,ima-1)
          cab(i,imj+0)%re =                      rcab(1,i,2,ima)
          cab(i,imj+0)%im =                      rcab(2,i,2,ima)
        end do
      end do
      
      if ( mod(this%jmax-im,2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj-1)%re = this%emj(imj+im+0) * rcab(1,i,1,ima) + this%emj(imj+im-1) * rcab(1,i,1,ima-1)
          cab(i,imj-1)%im = this%emj(imj+im+0) * rcab(2,i,1,ima) + this%emj(imj+im-1) * rcab(2,i,1,ima-1)
          cab(i,imj+0)%re =                      rcab(1,i,2,ima)
          cab(i,imj+0)%im =                      rcab(2,i,2,ima)
        end do
      
      else
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj)%re = this%emj(imj+im+0) * rcab(1,i,1,ima) + this%emj(imj+im-1) * rcab(1,i,1,ima-1)
          cab(i,imj)%im = this%emj(imj+im+0) * rcab(2,i,1,ima) + this%emj(imj+im-1) * rcab(2,i,1,ima-1)
        end do
      end if
    end do
    
    !$omp simd
    do i = 1, ncab
      cab(i,imj+1)%re = rcab(1,i,2,ima+1)
      cab(i,imj+1)%im = rcab(2,i,2,ima+1)
    end do
    
  end procedure index_fwd_sub
  
end submodule fwd_idx