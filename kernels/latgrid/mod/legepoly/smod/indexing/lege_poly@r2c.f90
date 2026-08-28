submodule (lege_poly) r2c
  implicit none; contains
  
  module procedure r2c_mj_to_mj_sub
    integer :: in, im, ij, imj, ima
    
    im = 0
      !ij == im
        ima = 1
        imj = 1
        
        !$omp simd
        do in = 1, ncab
          cab(in,imj)%re = rcab(1,in,2,ima)
          cab(in,imj)%im = rcab(2,in,2,ima)
        end do
      
      do ij = 1, (this%jmax-1)/2
        ima = ima+1
        imj = imj+2
        
        call fwd_idx_sub( ncab, this%emj(imj-1), rcab(1,1,1,ima-1), cab(1,imj-1) )
      end do
      
      !ij == this%jmax
      if ( mod((this%jmax),2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        call fwd_idx_sub( ncab, this%emj(imj-1), rcab(1,1,1,ima-1), cab(1,imj-1) )
        
      else
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do in = 1, ncab
          cab(in,imj) = this%emj(imj+1) * cmplx( rcab(1,in,1,ima  ), rcab(2,in,1,ima  ), kind=dbl ) + &
                      & this%emj(imj)   * cmplx( rcab(1,in,1,ima-1), rcab(2,in,1,ima-1), kind=dbl )
        end do
      end if
    
    do im = 1, this%jmax-1
      !ij == im
        ima = ima+1
        imj = imj+1
        
       !$omp simd
        do in = 1, ncab
          cab(in,imj)%re = rcab(1,in,2,ima)
          cab(in,imj)%im = rcab(2,in,2,ima)
        end do
      
      do ij = 1, (this%jmax-im-1)/2
        ima = ima+1
        imj = imj+2
        
        call fwd_idx_sub( ncab, this%emj(imj+im-1), rcab(1,1,1,ima-1), cab(1,imj-1) )
      end do
      
      !ij == this%jmax
      if ( mod((this%jmax-im),2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        call fwd_idx_sub( ncab, this%emj(imj+im-1), rcab(1,1,1,ima-1), cab(1,imj-1) )
        
      else
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do in = 1, ncab
          cab(in,imj) = this%emj(imj+im+1) * cmplx( rcab(1,in,1,ima  ), rcab(2,in,1,ima  ), kind=dbl ) + &
                      & this%emj(imj+im)   * cmplx( rcab(1,in,1,ima-1), rcab(2,in,1,ima-1), kind=dbl )
        end do
      end if
    end do
    
    im = this%jmax
      !ij == im
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do in = 1, ncab
          cab(in,imj)%re = rcab(1,in,2,ima)
          cab(in,imj)%im = rcab(2,in,2,ima)
        end do
      
  end procedure r2c_mj_to_mj_sub
  
end submodule r2c