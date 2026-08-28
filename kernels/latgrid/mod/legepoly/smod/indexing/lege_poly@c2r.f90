submodule (lege_poly) c2r
  implicit none; contains
  
  module procedure c2r_mj_to_mj_sub
    integer :: in, im, ij, imj, ima
    
    im = 0
      !ij == im
        ima = 1
        imj = 1
        
        !$omp simd
        do in = 1, ncab
          rcab(1,in,1,ima) = this%emj(imj+1) * cab(in,imj+1)%re
          rcab(2,in,1,ima) = this%emj(imj+1) * cab(in,imj+1)%im
          rcab(1,in,2,ima) =                   cab(in,imj  )%re
          rcab(2,in,2,ima) =                   cab(in,imj  )%im
        end do
      
      do ij = 1, (this%jmax-1)/2
        ima = ima+1
        imj = imj+2
        
        call bwd_idx_sub( ncab, this%emj(imj), cab(1,imj-1), rcab(1,1,1,ima) )
      end do
      
      !ij == this%jmax
      if ( mod((this%jmax),2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do in = 1, ncab
          rcab(1,in,1,ima) = this%emj(imj) * cab(in,imj-1)%re
          rcab(2,in,1,ima) = this%emj(imj) * cab(in,imj-1)%im
          rcab(1,in,2,ima) =                 cab(in,imj  )%re
          rcab(2,in,2,ima) =                 cab(in,imj  )%im
        end do
      
      else
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do in = 1, ncab
          rcab(1,in,1,ima) = this%emj(imj+1) * cab(in,imj)%re
          rcab(2,in,1,ima) = this%emj(imj+1) * cab(in,imj)%im
        end do
      end if
    
    do im = 1, this%jmax-1
      !ij == im
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do in = 1, ncab
          rcab(1,in,1,ima) = this%emj(imj+im+1) * cab(in,imj+1)%re
          rcab(2,in,1,ima) = this%emj(imj+im+1) * cab(in,imj+1)%im
          rcab(1,in,2,ima) =                      cab(in,imj  )%re
          rcab(2,in,2,ima) =                      cab(in,imj  )%im
        end do
      
      do ij = 1, (this%jmax-1-im)/2
        ima = ima+1
        imj = imj+2
        
        call bwd_idx_sub( ncab, this%emj(imj+im), cab(1,imj-1), rcab(1,1,1,ima) )
      end do
      
      !ij == this%jmax
      if ( mod((this%jmax-im),2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do in = 1, ncab
          rcab(1,in,1,ima) = this%emj(imj+im) * cab(in,imj-1)%re
          rcab(2,in,1,ima) = this%emj(imj+im) * cab(in,imj-1)%im
          rcab(1,in,2,ima) =                    cab(in,imj  )%re
          rcab(2,in,2,ima) =                    cab(in,imj  )%im
        end do
      
      else
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do in = 1, ncab
          rcab(1,in,1,ima) = this%emj(imj+im+1) * cab(in,imj)%re
          rcab(2,in,1,ima) = this%emj(imj+im+1) * cab(in,imj)%im
        end do
      end if
    end do
    
    im = this%jmax
      !ij == im
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do in = 1, ncab
          rcab(1,in,2,ima) = cab(in,imj)%re
          rcab(2,in,2,ima) = cab(in,imj)%im
        end do
    
  end procedure c2r_mj_to_mj_sub
  
end submodule c2r