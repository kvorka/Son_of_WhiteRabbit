submodule (lege_poly) bwd_idx
  implicit none; contains
  
  module procedure index_bwd_sub
    integer :: i, im, ij, imj, ima
    
    im = 0
      !ij == im
        ima = 1
        imj = 1
        
        !$omp simd
        do i = 1, ncab
          rcab(1,i,1,ima) = cab(i,imj+1)%re * this%emj(imj+1)
          rcab(2,i,1,ima) = cab(i,imj+1)%im * this%emj(imj+1)
          rcab(1,i,2,ima) = cab(i,imj+0)%re
          rcab(2,i,2,ima) = cab(i,imj+0)%im
        end do
      
      do ij = 1, (this%jmax-1)/2
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do i = 1, ncab
          rcab(1,i,1,ima) = this%emj(imj) * cab(i,imj-1)%re + this%emj(imj+1) * cab(i,imj+1)%re
          rcab(2,i,1,ima) = this%emj(imj) * cab(i,imj-1)%im + this%emj(imj+1) * cab(i,imj+1)%im
          rcab(1,i,2,ima) =                 cab(i,imj+0)%re
          rcab(2,i,2,ima) =                 cab(i,imj+0)%im
        end do
      end do
      
      !ij == this%jmax
      if ( mod((this%jmax),2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do i = 1, ncab
          rcab(1,i,1,ima) = this%emj(imj) * cab(i,imj-1)%re
          rcab(2,i,1,ima) = this%emj(imj) * cab(i,imj-1)%im
          rcab(1,i,2,ima) =                 cab(i,imj+0)%re
          rcab(2,i,2,ima) =                 cab(i,imj+0)%im
        end do
      
      else
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do i = 1, ncab
          rcab(1,i,1,ima) = this%emj(imj+1) * cab(i,imj)%re
          rcab(2,i,1,ima) = this%emj(imj+1) * cab(i,imj)%im
          rcab(1,i,2,ima) = 0._dbl
          rcab(2,i,2,ima) = 0._dbl
        end do
      end if
    
    do im = 1, this%jmax-1
      !ij == im
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do i = 1, ncab
          rcab(1,i,1,ima) = cab(i,imj+1)%re * this%emj(imj+im+1)
          rcab(2,i,1,ima) = cab(i,imj+1)%im * this%emj(imj+im+1)
          rcab(1,i,2,ima) = cab(i,imj+0)%re
          rcab(2,i,2,ima) = cab(i,imj+0)%im
        end do
      
      do ij = 1, (this%jmax-1-im)/2
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do i = 1, ncab
          rcab(1,i,1,ima) = this%emj(imj+im) * cab(i,imj-1)%re + this%emj(imj+im+1) * cab(i,imj+1)%re
          rcab(2,i,1,ima) = this%emj(imj+im) * cab(i,imj-1)%im + this%emj(imj+im+1) * cab(i,imj+1)%im
          rcab(1,i,2,ima) =                    cab(i,imj+0)%re
          rcab(2,i,2,ima) =                    cab(i,imj+0)%im
        end do
      end do
      
      !ij == this%jmax
      if ( mod((this%jmax-im),2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do i = 1, ncab
          rcab(1,i,1,ima) = this%emj(imj+im) * cab(i,imj-1)%re
          rcab(2,i,1,ima) = this%emj(imj+im) * cab(i,imj-1)%im
          rcab(1,i,2,ima) =                    cab(i,imj+0)%re
          rcab(2,i,2,ima) =                    cab(i,imj+0)%im
        end do
      
      else
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do i = 1, ncab
          rcab(1,i,1,ima) = this%emj(imj+im+1) * cab(i,imj)%re
          rcab(2,i,1,ima) = this%emj(imj+im+1) * cab(i,imj)%im
          rcab(1,i,2,ima) = 0._dbl
          rcab(2,i,2,ima) = 0._dbl
        end do
      end if
    end do
    
    im = this%jmax
      !ij == im
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do i = 1, ncab
          rcab(1,i,1,ima) = 0._dbl
          rcab(2,i,1,ima) = 0._dbl
          rcab(1,i,2,ima) = cab(i,imj)%re
          rcab(2,i,2,ima) = cab(i,imj)%im
        end do
    
  end procedure index_bwd_sub
  
end submodule bwd_idx