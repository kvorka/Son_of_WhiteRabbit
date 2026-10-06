submodule (lege_poly) fwd_idx
  implicit none; contains
  
  module procedure index_fwd_sub
    integer :: i, im, ij, imj, ima
    
    im = 0
      !ij == im
        ima = 1
        imj = 1
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj) = cmplx( rcab(1,i,2,ima), rcab(2,i,2,ima), kind=dbl )
        end do
      
      do ij = 1, (this%jmax-1)/2
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj-1) =  this%emj(imj+0) * cmplx( rcab(1,i,1,ima+0), rcab(2,i,1,ima+0), kind=dbl ) + &
                        & this%emj(imj-1) * cmplx( rcab(1,i,1,ima-1), rcab(2,i,1,ima-1), kind=dbl )
          cab(i,imj)   =                    cmplx( rcab(1,i,2,ima+0), rcab(2,i,2,ima+0), kind=dbl )
        end do
      end do
      
      !ij == this%jmax
      if ( mod(this%jmax,2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj-1) =  this%emj(imj+0) * cmplx( rcab(1,i,1,ima+0), rcab(2,i,1,ima+0), kind=dbl ) + &
                        & this%emj(imj-1) * cmplx( rcab(1,i,1,ima-1), rcab(2,i,1,ima-1), kind=dbl )
          cab(i,imj)   =                    cmplx( rcab(1,i,2,ima+0), rcab(2,i,2,ima+0), kind=dbl )
        end do
      
      else
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj) =  this%emj(imj+1) * cmplx( rcab(1,i,1,ima+0), rcab(2,i,1,ima+0), kind=dbl ) + &
                      & this%emj(imj+0) * cmplx( rcab(1,i,1,ima-1), rcab(2,i,1,ima-1), kind=dbl )
        end do
      end if
    
    do im = 1, this%jmax-1
      !ij == im
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj) = cmplx( rcab(1,i,2,ima), rcab(2,i,2,ima), kind=dbl )
        end do
      
      do ij = 1, ( this%jmax-im-1 ) / 2
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj-1) =  this%emj(imj+im+0) * cmplx( rcab(1,i,1,ima+0), rcab(2,i,1,ima+0), kind=dbl ) + &
                        & this%emj(imj+im-1) * cmplx( rcab(1,i,1,ima-1), rcab(2,i,1,ima-1), kind=dbl )
          cab(i,imj)   =                       cmplx( rcab(1,i,2,ima+0), rcab(2,i,2,ima+0), kind=dbl )
        end do
      end do
      
      !ij == this%jmax
      if ( mod( ( this%jmax-im ), 2 ) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj-1) =  this%emj(imj+im+0) * cmplx( rcab(1,i,1,ima+0), rcab(2,i,1,ima+0), kind=dbl ) + &
                        & this%emj(imj+im-1) * cmplx( rcab(1,i,1,ima-1), rcab(2,i,1,ima-1), kind=dbl )
          cab(i,imj)   =                       cmplx( rcab(1,i,2,ima+0), rcab(2,i,2,ima+0), kind=dbl )
        end do
      
      else
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj) =  this%emj(imj+im+1) * cmplx( rcab(1,i,1,ima+0), rcab(2,i,1,ima+0), kind=dbl ) + &
                      & this%emj(imj+im+0) * cmplx( rcab(1,i,1,ima-1), rcab(2,i,1,ima-1), kind=dbl )
        end do
      end if
    end do
    
    im = this%jmax
      !ij == im
        ima = ima+1
        imj = imj+1
        
        !$omp simd
        do i = 1, ncab
          cab(i,imj) = cmplx( rcab(1,i,2,ima), rcab(2,i,2,ima), kind=dbl )
        end do
    
  end procedure index_fwd_sub
  
end submodule fwd_idx