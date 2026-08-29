submodule (lege_poly) c2r
  implicit none; contains
  
  module procedure c2r_mj_to_mj_sub
    integer :: im, ij, imj, ima
    
    im = 0
      !ij == im
        ima = 1
        imj = 1
        
        call bwd_idx1_sub( ncab, this%emj(imj+1), cab(1,imj+1), rcab(1,ima) )
        call bwd_idx3_sub( ncab,                  cab(1,imj  ), rcab(1,ima) )
      
      do ij = 1, (this%jmax-1)/2
        ima = ima+1
        imj = imj+2
        
        call bwd_idx2_sub( ncab, this%emj(imj), cab(1,imj-1), rcab(1,ima) )
        call bwd_idx3_sub( ncab,                cab(1,imj  ), rcab(1,ima) )
      end do
      
      !ij == this%jmax
      if ( mod(this%jmax,2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        call bwd_idx1_sub( ncab, this%emj(imj), cab(1,imj-1), rcab(1,ima) )
        call bwd_idx3_sub( ncab,                cab(1,imj  ), rcab(1,ima) )
      
      else
        ima = ima+1
        imj = imj+1
        
        call bwd_idx1_sub( ncab, this%emj(imj+1), cab(1,imj), rcab(1,ima) )
      end if
    
    do im = 1, this%jmax-1
      !ij == im
        ima = ima+1
        imj = imj+1
        
        call bwd_idx1_sub( ncab, this%emj(imj+im+1), cab(1,imj+1), rcab(1,ima) )
        call bwd_idx3_sub( ncab,                     cab(1,imj  ), rcab(1,ima) )
      
      do ij = 1, (this%jmax-1-im)/2
        ima = ima+1
        imj = imj+2
        
        call bwd_idx2_sub( ncab, this%emj(imj+im), cab(1,imj-1), rcab(1,ima) )
        call bwd_idx3_sub( ncab,                   cab(1,imj  ), rcab(1,ima) )
      end do
      
      !ij == this%jmax
      if ( mod((this%jmax-im),2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        call bwd_idx1_sub( ncab, this%emj(imj+im), cab(1,imj-1), rcab(1,ima) )
        call bwd_idx3_sub( ncab,                   cab(1,imj  ), rcab(1,ima) )
      
      else
        ima = ima+1
        imj = imj+1
        
        call bwd_idx1_sub( ncab, this%emj(imj+im+1), cab(1,imj), rcab(1,ima) )
      end if
    end do
    
    im = this%jmax
      !ij == im
        ima = ima+1
        imj = imj+1
        
        call bwd_idx3_sub( ncab, cab(1,imj), rcab(1,ima) )
        
  end procedure c2r_mj_to_mj_sub
  
end submodule c2r