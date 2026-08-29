submodule (lege_poly) r2c
  implicit none; contains
  
  module procedure r2c_mj_to_mj_sub
    integer :: im, ij, imj, ima
    
    im = 0
      !ij == im
        ima = 1
        imj = 1
        
        call fwd_idx3_sub( ncab, rcab(1,ima), cab(1,imj) )
        
      do ij = 1, (this%jmax-1)/2
        ima = ima+1
        imj = imj+2
        
        call fwd_idx2_sub( ncab, this%emj(imj-1), rcab(1,ima-1), cab(1,imj-1) )
        call fwd_idx3_sub( ncab,                  rcab(1,ima  ), cab(1,imj  ) )
      end do
      
      !ij == this%jmax
      if ( mod(this%jmax,2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        call fwd_idx2_sub( ncab, this%emj(imj-1), rcab(1,ima-1), cab(1,imj-1) )
        call fwd_idx3_sub( ncab,                  rcab(1,ima  ), cab(1,imj  ) )
        
      else
        ima = ima+1
        imj = imj+1
        
        call fwd_idx2_sub( ncab, this%emj(imj), rcab(1,ima-1), cab(1,imj) )
      end if
    
    do im = 1, this%jmax-1
      !ij == im
        ima = ima+1
        imj = imj+1
        
       call fwd_idx3_sub( ncab, rcab(1,ima), cab(1,imj) )
      
      do ij = 1, (this%jmax-im-1)/2
        ima = ima+1
        imj = imj+2
        
        call fwd_idx2_sub( ncab, this%emj(imj+im-1), rcab(1,ima-1), cab(1,imj-1) )
        call fwd_idx3_sub( ncab,                     rcab(1,ima  ), cab(1,imj  ) )
      end do
      
      !ij == this%jmax
      if ( mod((this%jmax-im),2) == 0 ) then
        ima = ima+1
        imj = imj+2
        
        call fwd_idx2_sub( ncab, this%emj(imj+im-1), rcab(1,ima-1), cab(1,imj-1) )
        call fwd_idx3_sub( ncab,                     rcab(1,ima  ), cab(1,imj  ) )
        
      else
        ima = ima+1
        imj = imj+1
        
        call fwd_idx2_sub( ncab, this%emj(imj+im), rcab(1,ima-1), cab(1,imj) )
      end if
    end do
    
    im = this%jmax
      !ij == im
        ima = ima+1
        imj = imj+1
        
        call fwd_idx3_sub( ncab, rcab(1,ima), cab(1,imj) )
        
  end procedure r2c_mj_to_mj_sub
  
end submodule r2c