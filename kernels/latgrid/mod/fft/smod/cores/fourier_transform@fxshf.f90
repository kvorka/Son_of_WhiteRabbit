submodule (fourier_transform) fxshf
  implicit none; contains
  
  module procedure fxzshf
    integer                             :: j, i30, i31
    real(kind=dbl), pointer, contiguous :: y(:)
    type(c_ptr)                         :: c_y
    
    call alloc_aligned_sub( 8*m*ndbl, c_y, y )
    
    j = 0
    
    do while ( j < n/2-2 )
      i30 = it(j)
      
      if ( i30 < 0 ) then
        j = j + 1
        
      else
        j   = j + 1
        i31 = it(j)
        
        call fxcpy( m, x(1,i30), y )
        
        do while ( i31 >= 0 )
          call fxcpy( m, x(1,i31), x(1,i30) )
          i30 = i31
          
          j   = j + 1
          i31 = it(j)
        end do
        
        j = j + 1
        call fxcpy( m, x(1,i31-imm), x(1,i30    ) )
        call fxcpy( m, y,            x(1,i31-imm) )
      end if
    end do
    
    call free_aligned_sub( c_y, y )
    
  end procedure fxzshf
  
end submodule fxshf