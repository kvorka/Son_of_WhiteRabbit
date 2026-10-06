submodule (lege_poly) dealloc
  implicit none; contains
  
  module procedure deallocate_lege_sub
    
    call free_aligned_sub( this%c_cosx,  this%cosx  )
    call free_aligned_sub( this%c_sinx,  this%sinx  )
    call free_aligned_sub( this%c_cosx2, this%cosx2 )
    call free_aligned_sub( this%c_wght,  this%wght  )
    
    deallocate( this%emj  )
    deallocate( this%fmj  )
    deallocate( this%mamj )
    
  end procedure deallocate_lege_sub
  
end submodule dealloc