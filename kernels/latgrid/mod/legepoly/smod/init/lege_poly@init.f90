submodule (lege_poly) init
  implicit none; contains
  
  module procedure init_lege_sub
    
    this%nLege = nLege
    this%jmax  = jmax
    
    call this%get_nma_sub()
    call this%roots_sub()
    call this%coeffs_sub()
    
    this%wght = this%wght / wfac
    
  end procedure init_lege_sub
  
  module procedure deallocate_lege_sub
    
    call free_aligned_sub( this%c_cosx,  this%cosx  )
    call free_aligned_sub( this%c_sinx,  this%sinx  )
    call free_aligned_sub( this%c_cosx2, this%cosx2 )
    call free_aligned_sub( this%c_wght,  this%wght  )
    
    deallocate( this%emj  )
    deallocate( this%fmj  )
    deallocate( this%mamj )
    
  end procedure deallocate_lege_sub
  
end submodule init