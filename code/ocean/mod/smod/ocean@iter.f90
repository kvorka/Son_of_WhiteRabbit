submodule (ocean) iter
  implicit none; contains
  
  module procedure iter_ocean_sub
    integer :: k
    
    do k = 1, this%n_iter
      this%t = this%t + this%dt
        call this%time_scheme_sub()
    end do
    
    call this%vypis_ocean_sub()
    
  end procedure iter_ocean_sub
  
  module procedure speed_ocean_sub
    integer :: k
    
    do !k = 1, this%n_iter
      call this%time_scheme_sub()
    end do
    
  end procedure speed_ocean_sub
  
end submodule iter