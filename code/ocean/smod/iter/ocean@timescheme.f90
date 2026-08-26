submodule (ocean) timescheme
  implicit none; contains
  
  module procedure time_scheme_ocean_sub
    
    call this%fullnl_sub()
    call this%solve_all_sub()
    
  end procedure time_scheme_ocean_sub
  
end submodule timescheme
