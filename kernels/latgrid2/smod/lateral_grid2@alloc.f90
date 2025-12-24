submodule (lateral_grid2) alloc
  implicit none; contains
  
  module procedure alloc_grid_shtns_sub
    
    call alloc_aligned1d_sub( this%nL * this%nF, grid_c, grid )
    
  end procedure alloc_grid_shtns_sub
  
end submodule alloc