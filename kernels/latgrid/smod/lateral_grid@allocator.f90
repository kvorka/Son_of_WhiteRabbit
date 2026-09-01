submodule (lateral_grid) allocator
  implicit none; contains
  
  module procedure alloc_work_lgrid_sub
    
    call alloc_aligned_sub( 2 * nb * step * this%fft%n + 4 * step, c_work, work )
    
  end procedure alloc_work_lgrid_sub

end submodule allocator