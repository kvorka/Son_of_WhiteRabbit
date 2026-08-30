submodule (sphsvt) allocator
  implicit none; contains
  
  module procedure alloc_work_rxd_sub
    
    allocate( work(n*this%jms1) )
    
  end procedure alloc_work_rxd_sub
  
end submodule allocator