submodule (lege_poly) allocators
  implicit none; contains
  
  module procedure allocate_rscalars_sub
    
    allocate( rscal(4*ns*this%nrma) )
    
  end procedure allocate_rscalars_sub
  
end submodule allocators