submodule (sphsvt) allocators
  implicit none; contains
  
  module procedure allocate_scalars_sub
    
    allocate( cscal(ns*this%jms1) )
      call zero_carray_sub( ns*this%jms1, cscal(1) )
    
  end procedure allocate_scalars_sub
  
end submodule allocators