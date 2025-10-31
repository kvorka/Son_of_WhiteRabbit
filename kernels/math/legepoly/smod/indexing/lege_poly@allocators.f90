submodule (lege_poly) allocators
  implicit none; contains
  
  module procedure allocate_rscalars_sub
    integer :: n
    
    n = 4*ns*this%nrma
    
    c_rscal = fortmalloc( 32, n * int( c_sizeof(0._dbl) ) )
    call c_f_pointer( c_rscal, rscal, [n] )
    call zero_rarray_sub( n, rscal )
    
  end procedure allocate_rscalars_sub
  
end submodule allocators