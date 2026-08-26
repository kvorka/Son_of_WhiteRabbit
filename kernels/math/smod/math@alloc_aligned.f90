submodule (math) alloc_aligned
  implicit none; contains
  
  module procedure alloc_aligned_sub
    
    c_arr = fortmalloc( alig, n * size_c_dbl )
    call c_f_pointer( c_arr, f_arr, [n] )
    
  end procedure alloc_aligned_sub
  
  module procedure free_aligned_sub
    
    f_arr => null()
    call fortfree( c_arr )
    
  end procedure free_aligned_sub
  
end submodule alloc_aligned