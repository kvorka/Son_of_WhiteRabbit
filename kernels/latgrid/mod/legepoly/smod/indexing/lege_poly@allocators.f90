submodule (lege_poly) allocators
  implicit none; contains
  
  module procedure allocate_rscalars_sub
    integer :: n, i
    
    n = 4*ns*this%nrma
    
    c_rscal = fortmalloc( 32, n * size_c_dbl )
    call c_f_pointer( c_rscal, rscal, [n] )
    
    !$omp simd aligned (rscal:32)
    do i = 1, n
      rscal(i) = zero
    end do
    
  end procedure allocate_rscalars_sub
  
end submodule allocators