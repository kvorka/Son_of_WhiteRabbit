module calloc
  use iso_c_binding
  use iso_fortran_env, only : real128
  implicit none; public
  
#if defined (mem64)
  integer, parameter :: alig = 64  !! memory alignement: AVX512
  integer, parameter :: ndbl = 8   !! number of doubles in one reg. AVX512
#elif defined (mem32)
  integer, parameter :: alig = 32  !! memory alignement: AVX
  integer, parameter :: ndbl = 4   !! number of doubles in one reg. AVX
#elif defined (mem16)
  integer, parameter :: alig = 16  !! memory alignement: SSE
  integer, parameter :: ndbl = 2   !! number of doubles in one reg. SSE
#endif
  
  integer, parameter :: dbl = c_double
  integer, parameter :: qbl = real128
  integer, parameter :: size_c_dbl = int( c_sizeof(0._dbl) )
  
  interface
    type(c_ptr) function fortmalloc(alignmt, n) bind(C, name='aligned_alloc')
      import                     :: c_ptr, c_int
      integer(kind=c_int), value :: alignmt, n
    end function fortmalloc
    
    subroutine fortfree(ptr) bind(C, name="free")
      import             :: c_ptr
      type(c_ptr), value :: ptr
    end subroutine fortfree
  end interface
  
end module calloc