module calloc
  use iso_c_binding
  implicit none; public
  
#if defined (avx512)
  integer, parameter :: alig = 64      !memory alignement: AVX512
#elif defined (avx2)
  integer, parameter :: alig = 32      !memory alignement: AVX2
#else
  integer, parameter :: alig = 16      !memory alignement: AVX
#endif
  
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