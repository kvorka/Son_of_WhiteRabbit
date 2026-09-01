program BielyKralik
  use omp_lib
  use ocean
  implicit none
  
  type(T_ocean)  :: oceanconv
  integer        :: k
  real(kind=dbl) :: start, end
  
#if defined (convection) || defined(benchmark)
  
  call oceanconv%init_sub( speed = .False. )
  
  do
    do k = 1, n_iter_ocean
      call oceanconv%time_scheme_sub()
    end do
    
    call oceanconv%vypis_sub()
  end do
  
  call oceanconv%deallocate_sub()
  
#elif defined (wallclock)
  
  call oceanconv%init_sub( speed = .True. )
  
  start = omp_get_wtime()
  
  do k = 1, n_iter_ocean
    call oceanconv%time_scheme_sub()
  end do
  
  end = omp_get_wtime()
  
  write(*,*) ( end - start ) / n_iter_ocean
  
#endif

end program BielyKralik
  