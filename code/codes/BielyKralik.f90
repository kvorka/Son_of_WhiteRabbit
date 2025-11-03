program BielyKralik
  use ocean
  implicit none
  
#if defined (convection)
  type(T_ocean) :: oceanconv
  
  call oceanconv%init_sub()
  
  do
    call oceanconv%iter_sub()
  end do
  
  call oceanconv%deallocate_sub()
  
#elif defined (wallclock)
  type(T_ocean)  :: oceanspeed
  real(kind=dbl) :: start, end
  
  call oceanspeed%init_sub( speed = .True. )
  
  start = omp_get_wtime()
    call oceanspeed%speed_sub()
  end = omp_get_wtime()

  write(*,*) (end-start) / oceanspeed%n_iter
#endif

end program BielyKralik
  