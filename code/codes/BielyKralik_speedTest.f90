program BielyKralik_speedTest
  use omp_lib
  use ocean
  implicit none

  type(T_ocean)  :: oceanmodel
  real(kind=dbl) :: start, end
  
  call oceanmodel%init_sub( speed = .True. )
  
  start = omp_get_wtime()
    call oceanmodel%speed_sub()
  end = omp_get_wtime()

  write(*,*) (end-start) / oceanmodel%n_iter
  
end program BielyKralik_speedTest
