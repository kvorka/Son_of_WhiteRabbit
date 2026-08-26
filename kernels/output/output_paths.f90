module output_paths
  use math
  implicit none
  
  real(kind=dbl), parameter :: tNuss = 0._dbl
  
  integer, parameter :: avrg_start = 1300
  integer, parameter :: avrg_end   = 1750
  
  integer, parameter :: snpsht1 = 0
  integer, parameter :: snpsht2 = 0
  integer, parameter :: snpsht3 = 0
  
  character(len=*), parameter :: path_nuss       = 'data/Nuss.dat'
  character(len=*), parameter :: path_ocean_temp = 'data/data_ocean_temp/Temp-'
  character(len=*), parameter :: path_ocean_velc = 'data/data_ocean_veloc/Velc-'
  character(len=*), parameter :: path_ocean_flux = 'data/data_ocean_fluxu/Fluxu-'
  
end module output_paths
