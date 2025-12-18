module ocean_constants
  use math
  implicit none
  
  !Vseobecne nastavenie konstant
  integer, parameter :: nd_ocean = 145
  integer, parameter :: jmax_ocean = 213
  integer, parameter :: n_iter_ocean = 20
  
  !Nastavovanie konstant pre konvektivny vypocet
  character(len=*), parameter :: therm_bnd_ocean = 'basic'
  character(len=*), parameter :: diffusion_ocean = 'isotr'  ! 'isotr', 'mitgc', 'hyper'
  real(kind=dbl),   parameter :: r_ud_ocean      = 0.60_dbl
  real(kind=dbl),   parameter :: Pr_ocean        = 1._dbl
  real(kind=dbl),   parameter :: Ra_ocean        = 8.0d6
  real(kind=dbl),   parameter :: Ek_ocean        = 1.0d-4
  
  !Nastavovanie pociatocneho stavu
  logical, parameter :: init_through_file_ocean = .false.
  logical, parameter :: init_through_file_bnd_ocean = .false.
  integer, parameter :: nd_init_ocean = 73
  integer, parameter :: jmax_init_ocean = 213
  
end module ocean_constants