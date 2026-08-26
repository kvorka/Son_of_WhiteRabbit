module ocean_constants
  use math
  implicit none
  
  !! Resolution and output frequency
#if defined ( wallclock )
  integer, parameter :: nd_ocean = 145
  integer, parameter :: jmax_ocean = 213
  integer, parameter :: n_iter_ocean = 20
#elif defined ( benchmark )
  integer, parameter :: nd_ocean = 73
  integer, parameter :: jmax_ocean = 125
  integer, parameter :: n_iter_ocean = 100
#else
  integer, parameter :: nd_ocean = 73
  integer, parameter :: jmax_ocean = 125
  integer, parameter :: n_iter_ocean = 100
#endif
  
  !! Boundary condition and diffusion scheme
  character(len=*), parameter :: therm_bnd_ocean = 'basic'  ! 'basic', 'fluxd'
  character(len=*), parameter :: diffusion_ocean = 'isotr'  ! 'isotr', 'mitgc', 'hyper'
  
  !! Control parameters
#if defined ( benchmark )
  real(kind=dbl),   parameter :: r_ud_ocean = 0.60_dbl
  real(kind=dbl),   parameter :: Pr_ocean   = 1._dbl
  real(kind=dbl),   parameter :: Ra_ocean   = 8.0d6
  real(kind=dbl),   parameter :: Ek_ocean   = 1.0d-4
#else
  real(kind=dbl),   parameter :: r_ud_ocean = 0.60_dbl
  real(kind=dbl),   parameter :: Pr_ocean   = 1._dbl
  real(kind=dbl),   parameter :: Ra_ocean   = 8.0d6
  real(kind=dbl),   parameter :: Ek_ocean   = 1.0d-4
#endif
  
  !! Initialization of dynamical state
  logical,          parameter :: init_through_file_ocean = .false.
  integer,          parameter :: nd_init_ocean           = 73
  integer,          parameter :: jmax_init_ocean         = 213
  character(len=*), parameter :: init_temp_file          = 'inittemp'
  character(len=*), parameter :: init_velc_file          = 'initvelc'
  
  !! Initialization of the boundary condition
  logical,          parameter :: init_through_file_bnd_ocean = .false.
  character(len=*), parameter :: init_bbnd_file              ='heat_flux_cond.cmplx'
  
end module ocean_constants
