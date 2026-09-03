module ocean_parms
  use math
  implicit none
  
#if !defined ( wallclock ) && !defined ( benchmark )
  !! Resolution and output frequency
  integer, parameter :: nd_ocean = 73
  integer, parameter :: jmax_ocean = 177
  integer, parameter :: n_iter_ocean = 100
  
  !! Control parameters
  real(kind=dbl), parameter :: r_ud_ocean = 0.90_dbl
  real(kind=dbl), parameter :: Pr_ocean   = 10._dbl
  real(kind=dbl), parameter :: Ra_ocean   = 6.670d6
  real(kind=dbl), parameter :: Ek_ocean   = 6.0d-4
  
  !! Boundary conditions and diffusion scheme
  character(len=*), parameter :: therm_bnd_ocean = 'basic'  !! 'basic', 'fluxd'
  character(len=*), parameter :: mech_bnd_ocean  = 'frslp'  !! 'frslp', 'noslp'
  character(len=*), parameter :: diffusion_ocean = 'isotr'  !! 'isotr', 'mitgc', 'hyper'
  
  !! Initialization of dynamical state
  logical,          parameter :: init_through_file_ocean = .false.
  integer,          parameter :: nd_init_ocean           = 73
  integer,          parameter :: jmax_init_ocean         = 213
  character(len=*), parameter :: init_temp_file          = 'inittemp'
  character(len=*), parameter :: init_velc_file          = 'initvelc'
  
  !! Initialization of the boundary condition
  logical,          parameter :: init_through_file_bnd_ocean = .false.
  character(len=*), parameter :: init_bbnd_file              ='heat_flux_cond.cmplx'
  
#elif defined ( wallclock )
  integer,          parameter :: nd_ocean        = 145
  integer,          parameter :: jmax_ocean      = 213
  integer,          parameter :: n_iter_ocean    = 20
  real(kind=dbl),   parameter :: r_ud_ocean      = 0.60_dbl
  real(kind=dbl),   parameter :: Pr_ocean        = 1._dbl
  real(kind=dbl),   parameter :: Ra_ocean        = 1.0d7
  real(kind=dbl),   parameter :: Ek_ocean        = 1.0d-4
  character(len=*), parameter :: therm_bnd_ocean = 'basic'
  character(len=*), parameter :: mech_bnd_ocean  = 'noslp'
  character(len=*), parameter :: diffusion_ocean = 'isotr'
  logical,          parameter :: init_through_file_ocean     = .false.
  logical,          parameter :: init_through_file_bnd_ocean = .false.
  integer,          parameter :: nd_init_ocean   = 73
  integer,          parameter :: jmax_init_ocean = 213
  character(len=*), parameter :: init_temp_file  = 'inittemp'
  character(len=*), parameter :: init_velc_file  = 'initvelc'
  character(len=*), parameter :: init_bbnd_file  = 'heat_flux_cond.cmplx'
  
#elif defined ( benchmark )
  integer,          parameter :: nd_ocean        = 73
  integer,          parameter :: jmax_ocean      = 125
  integer,          parameter :: n_iter_ocean    = 100
  real(kind=dbl),   parameter :: r_ud_ocean      = 0.60_dbl
  real(kind=dbl),   parameter :: Pr_ocean        = 1._dbl
  real(kind=dbl),   parameter :: Ra_ocean        = 8.0d6
  real(kind=dbl),   parameter :: Ek_ocean        = 1.0d-4
  character(len=*), parameter :: therm_bnd_ocean = 'basic'
  character(len=*), parameter :: mech_bnd_ocean  = 'noslp'
  character(len=*), parameter :: diffusion_ocean = 'isotr'
  logical,          parameter :: init_through_file_ocean     = .false.
  logical,          parameter :: init_through_file_bnd_ocean = .false.
  integer,          parameter :: nd_init_ocean   = 73
  integer,          parameter :: jmax_init_ocean = 213
  character(len=*), parameter :: init_temp_file  = 'inittemp'
  character(len=*), parameter :: init_velc_file  = 'initvelc'
  character(len=*), parameter :: init_bbnd_file  = 'heat_flux_cond.cmplx'
  
#endif
  
end module ocean_parms
