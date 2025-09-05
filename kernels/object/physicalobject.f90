module physicalobject
  use math
  use sph_indexing
  use sph_unitvec_op
  use sph_norms
  use lateral_grid
  use radial_grid
  use matrices
  use solution
  implicit none
  
  type, abstract, public :: T_physicalObject
    character(len=5)               :: thermal_bnd
    integer                        :: nd, jmax, jms, jmv, n_iter, poc
    real(kind=dbl)                 :: t, dt, cf, ab, r_ud, Pr, Ra, Ek
    integer,           allocatable :: j_indx(:)
    complex(kind=dbl), allocatable :: rsph1(:,:), rsph2(:,:), rtorr(:,:), rtemp(:,:)
    
    type(T_radialGrid)  :: rad_grid
    type(T_lateralGrid) :: lat_grid
    type(T_matrices)    :: mat
    type(T_solution)    :: sol
    
    contains
    
    procedure, pass :: init_objects_sub       => init_objects_sub
    procedure, pass :: deallocate_objects_sub => deallocate_objects_sub
    
    !Variables :: Thermal solution
    procedure, pass :: temp_r_fn, dT_dr_r_fn, dT_dr_r_ijm_sub
    procedure, pass :: temp_rr_ijm_sub, dT_dr_rr_ijm_sub, gradT_rr_ijml_sub
    
    !Variables mechanical part of solution
    procedure, pass :: v_rr_ijml_sub
    procedure, pass :: dv_dr_rr_ijml_sub, curlv_rr_ijml_sub
    
    !Matrices, equations, solvers
    procedure, pass :: init_eq_mech_sub, init_eq_torr_sub, init_eq_temp_sub
    procedure, pass :: mat_temp_fn, mat_mech_fn, mat_torr_fn
    procedure, pass :: prepare_mat_mech_sub, prepare_mat_temp_sub, prepare_mat_torr_sub
    procedure, pass :: solve_temp_sub, solve_torr_sub, solve_mech_sub
    
    !Forces
    procedure, pass :: coriolis_rr_jml_sub
    procedure, pass :: buoy_rr_jml_sub
    
    !Output, control measures
    procedure, pass :: vypis_sub
    procedure, pass :: reynolds_fn, temperature_fn, nuss_fn
    procedure, pass :: reynolds_poloidal_fn, reynolds_torroidal_fn
    
  end type T_physicalObject
  
  interface
    module subroutine init_objects_sub(this, nd, jmax, r_ud)
      class(T_physicalObject),    intent(inout) :: this
      integer,                    intent(in)    :: nd, jmax
      real(kind=dbl),             intent(in)    :: r_ud
    end subroutine init_objects_sub
    
    module subroutine deallocate_objects_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine deallocate_objects_sub
    
    !Interfaces :: Variables temperature
    module pure complex(kind=dbl) function temp_r_fn(this, ir, ijm)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: ir, ijm
    end function temp_r_fn
    
    module pure complex(kind=dbl) function dT_dr_r_fn(this, ir, ijm)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: ir, ijm
    end function dT_dr_r_fn
    
    module pure subroutine dT_dr_r_ijm_sub(this, ir, dT_dr_r)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: dT_dr_r(*)
    end subroutine dT_dr_r_ijm_sub
    
    module pure subroutine temp_rr_ijm_sub(this, ir, temp_rr_ijm)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: temp_rr_ijm(*)
    end subroutine temp_rr_ijm_sub
    
    module pure subroutine dT_dr_rr_ijm_sub(this, ir, T, dT)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: T(*), dT(*)
    end subroutine dT_dr_rr_ijm_sub
    
    module pure subroutine gradT_rr_ijml_sub(this, ir, T, gradT, sgn)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir, sgn
      complex(kind=dbl),       intent(out) :: T(*), gradT(*)
    end subroutine gradT_rr_ijml_sub
    
    !Interfaces :: Variables velocity
    module pure subroutine v_rr_ijml_sub(this, ir, v_rr_ijml)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: v_rr_ijml(*)
    end subroutine v_rr_ijml_sub
    
    module pure subroutine dv_dr_rr_ijml_sub(this, ir, v, dv)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: dv(*), v(*)
    end subroutine dv_dr_rr_ijml_sub
    
    module pure subroutine curlv_rr_ijml_sub(this, ir, v, curlv)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: v(*), curlv(*)
    end subroutine curlv_rr_ijml_sub
    
    !Interfaces :: output
    module subroutine vypis_sub(this, filenum, path, quantity)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: filenum
      character(len=*),        intent(in) :: path, quantity
    end subroutine vypis_sub
    
    !Interfaces :: to be continued
    module pure subroutine coriolis_rr_jml_sub(this, v, coriolis)
      class(T_physicalObject), intent(in)    :: this
      complex(kind=dbl),       intent(in)    :: v(*)
      complex(kind=dbl),       intent(inout) :: coriolis(*)
    end subroutine coriolis_rr_jml_sub
    
    module pure subroutine buoy_rr_jml_sub(this, ir, T, force)
      class(T_physicalObject), intent(in)    :: this
      integer,                 intent(in)    :: ir
      complex(kind=dbl),       intent(in)    :: T(*)
      complex(kind=dbl),       intent(inout) :: force(2,*)
    end subroutine buoy_rr_jml_sub
    
    module pure function mat_temp_fn(this, j, a) result(matica)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: j
      real(kind=dbl),          intent(in) :: a
      real(kind=dbl),        allocatable  :: matica(:,:)
    end function mat_temp_fn
    
    module pure function mat_torr_fn(this, j, a) result(matica)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: j
      real(kind=dbl),          intent(in) :: a
      real(kind=dbl),        allocatable  :: matica(:,:)
    end function mat_torr_fn
    
    module pure function mat_mech_fn(this, j, a) result(matica)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: j
      real(kind=dbl),          intent(in) :: a
      real(kind=dbl),        allocatable  :: matica(:,:)
    end function mat_mech_fn
    
    module subroutine init_eq_temp_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine init_eq_temp_sub
    
    module subroutine init_eq_torr_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine init_eq_torr_sub
    
    module subroutine init_eq_mech_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine init_eq_mech_sub
    
    module subroutine prepare_mat_temp_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine prepare_mat_temp_sub
    
    module subroutine prepare_mat_torr_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine prepare_mat_torr_sub
    
    module subroutine prepare_mat_mech_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine prepare_mat_mech_sub
    
    module subroutine solve_temp_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine solve_temp_sub
    
    module subroutine solve_torr_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine solve_torr_sub
    
    module subroutine solve_mech_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine solve_mech_sub
    
    module pure real(kind=dbl) function nuss_fn(this)
      class(T_physicalObject), intent(in) :: this
    end function nuss_fn
    
    module real(kind=dbl) function reynolds_fn(this, choice)
      class(T_physicalObject), intent(in)           :: this
      character(len=*),        intent(in), optional :: choice
    end function reynolds_fn
    
    module real(kind=dbl) function reynolds_poloidal_fn(this)
      class(T_physicalObject), intent(in) :: this
    end function reynolds_poloidal_fn

    module real(kind=dbl) function reynolds_torroidal_fn(this)
      class(T_physicalObject), intent(in) :: this
    end function reynolds_torroidal_fn

    module real(kind=dbl) function temperature_fn(this)
      class(T_physicalObject), intent(in) :: this
    end function temperature_fn
    
    module real(kind=dbl) function laws_temp_fn(this)
      class(T_physicalObject), intent(in) :: this
    end function laws_temp_fn
  end interface

end module physicalobject