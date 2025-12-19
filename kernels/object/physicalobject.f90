module physicalobject
  use math
  use sph
  use lateral_grid
  use radial_grid
  use equations
  implicit none
  
  type, abstract, public :: T_physicalObject
    character(len=5) :: thermal_bnd, diffusion_type
    integer          :: nd, jmax, jms, jmv, n_iter, poc
    real(kind=dbl)   :: t, dt, cf, ab, r_ud, Pr, Ra, Ek, facPr, facRa, facEk
    
    type(T_radialGrid)             :: rad_grid
    type(T_lateralGrid)            :: lat_grid
    type(T_equations), allocatable :: temp(:), torr(:), mech(:)
    
    contains
    
    procedure, pass :: init_objects_sub       => init_objects_sub
    procedure, pass :: deallocate_objects_sub => deallocate_objects_sub
    
    procedure, pass :: temp_rr_fn, temp_rr_jm_sub, temp3_rr_jm_sub, temp4_rr_jm_sub, dT_dr_rr_jm_sub, gradT_rr_jml_sub,       &
                     & temp_r_fn, dT_dr_r_fn, dT_dr_r_jm_sub, velc_rr_jml_sub, velc3_rr_jml_sub, dv_dr_rr_jml_sub,            &
                     & curlv_rr_jml_sub, init_eq_all_sub, mat_temp_fn, mat_mech_fn, mat_torr_fn, prepare_mat_mech_sub,        &
                     & prepare_mat_temp_sub, prepare_mat_torr_sub, solve_temp_ij_sub, solve_torr_ij_sub, solve_mech_ij_sub,   &
                     & solve_all_sub, hdiff_fn, coriolis_rr_jml_sub, buoy_rr_jml_sub, vypis_sub, reynolds_fn, temperature_fn, &
                     & nuss_fn, reynolds_poloidal_fn, reynolds_torroidal_fn, write_binfile_sub
    
  end type T_physicalObject
  
  interface
    !! Interfaces :: initialization
    module subroutine init_objects_sub(this, nd, jmax, r_ud)
      class(T_physicalObject),    intent(inout) :: this
      integer,                    intent(in)    :: nd, jmax
      real(kind=dbl),             intent(in)    :: r_ud
    end subroutine init_objects_sub
    
    module subroutine deallocate_objects_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine deallocate_objects_sub
    
    !! Interfaces :: temperature on rr grid
    module complex(kind=dbl) function temp_rr_fn(this, ir, ij, im)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: ir, ij, im
    end function temp_rr_fn
    
    module subroutine temp_rr_jm_sub(this, ir, temp_jm)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: temp_jm(*)
    end subroutine temp_rr_jm_sub
    
    module subroutine temp3_rr_jm_sub(this, ir, temp1, temp2, temp3)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: temp1(*), temp2(*), temp3(*)
    end subroutine temp3_rr_jm_sub
    
    module subroutine temp4_rr_jm_sub(this, ir, temp1, temp2, temp3, temp4)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: temp1(*), temp2(*), temp3(*), temp4(*)
    end subroutine temp4_rr_jm_sub
    
    module subroutine dT_dr_rr_jm_sub(this, ir, T, dT)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: T(*), dT(*)
    end subroutine dT_dr_rr_jm_sub
    
    module subroutine gradT_rr_jml_sub(this, ir, T, gradT, sgn)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir, sgn
      complex(kind=dbl),       intent(out) :: T(*), gradT(*)
    end subroutine gradT_rr_jml_sub
    
    !! Interfaces :: temperature on r grid
    module complex(kind=dbl) function temp_r_fn(this, ir, ij, im)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: ir, ij, im
    end function temp_r_fn
    
    module complex(kind=dbl) function dT_dr_r_fn(this, ir, ij, im)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: ir, ij, im
    end function dT_dr_r_fn
    
    module subroutine dT_dr_r_jm_sub(this, ir, dT_dr_r)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: dT_dr_r(*)
    end subroutine dT_dr_r_jm_sub
    
    !! Interfaces :: velocity on rr grid
    module subroutine velc_rr_jml_sub(this, ir, v_jml)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: v_jml(*)
    end subroutine velc_rr_jml_sub
    
    module subroutine velc3_rr_jml_sub(this, ir, v1_jml, v2_jml, v3_jml)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: v1_jml(*), v2_jml(*), v3_jml(*)
    end subroutine velc3_rr_jml_sub
    
    module subroutine dv_dr_rr_jml_sub(this, ir, v, dv)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: dv(*), v(*)
    end subroutine dv_dr_rr_jml_sub
    
    module subroutine curlv_rr_jml_sub(this, ir, v, curlv)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: v(*), curlv(*)
    end subroutine curlv_rr_jml_sub
    
    !! Interfaces :: output
    module subroutine write_binfile_sub(this, filenum, filepath, arr, status)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: filenum
      character(len=*),        intent(in) :: filepath
      complex(kind=dbl),       intent(in) :: arr(*)
      character(len=*),        intent(in) :: status
    end subroutine write_binfile_sub
    
    module subroutine vypis_sub(this, filenum, path, quantity)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: filenum
      character(len=*),        intent(in) :: path, quantity
    end subroutine vypis_sub
    
    !! Interfaces :: forces
    module real(kind=dbl) function hdiff_fn(this, j)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: j
    end function hdiff_fn
    
    module subroutine coriolis_rr_jml_sub(this, v, coriolis)
      class(T_physicalObject), intent(in)    :: this
      complex(kind=dbl),       intent(in)    :: v(*)
      complex(kind=dbl),       intent(inout) :: coriolis(*)
    end subroutine coriolis_rr_jml_sub
    
    module subroutine buoy_rr_jml_sub(this, ir, T, nsph1, nsph2)
      class(T_physicalObject), intent(in)    :: this
      integer,                 intent(in)    :: ir
      complex(kind=dbl),       intent(in)    :: T(*)
      complex(kind=dbl),       intent(inout) :: nsph1(*), nsph2(*)
    end subroutine buoy_rr_jml_sub
    
    !! Interfaces :: matrices
    module function mat_temp_fn(this, j, a) result(matica)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: j
      real(kind=dbl),          intent(in) :: a
      real(kind=dbl),        allocatable  :: matica(:,:)
    end function mat_temp_fn
    
    module subroutine prepare_mat_temp_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine prepare_mat_temp_sub
    
    module function mat_torr_fn(this, j, a) result(matica)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: j
      real(kind=dbl),          intent(in) :: a
      real(kind=dbl),        allocatable  :: matica(:,:)
    end function mat_torr_fn
    
    module subroutine prepare_mat_torr_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine prepare_mat_torr_sub
    
    module function mat_mech_fn(this, j, a) result(matica)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: j
      real(kind=dbl),          intent(in) :: a
      real(kind=dbl),        allocatable  :: matica(:,:)
    end function mat_mech_fn
    
    module subroutine prepare_mat_mech_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine prepare_mat_mech_sub
    
    !! Interfaces :: init equations
    module subroutine init_eq_all_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine init_eq_all_sub
    
    !! Interfaces :: solvers
    module subroutine solve_temp_ij_sub(this, ij)
      class(T_physicalObject), intent(inout) :: this
      integer,                 intent(in)    :: ij
    end subroutine solve_temp_ij_sub
    
    module subroutine solve_torr_ij_sub(this, ij)
      class(T_physicalObject), intent(inout) :: this
      integer,                 intent(in)    :: ij
    end subroutine solve_torr_ij_sub
    
    module subroutine solve_mech_ij_sub(this, ij)
      class(T_physicalObject), intent(inout) :: this
      integer,                 intent(in)    :: ij
    end subroutine solve_mech_ij_sub
    
    module subroutine solve_all_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine solve_all_sub
    
    !! Interfaces :: diagnostics
    module real(kind=dbl) function nuss_fn(this)
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