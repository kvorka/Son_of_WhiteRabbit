module physicalobject
  use math
  use sph
  use lateral_grid
  use radial_grid
  use equations
  use binaryio
  implicit none
  
  type, abstract, public :: T_physicalObject
    character(len=5) :: thermal_bnd, diffusion_type
    integer          :: nd, jmax, jms, jmv, n_iter, poc
    real(kind=dbl)   :: t, dt, cf, ab, r_ud, Pr, Ra, Ek
    
    type(T_radialGrid)             :: rad_grid
    type(T_lateralGrid)            :: lat_grid
    type(T_equations), allocatable :: temp(:), torr(:), mech(:)
    complex(kind=dbl), allocatable :: nsph1(:,:), nsph2(:,:), ntorr(:,:), ntemp(:,:)
    
    contains
    
    procedure, pass :: init_objects_sub       => init_objects_sub
    procedure, pass :: deallocate_objects_sub => deallocate_objects_sub
    
    procedure, pass :: temp_rr_fn, temp_rr_jm_sub, temp3_rr_jm_sub, temp4_rr_jm_sub, dT_dr_rr_jm_sub, gradT_ptp_rr_jm_sub,  &
                     & temp_r_fn, dT_dr_r_fn, dT_dr_r_jm_sub, velc_rr_jml_sub, dv_dr_ptp_rr_jm_sub, velc3_ptp_rr_jm_sub,    &
                     & curlv_ptp_rr_jm_sub, init_eq_all_sub, mat_temp_sub, mat_mech_sub, mat_torr_sub, prepare_mat_mech_sub,   &
                     & prepare_mat_temp_sub, prepare_mat_torr_sub, solve_temp_ij_sub, solve_torr_ij_sub, solve_mech_ij_sub, &
                     & solve_all_sub, init_nl_all_sub, hdiff_fn, buoy_rr_jml_sub, grad_ptp_sub, curl_ptp_sub, vypis_sub, &
                     & reynolds_fn, nuss_fn, deallocEqs_sub
    
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
      complex(kind=dbl),       intent(out) :: temp_jm(this%jms)
    end subroutine temp_rr_jm_sub
    
    module subroutine temp3_rr_jm_sub(this, ir, temp1, temp2, temp3)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: temp1(this%jms), temp2(this%jms), temp3(this%jms)
    end subroutine temp3_rr_jm_sub
    
    module subroutine temp4_rr_jm_sub(this, ir, temp1, temp2, temp3, temp4)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: temp1(this%jms), temp2(this%jms), temp3(this%jms), temp4(this%jms)
    end subroutine temp4_rr_jm_sub
    
    module subroutine dT_dr_rr_jm_sub(this, ir, T, dT)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: T(this%jms), dT(this%jms)
    end subroutine dT_dr_rr_jm_sub
    
    module subroutine gradT_ptp_rr_jm_sub(this, ir, T, gradT, sgn)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir, sgn
      complex(kind=dbl),       intent(out) :: T(this%jms), gradT(this%jms,3)
    end subroutine gradT_ptp_rr_jm_sub
    
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
      complex(kind=dbl),       intent(out) :: dT_dr_r(this%jms)
    end subroutine dT_dr_r_jm_sub
    
    !! Interfaces :: velocity on rr grid
    module subroutine velc_rr_jml_sub(this, ir, v_jml)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: v_jml(this%jmv)
    end subroutine velc_rr_jml_sub
    
    module subroutine velc3_ptp_rr_jm_sub(this, ir, v1, v2, v3)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: v1(this%jms,3), v2(this%jms,3), v3(this%jms,3)
    end subroutine velc3_ptp_rr_jm_sub
    
    module subroutine dv_dr_ptp_rr_jm_sub(this, ir, v, dv)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: dv(this%jms,3), v(this%jms,3)
    end subroutine dv_dr_ptp_rr_jm_sub
    
    module subroutine curlv_ptp_rr_jm_sub(this, ir, v, curlv)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(out) :: v(this%jms,3), curlv(this%jms,3)
    end subroutine curlv_ptp_rr_jm_sub
    
    !! Interfaces :: output
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
    
    module subroutine buoy_rr_jml_sub(this, ir, T, nsph1, nsph2)
      class(T_physicalObject), intent(in)    :: this
      integer,                 intent(in)    :: ir
      complex(kind=dbl),       intent(in)    :: T(this%jms)
      complex(kind=dbl),       intent(inout) :: nsph1(this%jms), nsph2(this%jms)
    end subroutine buoy_rr_jml_sub
    
    !! Interfaces :: operators
    module subroutine grad_ptp_sub(this, sgn, ir, T, dT_dr, gradT)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir, sgn
      complex(kind=dbl),       intent(in)  :: T(this%jms), dT_dr(this%jms)
      complex(kind=dbl),       intent(out) :: gradT(this%jms,3)
    end subroutine grad_ptp_sub
    
    module subroutine curl_ptp_sub(this, ir, v, dv_dr, curlv)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: ir
      complex(kind=dbl),       intent(in)  :: v(this%jms,3), dv_dr(this%jms,3)
      complex(kind=dbl),       intent(out) :: curlv(this%jms,3)
    end subroutine curl_ptp_sub
    
    !! Interfaces :: matrices
    module subroutine prepare_mat_temp_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine prepare_mat_temp_sub
    
    module subroutine mat_temp_sub(this, j, a, matica)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: j
      real(kind=dbl),          intent(in)  :: a
      real(kind=dbl),          intent(out) :: matica(7,2*this%nd+1)
    end subroutine mat_temp_sub
    
    module subroutine prepare_mat_torr_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine prepare_mat_torr_sub
    
    module subroutine mat_torr_sub(this, j, a, matica)
      class(T_physicalObject), intent(in)  :: this
      integer,                 intent(in)  :: j
      real(kind=dbl),          intent(in)  :: a
      real(kind=dbl),          intent(out) :: matica(7,2*this%nd+1)
    end subroutine mat_torr_sub
    
    module subroutine prepare_mat_mech_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine prepare_mat_mech_sub
    
    module subroutine mat_mech_sub(this, j, a, matica)
      class(T_physicalObject), intent(in) :: this
      integer,                 intent(in) :: j
      real(kind=dbl),          intent(in) :: a
      real(kind=dbl),          intent(out) :: matica(18,5*this%nd+2)
    end subroutine mat_mech_sub
    
    !! Interfaces :: init equations
    module subroutine init_eq_all_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine init_eq_all_sub
    
    !! Interfaces :: init non-linear terms
    module subroutine init_nl_all_sub(this)
      class(T_physicalObject), intent(inout) :: this
    end subroutine init_nl_all_sub
    
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
    
    !! Interfaces :: finalizer
    module subroutine deallocEqs_sub(this, eqs_array)
      class(T_physicalObject),        intent(inout) :: this
      type(T_equations), allocatable, intent(inout) :: eqs_array(:)
    end subroutine deallocEqs_sub
  end interface
  
  interface
#if defined ( kernelC )
    module subroutine copy4_carray_sub(length, fac1, fac2, fac3, arr1, arr2, arr_to) bind(C, name="copy4_carray_c")
      integer, value,    intent(in)    :: length
      real(kind=dbl),    intent(in)    :: fac1, fac2, fac3
      complex(kind=dbl), intent(in)    :: arr1(*), arr2(*)
      complex(kind=dbl), intent(inout) :: arr_to(*)
    end subroutine copy4_carray_sub
    
    module subroutine copy5_carray_sub(length, fac1, fac2, fac3, fac4, arr1, arr2, arr3, arr_to) bind(C, name="copy5_carray_c")
      integer, value,    intent(in)    :: length
      real(kind=dbl),    intent(in)    :: fac1, fac2, fac3, fac4
      complex(kind=dbl), intent(in)    :: arr1(*), arr2(*), arr3(*)
      complex(kind=dbl), intent(inout) :: arr_to(*)
    end subroutine copy5_carray_sub
    
    module subroutine grad_pp_j_sub(length, fac1, fac2, fac3, fac4, darr, arr, grad1, grad3) bind(C, name="grad_pp_j_c")
      integer, value,    intent(in)  :: length
      real(kind=dbl),    intent(in)  :: fac1, fac2, fac3, fac4
      complex(kind=dbl), intent(in)  :: darr(*), arr(*)
      complex(kind=dbl), intent(out) :: grad1(*), grad3(*)
    end subroutine grad_pp_j_sub
    
    module subroutine curl_ptp_j_sub(length, fac1, fac2, fac3, fac4, fac5, fac6, darr1, darr2, darr3, arr1, arr2, arr3, &
                                    & curl1, curl2, curl3) bind(C, name="curl_ptp_j_c")
      integer, value,    intent(in)  :: length
      real(kind=dbl),    intent(in)  :: fac1, fac2, fac3, fac4, fac5, fac6
      complex(kind=dbl), intent(in)  :: darr1(*), darr2(*), darr3(*), arr1(*), arr2(*), arr3(*)
      complex(kind=dbl), intent(out) :: curl1(*), curl2(*), curl3(*)
    end subroutine curl_ptp_j_sub
#else
    module subroutine copy4_carray_sub(length, fac1, fac2, fac3, arr1, arr2, arr_to)
      integer,           intent(in)    :: length
      real(kind=dbl),    intent(in)    :: fac1, fac2, fac3
      complex(kind=dbl), intent(in)    :: arr1(length), arr2(length)
      complex(kind=dbl), intent(inout) :: arr_to(length)
    end subroutine copy4_carray_sub
    
    module subroutine copy5_carray_sub(length, fac1, fac2, fac3, fac4, arr1, arr2, arr3, arr_to)
      integer,           intent(in)    :: length
      real(kind=dbl),    intent(in)    :: fac1, fac2, fac3, fac4
      complex(kind=dbl), intent(in)    :: arr1(length), arr2(length), arr3(length)
      complex(kind=dbl), intent(inout) :: arr_to(length)
    end subroutine copy5_carray_sub
    
    module subroutine grad_pp_j_sub(length, fac1, fac2, fac3, fac4, darr, arr, grad1, grad3)
      integer,           intent(in)  :: length
      real(kind=dbl),    intent(in)  :: fac1, fac2, fac3, fac4
      complex(kind=dbl), intent(in)  :: darr(length), arr(length)
      complex(kind=dbl), intent(out) :: grad1(length), grad3(length)
    end subroutine grad_pp_j_sub
    
    module subroutine curl_ptp_j_sub(length, fac1, fac2, fac3, fac4, fac5, fac6, darr1, darr2, darr3, arr1, arr2, arr3, &
                                    & curl1, curl2, curl3)
      integer,           intent(in)  :: length
      real(kind=dbl),    intent(in)  :: fac1, fac2, fac3, fac4, fac5, fac6
      complex(kind=dbl), intent(in)  :: darr1(length), darr2(length), darr3(length), arr1(length), arr2(length), arr3(length)
      complex(kind=dbl), intent(out) :: curl1(length), curl2(length), curl3(length)
    end subroutine curl_ptp_j_sub
#endif
  end interface

end module physicalobject