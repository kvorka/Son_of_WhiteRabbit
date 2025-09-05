module lateral_grid
  use math
  use fourier_transform
  use lege_poly
  use sphsvt
  use grid_ops
  implicit none
  
  integer, parameter :: addmissible_jmax(47) = [   5,   7,   9,  13,  15,  21,  27,  29,  33,  37,  45,  47, &
                                               &  51,  57,  61,  69,  77,  87,  93,  97, 105, 117, 125, 141, &
                                               & 147, 157, 159, 177, 189, 197, 213, 237, 247, 253, 267, 285, &
                                               & 297, 317, 321, 357, 381, 397, 429, 447, 477, 497, 1021 ]
  
  type, public :: T_lateralGrid
    type(T_legep),  public :: lgp
    type(T_fft),    public :: fft
    type(T_sphsvt), public :: rxd
    
    contains
    
    procedure :: init_sub       => init_harmonics_sub
    procedure :: deallocate_sub => deallocate_harmonics_sub
    
    procedure :: transform_sub
    procedure :: vcvv_sub, vcvv_vcvgv_sub, vcvv_vcvxv_sub
    
  end type T_lateralGrid
  
  interface
    module subroutine init_harmonics_sub(this, jmax)
      class(T_lateralGrid), intent(inout) :: this
      integer,              intent(in)    :: jmax
    end subroutine init_harmonics_sub
    
    module subroutine deallocate_harmonics_sub(this)
      class(T_lateralGrid), intent(inout) :: this
    end subroutine deallocate_harmonics_sub
    
    module subroutine transform_sub(this, nf, nb, cc, cr, grid_sub)
      class(T_lateralGrid), intent(in)    :: this
      integer,              intent(in)    :: nf, nb
      complex(kind=dbl),    intent(in)    :: cc(nb,*)
      complex(kind=dbl),    intent(inout) :: cr(nf,*)
      
      interface
        subroutine grid_sub(nfour, gxyz, gtemp); import :: dbl
          integer,                intent(in)    :: nfour
          real(kind=dbl), target, intent(inout) :: gxyz(16,*)
          real(kind=dbl), target, intent(out)   :: gtemp(16,*)
        end subroutine grid_sub
      end interface
    end subroutine transform_sub
    
    module subroutine vcvv_sub(this, cajml, cbjml, cjm)
      class(T_lateralGrid), intent(in)  :: this
      complex(kind=dbl),    intent(in)  :: cajml(*), cbjml(*)
      complex(kind=dbl),    intent(out) :: cjm(*)
    end subroutine vcvv_sub
    
    module subroutine vcvv_vcvgv_sub(this, ri, q, dv_r, v, cjm)
      class(T_lateralGrid), intent(in)  :: this
      real(kind=dbl),       intent(in)  :: ri
      complex(kind=dbl),    intent(in)  :: dv_r(*), q(*), v(*)
      complex(kind=dbl),    intent(out) :: cjm(*)
    end subroutine vcvv_vcvgv_sub
    
    module subroutine vcvv_vcvxv_sub(this, q, curlv, v, cjm)
      class(T_lateralGrid), intent(in)  :: this
      complex(kind=dbl),    intent(in)  :: curlv(*), q(*), v(*)
      complex(kind=dbl),    intent(out) :: cjm(*)
    end subroutine vcvv_vcvxv_sub
  end interface
  
end module lateral_grid
