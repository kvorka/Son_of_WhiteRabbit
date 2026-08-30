module lateral_grid
  use math
  use lege_poly
  use fourier_transform
  implicit none
  
  !! Everything is build around two assumptions: (1) number of quadrature points is divisible by 4 * register length (step) and
  !! (2) the number of FFT points consists of only 2, 3 and 5 prime factors.
  integer, parameter :: step = 4 * ndbl
  integer, parameter :: addmissible_jmax(47) = [   5,   7,   9,  13,  15,  21,  27,  29,  33,  37,  45,  47,  51,  57,   61,  69, & 
                                               &  77,  87,  93,  97, 105, 117, 125, 141, 147, 157, 159, 177, 189, 197,  213, 237, &
                                               & 247, 253, 267, 285, 297, 317, 321, 357, 381, 397, 429, 447, 477, 497, 1021       ]
  
  type, public :: T_lateralGrid
    type(T_legep), public :: lgp
    type(T_fft),   public :: fft
    
    contains
    
    procedure :: init_sub       => init_harmonics_sub
    procedure :: alloc_work_lgrid_sub
    procedure :: transform_sub
    procedure :: deallocate_sub => deallocate_harmonics_sub
    
  end type T_lateralGrid
  
  interface
    module subroutine init_harmonics_sub(this, jmax)
      class(T_lateralGrid), intent(inout) :: this
      integer,              intent(in)    :: jmax
    end subroutine init_harmonics_sub
    
    module subroutine deallocate_harmonics_sub(this)
      class(T_lateralGrid), intent(inout) :: this
    end subroutine deallocate_harmonics_sub
    
    module subroutine alloc_work_lgrid_sub(this, nb, c_work, work)
      class(T_lateralGrid),                intent(in)  :: this
      integer,                             intent(in)  :: nb
      type(c_ptr),                         intent(out) :: c_work
      real(kind=dbl), pointer, contiguous, intent(out) :: work(:)
    end subroutine alloc_work_lgrid_sub
    
    module subroutine transform_sub(this, nf, nb, cc, cr, rcc, rcr, work, g_sub)
      class(T_lateralGrid),   intent(in)    :: this
      integer,                intent(in)    :: nf, nb
      complex(kind=dbl),      intent(in)    :: cc(nb,*)
      complex(kind=dbl),      intent(inout) :: cr(nf,*)
      real(kind=dbl),         intent(out)   :: rcc(*)
      real(kind=dbl),         intent(out)   :: rcr(*)
      real(kind=dbl), target, intent(out)   :: work(*)
      
      interface
        module subroutine g_sub(nfour, gxyz, gtemp)
          integer,        intent(in)    :: nfour
          real(kind=dbl), intent(inout) :: gxyz(ndbl,4,0:*)
          real(kind=dbl), intent(out)   :: gtemp(ndbl,4,0:*)
        end subroutine g_sub
      end interface
    end subroutine transform_sub
  end interface
  
end module lateral_grid
