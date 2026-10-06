module lege_poly
  use math
  implicit none
  
  type, public :: T_legep
    integer                             :: jmax, nLege, nrma
    integer,        allocatable         :: mamj(:)
    real(kind=dbl), allocatable         :: emj(:), fmj(:,:)
    real(kind=dbl), pointer, contiguous :: cosx(:), sinx(:), cosx2(:), wght(:)
    type(c_ptr)                         :: c_cosx, c_sinx, c_cosx2, c_wght
    
    contains
    
    procedure, public, pass :: init_sub => init_lege_sub
    procedure, public, pass :: index_bwd_sub, bwd_legesum_sub
    procedure, public, pass :: index_fwd_sub, fwd_legesum_sub
    procedure, public, pass :: deallocate_sub => deallocate_lege_sub
    
  end type T_legep
  
  !! Class routines
  interface
    module subroutine init_lege_sub(this, jmax, nLege, wfac)
      class(T_legep), intent(inout) :: this
      integer,        intent(in)    :: jmax, nLege
      real(kind=dbl), intent(in)    :: wfac
    end subroutine init_lege_sub
    
    module subroutine deallocate_lege_sub(this)
      class(T_legep), intent(inout) :: this
    end subroutine deallocate_lege_sub
    
    module subroutine index_bwd_sub(this, ncab, cab, rcab)
      class(T_legep),    intent(in)  :: this
      integer,           intent(in)  :: ncab
      complex(kind=dbl), intent(in)  :: cab(ncab,*)
      real(kind=dbl),    intent(out) :: rcab(2,ncab,2,*)
    end subroutine index_bwd_sub
    
    module subroutine bwd_legesum_sub(this, nb, cc, sumN, sumS, cosx, sinx, cosx2, pmm, pmj1, pmj, swork)
      class(T_legep), intent(in)  :: this
      integer,        intent(in)  :: nb
      real(kind=dbl), intent(in)  :: cosx(*), sinx(*), cosx2(*), cc(4*nb,*)
      real(kind=dbl), intent(out) :: pmm(*), pmj1(*), pmj(*), swork(*), sumN(8*nb*ndbl,0:*), sumS(8*nb*ndbl,0:*)
    end subroutine bwd_legesum_sub
    
    module subroutine index_fwd_sub(this, ncab, cab, rcab)
      class(T_legep),    intent(in)  :: this
      integer,           intent(in)  :: ncab
      real(kind=dbl),    intent(in)  :: rcab(2,ncab,2,*)
      complex(kind=dbl), intent(out) :: cab(ncab,*)
    end subroutine index_fwd_sub
    
    module subroutine fwd_legesum_sub(this, nf, sumN, sumS, cr, cosx, sinx, cosx2, weight, pmm, pmj1, pmj, swork)
      class(T_legep), intent(in)    :: this
      integer,        intent(in)    :: nf
      real(kind=dbl), intent(in)    :: sumN(8*nf*ndbl,0:*), sumS(8*nf*ndbl,0:*), cosx(*), sinx(*), cosx2(*), weight(*)
      real(kind=dbl), intent(out)   :: pmm(*), pmj1(*), pmj(*), swork(*)
      real(kind=dbl), intent(inout) :: cr(4*nf,*)
    end subroutine fwd_legesum_sub
  end interface
  
  !! Cores
  interface
    module subroutine bwd_m_sub(n, m, nma, fmj, cosx, sinx, cosx2, cc, pmm, pmj, pmj1, swork, sumN, sumS) &
    & bind(C, name="bwd_m_c")
      integer, value, intent(in)    :: n, m, nma
      real(kind=dbl), intent(in)    :: fmj(*), cosx(*), sinx(*), cosx2(*), cc(*)
      real(kind=dbl), intent(inout) :: pmm(*)
      real(kind=dbl), intent(out)   :: pmj(*), pmj1(*), swork(*), sumN(*), sumS(*)
    end subroutine bwd_m_sub
    
    module subroutine bwd_end_sub(n, m, fmj, cosx, sinx, cc, pmm, pmj, pmj1, swork, sumN, sumS) &
    & bind(C, name="bwd_end_c")
      integer, value, intent(in)    :: n, m
      real(kind=dbl), intent(in)    :: fmj(*), cosx(*), sinx(*), cc(*)
      real(kind=dbl), intent(inout) :: pmm(*)
      real(kind=dbl), intent(out)   :: pmj(*), pmj1(*), swork(*), sumN(*), sumS(*)
    end subroutine bwd_end_sub
    
    module subroutine fwd_m_sub(n, m, nma, fmj, cosx, sinx, cosx2, weight, sumN, sumS, pmm, pmj, pmj1, swork, cr) &
    & bind(C, name="fwd_m_c")
      integer, value, intent(in)    :: n, m, nma
      real(kind=dbl), intent(in)    :: fmj(*), cosx(*), sinx(*), cosx2(*), weight(*), sumN(*), sumS(*)
      real(kind=dbl), intent(inout) :: pmm(*), cr(*)
      real(kind=dbl), intent(out)   :: pmj(*), pmj1(*), swork(*)
    end subroutine fwd_m_sub
    
    module subroutine fwd_end_sub(n, m, fmj, cosx, sinx, weight, sumN, sumS, pmm, pmj, pmj1, swork, cr) &
    & bind(C, name="fwd_end_c")
      integer, value, intent(in)    :: n, m
      real(kind=dbl), intent(in)    :: fmj(*), cosx(*), sinx(*), weight(*), sumN(*), sumS(*)
      real(kind=dbl), intent(inout) :: pmm(*), cr(*)
      real(kind=dbl), intent(out)   :: pmj(*), pmj1(*), swork(*)
    end subroutine fwd_end_sub
  end interface
  
end module lege_poly