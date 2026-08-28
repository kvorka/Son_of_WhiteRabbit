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
    
    procedure, public,  pass :: init_sub       => init_lege_sub
    procedure, public,  pass :: deallocate_sub => deallocate_lege_sub
    
    procedure, private, pass :: roots_sub      => find_roots_sub
    procedure, private, pass :: coeffs_sub     => compute_coeffs_sub
    procedure, private, pass :: get_nma_sub    => get_nma_sub
    
    procedure, public, pass :: alloc_rscal_sub => allocate_rscalars_sub
    procedure, public, pass :: index_bwd_sub   => c2r_mj_to_mj_sub
    procedure, public, pass :: index_fwd_sub   => r2c_mj_to_mj_sub
    
    procedure, public, pass :: bwd_legesum_sub
    procedure, public, pass :: fwd_legesum_sub
    
  end type T_legep
  
  interface
    module real(kind=qbl) function lege_fn(deg, x)
      integer,        intent(in) :: deg
      real(kind=qbl), intent(in) :: x
    end function lege_fn
    
    module subroutine init_lege_sub(this, jmax, nLege, wfac)
      class(T_legep), intent(inout) :: this
      integer,        intent(in)    :: jmax, nLege
      real(kind=dbl), intent(in)    :: wfac
    end subroutine init_lege_sub
    
    module subroutine deallocate_lege_sub(this)
      class(T_legep), intent(inout) :: this
    end subroutine deallocate_lege_sub
    
    module subroutine find_roots_sub(this)
      class(T_legep), intent(inout) :: this
    end subroutine find_roots_sub
    
    module subroutine compute_coeffs_sub(this)
      class(T_legep), intent(inout) :: this
    end subroutine compute_coeffs_sub
    
    module subroutine get_nma_sub(this)
      class(T_legep), intent(inout) :: this
    end subroutine get_nma_sub
    
    module subroutine allocate_rscalars_sub(this, ns, c_rscal, rscal)
      class(T_legep),                      intent(in)  :: this
      integer,                             intent(in)  :: ns
      type(c_ptr),                         intent(out) :: c_rscal
      real(kind=dbl), pointer, contiguous, intent(out) :: rscal(:)
    end subroutine allocate_rscalars_sub
    
    module subroutine c2r_mj_to_mj_sub(this, ncab, cab, rcab)
      class(T_legep),    intent(in)  :: this
      integer,           intent(in)  :: ncab
      complex(kind=dbl), intent(in)  :: cab(ncab,*)
      real(kind=dbl),    intent(out) :: rcab(2,ncab,2,*)
    end subroutine c2r_mj_to_mj_sub
    
    module subroutine r2c_mj_to_mj_sub(this, ncab, cab, rcab)
      class(T_legep),    intent(in)  :: this
      integer,           intent(in)  :: ncab
      real(kind=dbl),    intent(in)  :: rcab(2,ncab,2,*)
      complex(kind=dbl), intent(out) :: cab(ncab,*)
    end subroutine r2c_mj_to_mj_sub
    
    module subroutine bwd_legesum_sub(this, nb, cc, sumN, sumS, cosx, sinx, cosx2, pmm, pmj1, pmj, swork)
      class(T_legep), intent(in)  :: this
      integer,        intent(in)  :: nb
      real(kind=dbl), intent(in)  :: cosx(*), sinx(*), cosx2(*), cc(4*nb,*)
      real(kind=dbl), intent(out) :: pmm(*), pmj1(*), pmj(*), swork(*), sumN(8*nb*ndbl,0:*), sumS(8*nb*ndbl,0:*)
    end subroutine bwd_legesum_sub
    
    module subroutine fwd_legesum_sub(this, nf, sumN, sumS, cr, cosx, sinx, cosx2, weight, pmm, pmj1, pmj, swork)
      class(T_legep), intent(in)    :: this
      integer,        intent(in)    :: nf
      real(kind=dbl), intent(in)    :: sumN(8*nf*ndbl,0:*), sumS(8*nf*ndbl,0:*), cosx(*), sinx(*), cosx2(*), weight(*)
      real(kind=dbl), intent(out)   :: pmm(*), pmj1(*), pmj(*), swork(*)
      real(kind=dbl), intent(inout) :: cr(4*nf,*)
    end subroutine fwd_legesum_sub
  end interface
  
  interface
#if defined ( kernelC )
    module subroutine bwd_idx_sub(n, cff, cab, rcab) bind(C, name="bwd_idx_c")
      integer, value,    intent(in)  :: n
      real(kind=dbl),    intent(in)  :: cff(*)
      complex(kind=dbl), intent(in)  :: cab(*)
      real(kind=dbl),    intent(out) :: rcab(*)
    end subroutine bwd_idx_sub
    
    module subroutine fwd_idx_sub(n, cff, rcab, cab) bind(C, name="fwd_idx_c")
      integer, value,    intent(in)  :: n
      real(kind=dbl),    intent(in)  :: cff(2)
      real(kind=dbl),    intent(in)  :: rcab(2,n,4)
      complex(kind=dbl), intent(out) :: cab(n,2)
    end subroutine fwd_idx_sub
    
    module subroutine bwd_set_sub(n, ma1, cff, cosx, sinx, cc, pmm, pmj1, pmj, swork) bind(C, name="bwd_set_c")
      integer, value, intent(in)    :: n, ma1
      real(kind=dbl), intent(in)    :: cff, cosx(*), sinx(*), cc(*)
      real(kind=dbl), intent(inout) :: pmm(*)
      real(kind=dbl), intent(out)   :: pmj1(*), pmj(*), swork(*)
    end subroutine bwd_set_sub
    
    module subroutine bwd_rec_sub(n, nma, fmj, cosx2, cc, pmj1, pmj, swork) bind(C, name="bwd_rec_c")
      integer, value, intent(in)    :: n, nma
      real(kind=dbl), intent(in)    :: fmj(*), cosx2(*), cc(*)
      real(kind=dbl), intent(inout) :: pmj1(*), pmj(*), swork(*)
    end subroutine bwd_rec_sub
    
    module subroutine bwd_rsc_sub(n, cosx, swork, sumN, sumS) bind(C, name="bwd_rsc_c")
      integer, value, intent(in)  :: n
      real(kind=dbl), intent(in)  :: cosx(*), swork(*)
      real(kind=dbl), intent(out) :: sumN(*), sumS(*)
    end subroutine bwd_rsc_sub
    
    module subroutine fwd_rsc_sub(n, w, cosx, sumN, sumS, swork) bind(C, name="fwd_rsc_c")
      integer, value, intent(in)  :: n
      real(kind=dbl), intent(in)  :: w(*), cosx(*), sumN(*), sumS(*)
      real(kind=dbl), intent(out) :: swork(*)
    end subroutine fwd_rsc_sub
    
    module subroutine fwd_set_sub(n, ma1, cff, cosx, sinx, swork, pmm, pmj1, pmj, cr) bind(C, name="fwd_set_c")
      integer, value, intent(in)    :: n, ma1
      real(kind=dbl), intent(in)    :: cff, cosx(*), sinx(*), swork(*)
      real(kind=dbl), intent(out)   :: pmj1(*), pmj(*)
      real(kind=dbl), intent(inout) :: pmm(*), cr(*)
    end subroutine fwd_set_sub
    
    module subroutine fwd_rec_sub(n, nma, fmj, cosx2, swork, pmj1, pmj, cr) bind(C, name="fwd_rec_c")
      integer, value, intent(in)    :: n, nma
      real(kind=dbl), intent(in)    :: fmj(*), cosx2(*), swork(*)
      real(kind=dbl), intent(inout) :: pmj1(*), pmj(*), cr(*)
    end subroutine fwd_rec_sub
#else
    module subroutine bwd_idx_sub(n, cff, cab, rcab)
      integer,           intent(in)  :: n
      real(kind=dbl),    intent(in)  :: cff(2)
      complex(kind=dbl), intent(in)  :: cab(n,3)
      real(kind=dbl),    intent(out) :: rcab(2,n,2)
    end subroutine bwd_idx_sub
    
    module subroutine fwd_idx_sub(n, cff, rcab, cab)
      integer,           intent(in)  :: n
      real(kind=dbl),    intent(in)  :: cff(2)
      real(kind=dbl),    intent(in)  :: rcab(2,n,4)
      complex(kind=dbl), intent(out) :: cab(n,2)
    end subroutine fwd_idx_sub
    
    module subroutine bwd_set_sub(n, ma1, cff, cosx, sinx, cc, pmm, pmj1, pmj, swork)
      integer,        intent(in)    :: n, ma1
      real(kind=dbl), intent(in)    :: cff, cosx(ndbl,4), sinx(ndbl,4), cc(4,n)
      real(kind=dbl), intent(inout) :: pmm(ndbl,4)
      real(kind=dbl), intent(out)   :: pmj1(ndbl,4), pmj(ndbl,4), swork(ndbl,4,4,n)
    end subroutine bwd_set_sub
    
    module subroutine bwd_rec_sub(n, nma, fmj, cosx2, cc, pmj1, pmj, swork)
      integer,        intent(in)    :: n, nma
      real(kind=dbl), intent(in)    :: fmj(3,nma), cosx2(ndbl,4), cc(4,n,nma)
      real(kind=dbl), intent(inout) :: pmj1(ndbl,4), pmj(ndbl,4)
      real(kind=dbl), intent(out)   :: swork(ndbl,4,4,n)
    end subroutine bwd_rec_sub
    
    module subroutine bwd_rsc_sub(n, cosx, swork, sumN, sumS)
      integer,        intent(in)  :: n
      real(kind=dbl), intent(in)  :: cosx(ndbl,4), swork(ndbl,4,2,n,2)
      real(kind=dbl), intent(out) :: sumN(ndbl,4,n,2), sumS(ndbl,4,n,2)
    end subroutine bwd_rsc_sub
    
    module subroutine fwd_rsc_sub(n, w, cosx, sumN, sumS, swork)
      integer,        intent(in)  :: n
      real(kind=dbl), intent(in)  :: w(ndbl,4), cosx(ndbl,4), sumN(ndbl,4,n,2), sumS(ndbl,4,n,2)
      real(kind=dbl), intent(out) :: swork(ndbl,4,2,n,2)
    end subroutine fwd_rsc_sub
    
    module subroutine fwd_set_sub(n, ma1, cff, cosx, sinx, swork, pmm, pmj1, pmj, cr)
      integer,        intent(in)    :: n, ma1
      real(kind=dbl), intent(in)    :: cff, cosx(ndbl,n), sinx(ndbl,n), swork(ndbl,4,4,n)
      real(kind=dbl), intent(out)   :: pmj1(ndbl,n), pmj(ndbl,n)
      real(kind=dbl), intent(inout) :: pmm(ndbl,n), cr(4,n)
    end subroutine fwd_set_sub
    
    module subroutine fwd_rec_sub(n, nma, fmj, cosx2, swork, pmj1, pmj, cr)
      integer,        intent(in)    :: n, nma
      real(kind=dbl), intent(in)    :: fmj(3,nma), cosx2(ndbl,n), swork(ndbl,4,4,n)
      real(kind=dbl), intent(inout) :: pmj1(ndbl,4), pmj(ndbl,4), cr(4,n,nma)
    end subroutine fwd_rec_sub
#endif
  end interface
  
end module lege_poly
