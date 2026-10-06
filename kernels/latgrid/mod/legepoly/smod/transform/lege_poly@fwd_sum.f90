submodule (lege_poly) fwd_sum
  implicit none; contains

  module procedure fwd_legesum_sub
    integer :: im, ima1, ima2
    
    do im = 0, this%jmax-1
      ima1 = this%mamj(im)
      ima2 = this%mamj(im+1)-1
      
      call fwd_m_sub( n      = nf,               &
                      m      = im,               &
                      nma    = ima2-ima1,        &
                      fmj    = this%fmj(1,ima1), &
                      cosx   = cosx,             &
                      sinx   = sinx,             &
                      cosx2  = cosx2,            &
                      weight = weight,           &
                      sumN   = sumN(1,im),       &
                      sumS   = sumS(1,im),       &
                      pmm    = pmm,              &
                      pmj    = pmj,              &
                      pmj1   = pmj1,             &
                      swork  = swork,            &
                      cr     = cr(1,ima1)        )
    end do
    
    im = this%jmax
      ima1 = this%mamj(im)
      
      call fwd_end_sub( n      = nf,               &
                        m      = im,               &
                        fmj    = this%fmj(1,ima1), &
                        cosx   = cosx,             &
                        sinx   = sinx,             &
                        weight = weight,           &
                        sumN   = sumN(1,im),       &
                        sumS   = sumS(1,im),       &
                        pmm    = pmm,              &
                        pmj    = pmj,              &
                        pmj1   = pmj1,             &
                        swork  = swork,            &
                        cr     = cr(1,ima1)        )
    
  end procedure fwd_legesum_sub
  
end submodule fwd_sum