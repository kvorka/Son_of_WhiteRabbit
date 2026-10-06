submodule (lege_poly) bwd_sum
  implicit none; contains
  
  module procedure bwd_legesum_sub
    integer :: im, ima1, ima2
    
    do im = 0, this%jmax-1
      ima1 = this%mamj(im)
      ima2 = this%mamj(im+1)-1
      
      call bwd_m_sub( n     = nb,               &
                      m     = im,               &
                      nma   = ima2-ima1,        &
                      fmj   = this%fmj(1,ima1), &
                      cosx  = cosx,             &
                      sinx  = sinx,             &
                      cosx2 = cosx2,            &
                      cc    = cc(1,ima1),       &
                      pmm   = pmm,              &
                      pmj   = pmj,              &
                      pmj1  = pmj1,             &
                      swork = swork,            &
                      sumN  = sumN(1,im),       &
                      sumS  = sumS(1,im)        )
    end do
    
    im = this%jmax
      ima1 = this%mamj(im)
      
      call bwd_end_sub( n     = nb,               &
                        m     = im,               &
                        fmj   = this%fmj(1,ima1), &
                        cosx  = cosx,             &
                        sinx  = sinx,             &
                        cc    = cc(1,ima1),       &
                        pmm   = pmm,              &
                        pmj   = pmj,              &
                        pmj1  = pmj1,             &
                        swork = swork,            &
                        sumN  = sumN(1,im),       &
                        sumS  = sumS(1,im)        )
      
  end procedure bwd_legesum_sub
  
end submodule bwd_sum
