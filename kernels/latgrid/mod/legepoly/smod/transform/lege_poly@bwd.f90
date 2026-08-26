submodule (lege_poly) bwd
  implicit none; contains
  
  module procedure bwd_legesum_sub
    integer :: im, ima1, ima2
    
    do im = 0, this%jmax
      ima1 = this%mamj(im)
      ima2 = this%mamj(im+1)-1
      
      call bwd_set_sub( n     = nb,               &
                        ma1   = ima1,             &
                        cff   = this%fmj(1,ima1), &
                        cosx  = cosx,             &
                        sinx  = sinx,             &
                        cc    = cc(1,ima1),       &
                        pmm   = pmm,              &
                        pmj1  = pmj1,             &
                        pmj   = pmj,              &
                        swork = swork             )
      
      call bwd_rec_sub( n     = nb,                 &
                        nma   = ima2-ima1,          &
                        fmj   = this%fmj(1,ima1+1), &
                        cosx2 = cosx2,              &
                        cc    = cc(1,ima1+1),       &
                        pmj1  = pmj1,               &
                        pmj   = pmj,                &
                        swork = swork               )
      
      call bwd_rsc_sub( n     = nb,         &
                        cosx  = cosx,       &
                        swork = swork,      &
                        sumN  = sumN(1,im), &
                        sumS  = sumS(1,im)  )
    end do
    
  end procedure bwd_legesum_sub
  
end submodule bwd
