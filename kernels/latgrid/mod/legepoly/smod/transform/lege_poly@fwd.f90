submodule (lege_poly) fwd
  implicit none; contains

  module procedure fwd_legesum_sub
    integer :: im, ima1, ima2
    
    !! Recursion is needed only for orders lower 
    !! than maximum order
    do im = 0, this%jmax-1
      ima1 = this%mamj(im)
      ima2 = this%mamj(im+1)-1
      
      call fwd_rsc_sub( n     = nf,         &
                        w     = weight,     &
                        cosx  = cosx,       &
                        sumN  = sumN(1,im), &
                        sumS  = sumS(1,im), &
                        swork = swork       )
      
      call fwd_set_sub( n     = nf,               &
                        ma1   = ima1,             &
                        cff   = this%fmj(1,ima1), &
                        cosx  = cosx,             &
                        sinx  = sinx,             &
                        swork = swork,            &
                        pmm   = pmm,              &
                        pmj1  = pmj1,             &
                        pmj   = pmj,              &
                        cr    = cr(1,ima1)        )
      
      call fwd_rec_sub( n     = nf,                 &
                        nma   = ima2-ima1,          &
                        fmj   = this%fmj(1,ima1+1), &
                        cosx2 = cosx2,              &
                        swork = swork,              &
                        pmj1  = pmj1,               &
                        pmj   = pmj,                &
                        cr    = cr(1,ima1+1)        )
      
    end do
    
    !! For the last order, 
    !! only pmm recursion is needed
    im = this%jmax
      ima1 = this%mamj(im)
      
      call fwd_rsc_sub( n     = nf,         &
                        w     = weight,     &
                        cosx  = cosx,       &
                        sumN  = sumN(1,im), &
                        sumS  = sumS(1,im), &
                        swork = swork       )
      
      call fwd_set_sub( n     = nf,               &
                        ma1   = ima1,             &
                        cff   = this%fmj(1,ima1), &
                        cosx  = cosx,             &
                        sinx  = sinx,             &
                        swork = swork,            &
                        pmm   = pmm,              &
                        pmj1  = pmj1,             &
                        pmj   = pmj,              &
                        cr    = cr(1,ima1)        )
    
  end procedure fwd_legesum_sub
  
end submodule fwd