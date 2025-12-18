submodule (lateral_grid) transform
  implicit none; contains
  
  module procedure transform_sub
    integer                             :: itheta, i1
    type(c_ptr)                         :: c_work, c_rcc, c_rcr
    real(kind=dbl), pointer, contiguous :: work(:)
    real(kind=dbl), pointer, contiguous :: pmm(:), pmj2(:), pmj1(:), pmj(:)
    real(kind=dbl), pointer, contiguous :: cosx(:), sinx(:), cosx2(:), wght(:)
    real(kind=dbl), pointer, contiguous :: sumN(:), sumS(:), swork(:), grid(:)
    real(kind=dbl), pointer, contiguous :: rcr(:), rcc(:)
    
    !Prepare input and output arrays
    call this%lgp%alloc_rscal_sub( nb, c_rcc, rcc )
    call this%lgp%alloc_rscal_sub( nf, c_rcr, rcr )
    
    call this%lgp%index_bwd_sub( nb, cc, rcc )
    
    !Allocating memory
    call alloc_aligned1d_sub( (4*(nb+2)+nb+2*nb*this%fft%n)*16, c_work, work )
      
      cosx  => work(                                 1 :                                 16 )
      sinx  => work(                              16+1 :   2*                            16 )
      cosx2 => work(   2*                         16+1 :   3*                            16 )
      wght  => work(   3*                         16+1 :   4*                            16 )
      pmm   => work(   4*                         16+1 :   5*                            16 )
      pmj   => work(   5*                         16+1 :   6*                            16 )
      pmj1  => work(   6*                         16+1 :   7*                            16 )
      pmj2  => work(   7*                         16+1 :   8*                            16 )
      swork => work(   8*                         16+1 :   4*(nb+2)*                     16 )
      sumN  => work(   4*(nb+2)*                  16+1 : ( 4*(nb+2)+     nb*this%fft%n )*16 )
      sumS  => work( ( 4*(nb+2)+  nb*this%fft%n )*16+1 : ( 4*(nb+2)   +2*nb*this%fft%n )*16 )
      grid  => work( ( 4*(nb+2)+2*nb*this%fft%n )*16+1 : ( 4*(nb+2)+nb+2*nb*this%fft%n )*16 )
    
    !Cycle over latitudes :: calculating 16 at once
    do itheta = 1, (this%lgp%nLege/16)*16, 16
      !$omp simd
      do i1 = 1, 16
        cosx(i1)  = this%lgp%rw(itheta+i1-1,1)
        sinx(i1)  = this%lgp%rw(itheta+i1-1,2)
        cosx2(i1) = this%lgp%rw(itheta+i1-1,3)
        wght(i1)  = this%lgp%rw(itheta+i1-1,4)
      end do
      
      call zero_rarray_sub( nb*16*this%fft%n, sumN )
      call zero_rarray_sub( nb*16*this%fft%n, sumS )
      
      call this%lgp%bwd_legesum_sub( nb, rcc, sumN, sumS, cosx, sinx, cosx2, pmm, pmj2, pmj1, pmj, swork )
      
      call this%fft%fft_c2r_sub( nb, sumN )
      call this%fft%fft_c2r_sub( nb, sumS )
      
      call grid_sub( this%fft%n, sumS, grid )
      call grid_sub( this%fft%n, sumN, grid )
      
      call this%fft%fft_r2c_sub( nf, sumN )
      call this%fft%fft_r2c_sub( nf, sumS )
      
      call this%lgp%fwd_legesum_sub( nf, sumN, sumS, rcr, cosx, sinx, cosx2, wght, pmm, pmj2, pmj1, pmj, swork )
    end do
    
    call this%lgp%index_fwd_sub( nf, cr, rcr )
    
    !Cleaning
    call free_aligned1d_sub( c_work, work )
    call free_aligned1d_sub( c_rcc, rcc )
    call free_aligned1d_sub( c_rcr, rcr )
    
  end procedure transform_sub
  
end submodule transform