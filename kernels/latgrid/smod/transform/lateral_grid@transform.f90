submodule (lateral_grid) transform
  implicit none; contains
  
  module procedure transform_sub
    integer                             :: nGrid, nLege, nSwork, itheta, i1
    type(c_ptr)                         :: c_work, c_rcc, c_rcr
    real(kind=dbl), pointer, contiguous :: work(:), pmm(:), pmj1(:), pmj(:), cosx(:), sinx(:), cosx2(:), wght(:), &
                                         & sumN(:), sumS(:), swork(:), gridN(:), gridS(:), rcr(:), rcc(:)
    
    !! Size of the grid and number of non-zero frequencies
    nGrid  =     nb * step * ( this%fft%n      )
    nLege  = 2 * nb * step * ( this%lgp%jmax+1 )
    nSwork = 4 * nb * step
    
    !! Allocate input and output arrays
    call this%lgp%alloc_rscal_sub( nb, c_rcc, rcc )
    call this%lgp%alloc_rscal_sub( nf, c_rcr, rcr )
    
    !! Reindex and rescale the input
    call this%lgp%index_bwd_sub( nb, cc, rcc )
    
    !! Allocating work memory :: sumN and sumS are basically phi-grids and need to be saved. Anything else is pointed into zero 
    !! bytes of the grids, which are used only for dealiasing and are needed just once. This code wont work for small jmax.
    call alloc_aligned_sub( 2*nGrid, c_work, work )
      
      sumN  => work(       1 :   nGrid )
      sumS  => work( nGrid+1 : 2*nGrid )
      
      swork => sumN( nLege              +1 : nLege+nSwork        )
      pmm   => sumN( nLege+nSwork       +1 : nLege+nSwork+  step )
      pmj   => sumN( nLege+nSwork+  step+1 : nLege+nSwork+2*step )
      pmj1  => sumN( nLege+nSwork+2*step+1 : nLege+nSwork+3*step )
      
      gridN => sumS( nLege+1:nLege+nb*step )
      gridS => sumN( nLege+1:nLege+nb*step )
      
    !! Cycle over latitudes :: calculating step at once
    do itheta = 1, (this%lgp%nLege/step)*step, step
      !! The values of sinx, wght, cosx and cosx2 are small arrays and are aligned by construction. Simple pointers
      !! are set to their memory. This might not be optimal for threading.
      sinx  => this%lgp%sinx(itheta:itheta+step-1)
      cosx  => this%lgp%cosx(itheta:itheta+step-1)
      wght  => this%lgp%wght(itheta:itheta+step-1)
      cosx2 => this%lgp%cosx2(itheta:itheta+step-1)
      
      !! Synthesis of associated Legendre polynomials over degrees
      call this%lgp%bwd_legesum_sub( nb, rcc, sumN, sumS, cosx, sinx, cosx2, pmm, pmj1, pmj, swork )
      
      !! Northern hemisphere :: synthesis over orders (fft) followed by grid computations and analysis (fft) back 
      !! to orders. Keep in mind, that few last frequencies are used only for dealiasing and need to be set to zero 
      !! before the second fft.
      !$omp simd aligned (sumN:alig)
      do i1 = nLege+1, nGrid
        sumN(i1) = zero
      end do
      
      call this%fft%fft_c2r_sub( nb, sumN )
      call grid_sub( this%fft%n, sumN, gridN )
      call this%fft%fft_r2c_sub( nf, sumN )
      
      !! Southern hemisphere :: synthesis over orders (fft) followed by grid computations and analysis (fft) back 
      !! to orders. Keep in mind, that few last frequencies are used only for dealiasing and need to be set to zero 
      !! before the second fft.
      !$omp simd aligned (sumS:alig)
      do i1 = nLege+1, nGrid
        sumS(i1) = zero
      end do
      
      call this%fft%fft_c2r_sub( nb, sumS )
      call grid_sub( this%fft%n, sumS, gridS )
      call this%fft%fft_r2c_sub( nf, sumS )
      
      !! Analysis of associated Legendre polynomials into degrees
      call this%lgp%fwd_legesum_sub( nf, sumN, sumS, rcr, cosx, sinx, cosx2, wght, pmm, pmj1, pmj, swork )
    end do
    
    !! Reindex and rescale the output
    call this%lgp%index_fwd_sub( nf, cr, rcr )
    
    !! Cleaning after the computation
    call free_aligned_sub( c_work, work )
    call free_aligned_sub( c_rcc, rcc )
    call free_aligned_sub( c_rcr, rcr )
    
  end procedure transform_sub
  
end submodule transform