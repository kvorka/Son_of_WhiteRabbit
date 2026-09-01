submodule (lateral_grid) transform
  implicit none; contains
  
  module procedure transform_sub
    integer                             :: nGrid, nLege, nSwork, itheta
    real(kind=dbl), pointer, contiguous :: pmm(:), pmj1(:), pmj(:), cosx(:), sinx(:), cosx2(:), wght(:), &
                                         & sumN(:), sumS(:), swork(:), gridN(:), gridS(:)
    
    !! Size of the grid and number of non-zero frequencies
    nGrid  =     nb * step * ( this%fft%n      )
    nLege  = 2 * nb * step * ( this%lgp%jmax+1 )
    nSwork = 4 * nb * step
    
    !! Work memory :: sumN and sumS are basically phi-grids and need to be saved. Anything else is pointed into zero 
    !! bytes of the grids, which are used only for dealiasing and are needed just once. This code wont work for small jmax.
    !! The root-related things, line sinx, cosx ... have their space allocated after the work(2*nGrid) by construction and
    !! God forbid overwritting these bytes with something else within the cycle.
    sumN  => work(       1 :   nGrid )
    sumS  => work( nGrid+1 : 2*nGrid )
    
    sinx  => work( 2*nGrid + 0*step + 1 : 2*nGrid + 1*step )
    cosx  => work( 2*nGrid + 1*step + 1 : 2*nGrid + 2*step )
    wght  => work( 2*nGrid + 2*step + 1 : 2*nGrid + 3*step )
    cosx2 => work( 2*nGrid + 3*step + 1 : 2*nGrid + 4*step )
    
    swork => sumN( nLege              +1 : nLege+nSwork        )
    pmm   => sumN( nLege+nSwork       +1 : nLege+nSwork+  step )
    pmj   => sumN( nLege+nSwork+  step+1 : nLege+nSwork+2*step )
    pmj1  => sumN( nLege+nSwork+2*step+1 : nLege+nSwork+3*step )
    
    gridN => sumS( nLege+1:nLege+nb*step )
    gridS => sumN( nLege+1:nLege+nb*step )
    
    !! Reindex and rescale the input and zero the output, where a sum over latitudes will be stored
    call this%lgp%index_bwd_sub( nb, cc, rcc )
    call zero_rarray_sub( 0, 4*nf*this%lgp%nrma, rcr )
    
    !! Cycle over latitudes :: calculating step at once
    do itheta = 1, (this%lgp%nLege/step)*step, step
      !! The values of sinx, wght, cosx and cosx2 are small arrays and are aligned by construction, however, alocatted
      !! by master thread. Simple copy to the private threads is therefore done.
      call copy_rarray_sub( itheta, step, this%lgp%sinx,  sinx  )
      call copy_rarray_sub( itheta, step, this%lgp%cosx,  cosx  )
      call copy_rarray_sub( itheta, step, this%lgp%wght,  wght  )
      call copy_rarray_sub( itheta, step, this%lgp%cosx2, cosx2 )
      
      !! Synthesis of associated Legendre polynomials over degrees
      call this%lgp%bwd_legesum_sub( nb, rcc, sumN, sumS, cosx, sinx, cosx2, pmm, pmj1, pmj, swork )
      
      !! Northern hemisphere :: synthesis over orders (fft) followed by grid computations and analysis (fft) back 
      !! to orders. Keep in mind, that few last frequencies are used only for dealiasing and need to be set to zero 
      !! before fft.
      call zero_rarray_sub( nLege, nGrid, sumN )
      
      call this%fft%fft_c2r_sub( nb, sumN )
      call g_sub( this%fft%n, sumN, gridN )
      call this%fft%fft_r2c_sub( nf, sumN )
      
      !! Southern hemisphere :: synthesis over orders (fft) followed by grid computations and analysis (fft) back 
      !! to orders. Keep in mind, that few last frequencies are used only for dealiasing and need to be set to zero 
      !! before fft.
      call zero_rarray_sub( nLege, nGrid, sumS )
      
      call this%fft%fft_c2r_sub( nb, sumS )
      call g_sub( this%fft%n, sumS, gridS )
      call this%fft%fft_r2c_sub( nf, sumS )
      
      !! Analysis of associated Legendre polynomials into degrees
      call this%lgp%fwd_legesum_sub( nf, sumN, sumS, rcr, cosx, sinx, cosx2, wght, pmm, pmj1, pmj, swork )
    end do
    
    !! Reindex and rescale the output
    call this%lgp%index_fwd_sub( nf, cr, rcr )
    
  end procedure transform_sub
  
end submodule transform