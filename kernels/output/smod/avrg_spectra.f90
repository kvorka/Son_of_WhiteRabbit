submodule (output_mod) avrg_spectra
  implicit none
  
  real(kind=dbl), parameter :: dtime = one / ( avrg_end - avrg_start )
  
  contains
  
  module procedure avrg_spectra_2d_sub
    integer                        :: in
    complex(kind=dbl), allocatable :: spectra(:)
    
    allocate( spectra(njm) )
    
    do in = avrg_start, avrg_end
      call read_2d_binfile_sub( 7, opt//trim(adjustl(int2str_fn(in)))//'.dat', njm, spectra )
      call copy3_carray_sub( njm, dtime, spectra, spectra_out )
    end do
    
    deallocate( spectra )
    
  end procedure avrg_spectra_2d_sub
  
  module procedure avrg_spectra_3d_sub
    integer                        :: in, ir
    complex(kind=dbl), allocatable :: spectra(:,:)
    
    allocate( spectra(njm,nr) )
    
    do in = avrg_start, avrg_end
      call read_3d_binfile_sub( 7, opt//trim(adjustl(int2str_fn(in)))//'.dat', njm*nr, spectra, nr, r_out )
      
      !$omp parallel do
      do ir = 1, nr
        call copy3_carray_sub( njm, dtime, spectra(1,ir), spectra_out(1,ir) )
      end do
      !$omp end parallel do
    end do
    
    deallocate( spectra )
    
  end procedure avrg_spectra_3d_sub
  
end submodule avrg_spectra