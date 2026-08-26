submodule (output_mod) save_spectra
  implicit none; contains
  
  subroutine save_spectra_flux_sub()
    complex(kind=dbl), allocatable :: flux(:)
    
    allocate( flux(jms) ); call zero_carray_sub( jms, flux )
    
    call avrg_spectra_2d_sub( path_ocean_flux, jms, flux )
    call out_spectra_2d_sub( 'flux-averaged.spec', flux )
    
    deallocate( flux )
    
  end subroutine save_spectra_flux_sub
  
  subroutine save_spectra_temp_sub()
    real(kind=dbl),    allocatable :: r(:)
    complex(kind=dbl), allocatable :: temp(:,:)
    
    allocate( r(nd), temp(jms,nd) )
    call zero_carray_sub( nd*jms, temp )
    
    call avrg_spectra_3d_sub( path_ocean_temp, nd, jms, r, temp )
    call out_spectra_3d_sub( 'temp-averaged.spec', r, temp )
    
    deallocate( r, temp )
    
  end subroutine save_spectra_temp_sub
  
  subroutine save_spectra_velc_sub()
    real(kind=dbl),    allocatable :: r(:)
    complex(kind=dbl), allocatable :: velc(:,:)
    
    allocate( r(nd), velc(jmv,nd) )
    call zero_carray_sub( jmv*nd, velc )
    
    call avrg_spectra_3d_sub( path_ocean_velc, nd, jmv, r, velc )
    call out_spectra_3d_sub( 'velc-averaged.spec', r, velc )
    
    deallocate( r, velc )
    
  end subroutine save_spectra_velc_sub
  
end submodule save_spectra