submodule (output_mod) save_snpsht
  implicit none; contains
  
  subroutine save_snpsht_flux_sub()
    complex(kind=dbl), allocatable :: flux(:)
    
    allocate( flux(jms) )
    
    call read_2d_binfile_sub( 7, path_ocean_flux//trim(adjustl(int2str_fn(snpsht1)))//'.dat', jms, flux )
    call out_spectra_2d_sub('flux-snpsht1.spec', flux)
    
    call read_2d_binfile_sub( 7, path_ocean_flux//trim(adjustl(int2str_fn(snpsht2)))//'.dat', jms, flux )
    call out_spectra_2d_sub('flux-snpsht2.spec', flux)
    
    call read_2d_binfile_sub( 7, path_ocean_flux//trim(adjustl(int2str_fn(snpsht3)))//'.dat', jms, flux )
    call out_spectra_2d_sub('flux-snpsht3.spec', flux)
    
    deallocate( flux )
    
  end subroutine save_snpsht_flux_sub
  
  subroutine save_snpsht_temp_sub()
    real(kind=dbl),    allocatable :: r(:)
    complex(kind=dbl), allocatable :: temp(:,:)
    
    allocate( r(nd), temp(jms,nd) )
    
    call read_3d_binfile_sub( 7, path_ocean_temp//trim(adjustl(int2str_fn(snpsht1)))//'.dat', jms*nd, temp, nd, r )
    call out_spectra_3d_sub('temp-snpsht1.spec', r, temp)
    
    call read_3d_binfile_sub( 7, path_ocean_temp//trim(adjustl(int2str_fn(snpsht2)))//'.dat', jms*nd, temp, nd, r )
    call out_spectra_3d_sub('temp-snpsht2.spec', r, temp)
    
    call read_3d_binfile_sub( 7, path_ocean_temp//trim(adjustl(int2str_fn(snpsht3)))//'.dat', jms*nd, temp, nd, r )
    call out_spectra_3d_sub('temp-snpsht3.spec', r, temp)
    
    deallocate( r, temp )
    
  end subroutine save_snpsht_temp_sub
  
  subroutine save_snpsht_velc_sub()
    real(kind=dbl),    allocatable :: r(:)
    complex(kind=dbl), allocatable :: velc(:,:)
    
    allocate( r(nd), velc(jmv,nd) )
    
    call read_3d_binfile_sub( 7, path_ocean_velc//trim(adjustl(int2str_fn(snpsht1)))//'.dat', jmv*nd, velc, nd, r )
    call out_spectra_3d_sub('velc-snpsht1.spec', r, velc)
    
    call read_3d_binfile_sub( 7, path_ocean_velc//trim(adjustl(int2str_fn(snpsht2)))//'.dat', jmv*nd, velc, nd, r )
    call out_spectra_3d_sub('velc-snpsht2.spec', r, velc)
    
    call read_3d_binfile_sub( 7, path_ocean_velc//trim(adjustl(int2str_fn(snpsht3)))//'.dat', jmv*nd, velc, nd, r )
    call out_spectra_3d_sub('velc-snpsht3.spec', r, velc)
    
    deallocate( r, velc )
    
  end subroutine save_snpsht_velc_sub
  
end submodule save_snpsht