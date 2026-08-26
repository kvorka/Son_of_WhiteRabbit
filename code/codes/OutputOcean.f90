program OutputOcean
  use output_mod
  implicit none
  
  call nuss_curve_sub()
  
  call save_snpsht_flux_sub()
  call save_snpsht_temp_sub()
  call save_snpsht_velc_sub()
  
  call save_spectra_flux_sub()
  call save_spectra_temp_sub()
  call save_spectra_velc_sub()
  
end program OutputOcean
