module output_mod
  use math
  use ocean_constants
  use binaryio
  use output_paths
  implicit none; public
  
  integer, parameter :: nd  = nd_ocean+1
  integer, parameter :: jms =       jmax_ocean * ( jmax_ocean+1 ) / 2 + jmax_ocean   + 1
  integer, parameter :: jmv = 3 * ( jmax_ocean * ( jmax_ocean+1 ) / 2 + jmax_ocean ) + 1
  
  interface
    module subroutine nuss_curve_sub()
    end subroutine nuss_curve_sub
    
    module subroutine avrg_spectra_2d_sub(opt, njm, spectra_out)
      character(len=*),  intent(in)  :: opt
      integer,           intent(in)  :: njm
      complex(kind=dbl), intent(out) :: spectra_out(*)
    end subroutine avrg_spectra_2d_sub
    
    module subroutine avrg_spectra_3d_sub(opt, nr, njm, r_out, spectra_out)
      character(len=*),  intent(in)  :: opt
      integer,           intent(in)  :: nr, njm
      real(kind=dbl),    intent(out) :: r_out(*)
      complex(kind=dbl), intent(out) :: spectra_out(njm,*)
    end subroutine avrg_spectra_3d_sub
    
    module subroutine out_spectra_2d_sub(opt, spectra_in)
      character(len=*),  intent(in)    :: opt
      complex(kind=dbl), intent(inout) :: spectra_in(:)
    end subroutine out_spectra_2d_sub
    
    module subroutine out_spectra_3d_sub(opt, r_in, data_in)
      character(len=*),  intent(in) :: opt
      real(kind=dbl),    intent(in) :: r_in(nd)
      complex(kind=dbl), intent(in) :: data_in(:,:)
    end subroutine out_spectra_3d_sub
    
    module subroutine save_spectra_flux_sub()
    end subroutine save_spectra_flux_sub
    
    module subroutine save_spectra_temp_sub()
    end subroutine save_spectra_temp_sub
    
    module subroutine save_spectra_velc_sub()
    end subroutine save_spectra_velc_sub
    
    module subroutine save_snpsht_flux_sub()
    end subroutine save_snpsht_flux_sub
    
    module subroutine save_snpsht_temp_sub()
    end subroutine save_snpsht_temp_sub
    
    module subroutine save_snpsht_velc_sub()
    end subroutine save_snpsht_velc_sub
  end interface
  
end module output_mod