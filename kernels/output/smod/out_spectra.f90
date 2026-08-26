submodule (output_mod) out_spectra
  implicit none; contains
  
  module procedure out_spectra_2d_sub
    integer :: ijm
    
    open(unit=1, file=opt, status='new', action='write')
    
    do ijm = 1, size(spectra_in)
      write(1,*) ijm, spectra_in(ijm)
    end do
    
    close(1)
    
  end procedure out_spectra_2d_sub
  
  module procedure out_spectra_3d_sub
    integer :: ir
    
    open(unit=1, file=opt, status='new', action='write')
    
    do ir = 1, size(r_in)
      write(1,*) r_in(ir), data_in(:,ir)
    end do
    
    close(1)
    
  end procedure out_spectra_3d_sub
  
end submodule out_spectra