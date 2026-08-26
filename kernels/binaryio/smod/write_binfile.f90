submodule (binaryio) write_binfile
  implicit none; contains
  
  module procedure write_2d_binfile_sub
    
    open( unit=filenum, file=filepath, form='unformatted', access='stream', status='new', action='write')
      write(filenum) arr(1:narr)
    close(filenum)
    
  end procedure write_2d_binfile_sub
  
  module procedure write_3d_binfile_sub
    
    open( unit=filenum, file=filepath, form='unformatted', access='stream', status='new', action='write')
      write(filenum) rr(1:nrr), arr(1:narr)
    close(filenum)
    
  end procedure write_3d_binfile_sub
  
end submodule write_binfile