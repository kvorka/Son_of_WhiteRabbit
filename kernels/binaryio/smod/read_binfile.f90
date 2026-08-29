submodule (binaryio) read_binfile
  implicit none; contains
  
  module procedure read_2d_binfile_sub
    
    open( unit   = filenum,       &
          file   = filepath,      &
          form   = 'unformatted', &
          access = 'stream',      &
          status = 'old',         &
          action = 'read'         )
      
      read(filenum) arr(1:narr)
      
    close( filenum )
    
  end procedure read_2d_binfile_sub
  
  module procedure read_3d_binfile_sub
    
    open( unit   = filenum,       &
          file   = filepath,      &
          form   = 'unformatted', &
          access = 'stream',      &
          status = 'old',         &
          action = 'read'         )
      
      read(filenum) rr(1:nrr), arr(1:narr)
      
    close( filenum )
    
  end procedure read_3d_binfile_sub
  
end submodule read_binfile